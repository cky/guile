/* guile-snarf.c --- extract declarations from Guile source code
   Jim Blandy <jimb@red-bean.com> --- September 1999

	  Copyright (C) 1999 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
   Boston, MA 02111-1307 USA
  
   As a special exception, the Free Software Foundation gives permission
   for additional uses of the text contained in its release of GUILE.
  
   The exception is that, if you link the GUILE library with other files
   to produce an executable, this does not by itself cause the
   resulting executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of
   linking the GUILE library code into it.
  
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.
  
   This exception applies only to the code released by the
   Free Software Foundation under the name GUILE.  If you copy
   code from other Free Software Foundation releases into a copy of
   GUILE, as the General Public License permits, the exception does
   not apply to the code that you add in this way.  To avoid misleading
   anyone as to the status of such modified files, you must delete
   this exception notice from them.
  
   If you write modifications of your own for GUILE, it is your choice
   whether to permit this exception to apply to your modifications.
   If you do not wish that, delete this exception notice.  */


/* The problem:

   It's really nice to be able to create Scheme procedures by simply
   adding a declaration above the C function's definition.  And you
   can use similar facilities for variables, symbols, etc.
 
   Using the C preprocessor to extract declarations from the C code is
   a clever idea, but it's not very robust.  Finding the C
   preprocessor, and getting it to run without errors, is too prone to
   interference from outside influences.  We end up having to pass all
   the -I flags and compiler flags to guile-snarf, whose job doesn't
   have anything to do with them.

   Thus, we redefine snarfing as a process which operates on a single
   text file, and ignores #inclusion.

   The general strategy:

   Scan the source file in just enough detail to correctly recognize C
   identifiers or reserved words.  This means watching for comments,
   string and character literals, etc.

   When we see an identifier from our selected list, parse an argument
   list after it according to the C preprocessor's rules, and then
   pass it to an appropriate function to handle.

   Still to do:

   - Test by comparing output with old shell script.
   - Add a test for this to Makefile.am.
   - Change build process to actually use this.
   - Make sure that errors cause .x file to be deleted.  `make' will do
     this if the process exits with a non-zero status, won't it?  If not,
     delete it ourselves.
   - Could we generate both the declarations and the initializations?
   - Can we simplify or improve the argument lists for some of the
     snarfing keywords, now that we can parse them any way we like?
   - When a file declares a *lot* of something, would it save space to
     emit a table and a loop, instead of a zillion function calls?  */


#include <stddef.h>
#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <stdarg.h>

#include "versiondat.h"


/* Utility functions.  */

char *program_name;

static void *
check_ptr (void *p)
{
  if (! p)
    {
      fprintf (stderr, "%s: out of memory\n", program_name);
      exit (2);
    }
  return p;
}

static void *
xmalloc (size_t size)
{
  return check_ptr (malloc (size));
}

static void *
xrealloc (void *ptr, size_t size)
{
  return check_ptr (realloc (ptr, size));
}

static void
system_error (char *message, char *filename)
{
  fprintf (stderr, "%s: ", program_name);
  if (filename) fprintf (stderr, "%s: ", filename);
  perror (message);

  exit (1);
}


/* Character table.  */

struct chartable {

  enum action {

    /* Just skip this character.  */
    act_normal,
    
    /* This character is whitespace.  */
    act_space,

    /* This character begins and ends a string/char literal.  */
    act_literal,

    /* This character is '/'.  If followed by a '*', begin a block
       comment.  If followed by another '/', begin a line comment.  */
    act_comment_start,

    /* This character increments the nesting level, and must be
       matched by the character in `follow'.  */
    act_open,

    /* This character decrements the nesting level, if it matches properly.  */
    act_close,

    /* This character could appear anywhere in an identifier.  */
    act_id_start,
    
    /* This character could appear anywhere in an identifier, except
       as the first character.  */
    act_id_follow,

    /* This character separates arguments in a keyword argument list,
       if it occurs at the outermost nesting level.  */
    act_arg_separator

  } action;

  int follow;
};

struct chartable chartable[256];
#define CHARTABLE(i) (chartable[(unsigned) (i)])


static void
match (char open, char close)
{
  CHARTABLE (open).action = act_open;
  CHARTABLE (open).follow = close;
  CHARTABLE (close).action = act_close;
}

static void
init_chartable ()
{
  int i;
  char *c;

  for (i = 0; i < 256; i++)
    CHARTABLE (i).action = act_normal;

  for (c = " \t\n\f"; *c; c++)
    CHARTABLE (*c).action = act_space;

  CHARTABLE ('"' ).action = act_literal;
  CHARTABLE ('\'').action = act_literal;

  CHARTABLE ('/').action = act_comment_start;

  match ('(', ')');
  match ('[', ']');
  match ('{', '}');

  CHARTABLE (',').action = act_arg_separator;

  for (i = 'a'; i <= 'z'; i++)
    CHARTABLE (i).action = act_id_start;
  for (i = 'A'; i <= 'Z'; i++)
    CHARTABLE (i).action = act_id_start;
  CHARTABLE ('_').action = act_id_start;

  for (i = '0'; i <= '9'; i++)
    CHARTABLE (i).action = act_id_follow;
}



/* Reading text while tracking the current line number.  */

/* The input file and the line number are global, because it's too
   much of a pain to pass them around everywhere as arguments.  Sorry.  */

FILE *in;			/* The current input file. */
char *in_name;			/* Its filename. */
int in_line;			/* The current line number in that file. */

FILE *out;			/* The current output file. */

static int
source_getc ()
{
  int c = getc (in);
  if (c == '\n')
    in_line++;
  return c;
}

static void
source_ungetc (int c)
{
  if (c == '\n')
    in_line--;
  ungetc (c, in);
}

static void
syntax_error (char *message, ...)
{
  va_list args;
  va_start (args, message);
  
  fprintf (stderr, "%s:%d: ", in_name, in_line);
  vfprintf (stderr, message, args);
  va_end (args);
  exit (1);
}


/* Skipping comments.  */

/* If we've read the first character of a comment start (that is, the
   character '/'), then check the second character.  If it's '*',
   we're in a block comment; read to the end of it.  If it's '/',
   we're starting a line comment; skip to the end of the line.
   Otherwise, put the second character back on the input stream.

   Return non-zero if this was a comment, zero otherwise.  */
static int
maybe_skip_comment ()
{
  int d = source_getc ();
    
  if (d == '*')
    {
      int prev;
      int start_line = in_line;

      /* Make sure the asterisk of the comment start doesn't
	 get mistaken for the asterisk of the comment end.  */
      d = 0;

      do
	{
	  prev = d;
	  d = source_getc ();
	}
      while (d != EOF && ! (prev == '*' && d == '/'));

      if (d == EOF)
	{
	  in_line = start_line;
	  syntax_error ("unterminated comment");
	}

      return 1;
    }
  else if (d == '/')
    {
      do
	d = source_getc ();
      while (d != '\n' && d != EOF);
      
      return 1;
    }
  else
    {
      source_ungetc (d);
      return 0;
    }
}


/* Parsing argument lists for keywords.  */

static int
arglist_getc (char *keyword)
{
  int c = source_getc ();
  if (c == EOF)
    syntax_error ("file ends in midst of arguments to keyword `%s'\n",
		  keyword);
  return c;
}

struct buffer {
  int size, len;
  char *text;
};

static void
init_buffer (struct buffer *b)
{
  b->size = 1;
  b->text = (char *) xmalloc (b->size);
  b->len = 0;
}

static void
add_char (struct buffer *b, int c)
{
  if (b->len >= b->size)
    {
      b->size *= 2;
      b->text = (char *) xrealloc (b->text, b->size);
    }

  b->text[b->len++] = c;
}

/* Parse a string or character literal, appending its text to b.  */
static void
read_literal (char *keyword, struct buffer *b, int start)
{
  add_char (b, start);
  
  for (;;)
    {
      int c = arglist_getc (keyword);
      add_char (b, c);
      if (c == '\\')
	add_char (b, arglist_getc (keyword));
      else if (c == start)
	break;
    }
}


/* Parse the argument list following a keyword, just as the
   C preprocessor would parse the arguments to a macro invocation.
   Return the arguments as an array of strings, terminated by a null
   pointer.

   If there's an error parsing the arguments, print an error message
   using KEYWORD as the keyword name, and exit. 

   Caller must free the array and each string; see the free_args
   function.  */
static char **
parse_args (char *keyword)
{
  /* Our read-ahead character.  */
  int c = arglist_getc (keyword);

  /* String of closing parens we are expecting, outermost first.  */ 
  struct buffer stack;

  /* Current list of arguments.  */
  int arglist_size = 1;
  char **arglist = (char **) xmalloc (arglist_size * sizeof (*arglist));
  int arglist_len = 0;

  init_buffer (&stack);

  /* Skip any initial whitespace.  */
  while (CHARTABLE (c).action == act_space)
    c = arglist_getc (keyword);

  /* Require an opening paren.  */
  if (c != '(')
    syntax_error ("arguments to snarfing keyword `%s' missing", keyword);
  add_char (&stack, ')');
  c = arglist_getc (keyword);
		  
  /* Read arguments, separated by commas outside of any (), {}, or []
     pairs.  */
  while (stack.len > 0)
    {
      /* start a new argument.  */
      struct buffer arg;
      int arg_incomplete = 1;

      init_buffer (&arg);

      /* Skip whitespace.  */
      while (CHARTABLE (c).action == act_space)
	c = arglist_getc (keyword);
      
      do
	{
	  switch (CHARTABLE (c).action)
	    {
	    case act_normal:
	    case act_space:
	    case act_id_start:
	    case act_id_follow:
	      add_char (&arg, c);
	      break;

	    case act_literal:
	      read_literal (keyword, &arg, c);
	      break;

	    case act_comment_start:
	      if (maybe_skip_comment ())
		add_char (&arg, ' ');
	      else
		add_char (&arg, c);
	      break;

	    case act_open:
	      add_char (&stack, CHARTABLE (c).follow);
	      add_char (&arg, c);
	      break;

	    case act_close:
	      if (stack.len == 0)
		/* We should have finished argument list parsing
		   when the stack became empty.  */
		abort ();
	      if (c != stack.text[stack.len - 1])
		syntax_error ("mismatched parenthesis: '%c' and '%c'",
			      stack.text[stack.len - 1], c);
	      stack.len--;
	      /* Closing parens are part of the argument, except for the
		 outermost closing paren.  */
	      if (stack.len > 0)
		add_char (&arg, c);
	      else
		arg_incomplete = 0;
	      break;

	    case act_arg_separator:
	      /* Commas are part of the argument, unless they occur at
		 the top level within the argument list.  */
	      if (stack.len == 1)
		arg_incomplete = 0;
	      else
		add_char (&arg, c);
	      break;

	    default:
	      abort ();
	    }

	  c = arglist_getc (keyword);
	}
      while (arg_incomplete);

      /* Add this argument to the list.  */
      add_char (&arg, '\0');
      if (arglist_len >= arglist_size)
	{
	  arglist_size *= 2;
	  arglist = (char **) xrealloc (arglist,
					arglist_size * sizeof (*arglist));
	}
      arglist[arglist_len++] = arg.text;
    }

  /* Null-terminate the argument list.  */
  arglist = (char **) xrealloc (arglist,
				(arglist_len + 1) * sizeof (*arglist));
  arglist[arglist_len] = 0;
  return arglist;
}

static int
count_args (char **args)
{
  int i;

  for (i = 0; args[i]; i++)
    ;

  return i;
}

static void
free_args (char **args)
{
  int i;

  for (i = 0; args[i]; i++)
    free (args[i]);
  free (args);
}


/* Individual routines for processing keywords.  */

/* Flags these routines might use to select details of their behavior.  */
enum keyword_flags {
  kw_gsubr   = 0x01,		/* create a gsubr */
  kw_generic = 0x02,		/* create a generic function */
  kw_global  = 0x04,		/* global declaration, not static */
  kw_init    = 0x08,		/* initialized value */
  kw_long    = 0x10,		/* initialize it with a long */
  kw_keyword = 0x20		/* make a keyword, not a symbol */
};

static void
check_arg_count (char *keyword, char **args, int expected)
{
  int actual = count_args (args);
  if (actual != expected)
    syntax_error ("keyword `%s' expects %d args, but got %d",
		  keyword, expected, actual);
}

static void
proc_keyword (char *keyword, char **args, int flags)
{
  int expected_args;

  if (flags & kw_gsubr)
    expected_args = 6;
  else
    expected_args = 4;

  if (flags & kw_generic)
    expected_args++;

  check_arg_count (keyword, args, expected_args);

  /* Print some nice indentation.  */
  fputs ("  ", out);

  /* Print out the function name.  */
  fprintf (out, "scm_make_%s%s",
	   (flags & kw_gsubr) ? "gsubr" : "subr",
	   (flags & kw_generic) ? "_with_generic" : "");

  /* And the arguments.  */
  fputs (" (", out);
  if (flags & kw_gsubr)
    fprintf (out, "%s, %s, %s, %s, (SCM (*) (...)) %s",
	     args[0], args[2], args[3], args[4], args[5]);
  else
    fprintf (out, "%s, %s, (SCM (*) (...)) %s",
	     args[0], args[2], args[3]);

  /* The generic versions have an extra argument at the end,
     which is a pointer to a generic function variable.  */
  if (flags & kw_generic)
    fprintf (out, ", &%s", (flags & kw_gsubr) ? args[6] : args[4]);

  fputs (");\n", out);
}

static void
syntax_keyword (char *keyword, char **args, int flags)
{
  check_arg_count (keyword, args, 4);

  fprintf (out, "  scm_make_synt (%s, %s, %s);\n",
	   args[0], args[2], args[3]);
}

static void
symbol_keyword (char *keyword, char **args, int flags)
{
  check_arg_count (keyword, args, 2);

  fprintf (out, "  %s = scm_permanent_object (", args[0]);
  if (flags & kw_keyword)
    fprintf (out, "scm_c_make_keyword (%s)", args[1]);
  else
    fprintf (out, "SCM_CAR (scm_intern0 (%s))", args[1]);
  fputs (");\n", out);
}

static void
vcell_keyword (char *keyword, char **args, int flags)
{
  check_arg_count (keyword, args, (flags & kw_init) ? 3 : 2);

  fprintf (out, "  %s = scm_permanent_object (scm_intern0 (%s));\n",
	   args[0], args[1]);
  fprintf (out, "  SCM_SETCDR (%s, ", args[0]);

  if (flags & kw_long)
    fprintf (out, "scm_long2num (%s)", args[2]);
  else if (flags & kw_init)
    fputs (args[2], out);
  else
    fputs ("SCM_BOOL_F", out);

  fputs (");\n", out);
}


/* The keyword table.  */

struct keyword {
  char *name;
  void (*func) (char *keyword, char **args, int flags);
  int flags;
};

/* The maximum length of any keyword, in bytes.  */
#define MAX_KEYWORD_LEN (20)

struct keyword keywords[] =
{
  { "SCM_PROC",   	     proc_keyword,   kw_gsubr },
  { "SCM_GPROC",  	     proc_keyword,   kw_gsubr | kw_generic },
  { "SCM_PROC1",  	     proc_keyword,   0 },
  { "SCM_GPROC1", 	     proc_keyword,   kw_generic },

  { "SCM_SYNTAX",            syntax_keyword, 0 },

  { "SCM_SYMBOL",            symbol_keyword, 0 },
  { "SCM_GLOBAL_SYMBOL",     symbol_keyword, kw_global },
  { "SCM_KEYWORD",           symbol_keyword, kw_keyword },
  { "SCM_GLOBAL_KEYWORD",    symbol_keyword, kw_keyword | kw_global },

  { "SCM_VCELL",             vcell_keyword,  0 },
  { "SCM_GLOBAL_VCELL",      vcell_keyword,  kw_global },
  { "SCM_VCELL_INIT",        vcell_keyword,  kw_init },
  { "SCM_GLOBAL_VCELL_INIT", vcell_keyword,  kw_init | kw_global },
  { "SCM_CONST_LONG",        vcell_keyword,  kw_init | kw_long },

  { 0, 0, 0 }
  
};

/* The bigger this is, the less likely a random identifier is
   to clash with the user's program.  */
#define KEYWORD_HASH_SIZE (1009)
struct keyword *keyword_hash[KEYWORD_HASH_SIZE];

static unsigned long
hash (char *text)
{
  long h = 0;

  while (*text)
    {
      unsigned char c = (unsigned char) *text++;
      h = (h << 4) + c + (c << 9) + (h >> 24) + 32;
    }
  
  return h;
}

static struct keyword *
is_keyword (char *name)
{
  unsigned long h = hash (name) % KEYWORD_HASH_SIZE;
  if (keyword_hash[h])
    {
      if (strcmp (name, keyword_hash[h]->name))
	{
	  /* Just for testing.  */
	  fprintf (stderr, "%s: keyword/user id hash collision: %s and %s\n",
		   program_name, keyword_hash[h]->name, name);
	  return 0;
	}

      return keyword_hash[h];
    }

  return 0;
}

static void
process_keyword (struct keyword *k)
{
  char **args = parse_args (k->name);
  k->func (k->name, args, k->flags);
  free_args (args);
}

static void
init_keyword_hash_table ()
{
  int i;

  for (i = 0; keywords[i].func; i++)
    {
      unsigned long h = hash (keywords[i].name) % KEYWORD_HASH_SIZE;
      if (keyword_hash[h])
	{
	  fprintf (stderr, "%s: keyword hash collision: %s and %s\n",
		   program_name, keyword_hash[h]->name, keywords[i].name);
	  exit (2);
	}
      keyword_hash[h] = &keywords[i];
    }
}


/* Scanning a file of C code.  */

/* Skip a string or character literal that started with the character
   FOLLOW.  */
static void
skip_literal (int follow)
{
  int start_line = in_line;

  for (;;)
    {
      int c = source_getc ();

      if (c == EOF)
	{
	  in_line = start_line;
	  syntax_error ("unterminated character or string literal");
	}
      else if (c == '\\')
	source_getc ();
      else if (c == follow)
	break;
    }
}

static void
read_id (char *buf, size_t size)
{
  int i = 0;

  for (;;)
    {
      int c = source_getc ();
      int action;

      if (c == EOF)
	break;

      action = CHARTABLE (c).action;
      if (action == act_id_start || action == act_id_follow)
	{
	  if (i < size)
	    buf[i] = c;
	  i++;
	}
      else
	{
	  source_ungetc (c);
	  break;
	}
    }

  /* It doesn't matter that we truncate the keyword to SIZE-1
     characters, since the buffer is large enough to hold any valid
     keyword.  */
  buf[(i >= size) ? size - 1 : i] = '\0';
}

static void
process_stream ()
{
  int c;

  for (;;)
    {
      c = source_getc ();
      if (c == EOF)
	break;

      switch (CHARTABLE (c).action)
	{
	case act_normal:
	case act_space:
	case act_open:
	case act_close:
	case act_id_follow:
	case act_arg_separator:
	  break;

	case act_literal:
	  skip_literal (c);
	  break;
	  
	case act_comment_start:
	  maybe_skip_comment ();
	  break;

	case act_id_start:
	  {
	    char buf[MAX_KEYWORD_LEN + 1];
	    struct keyword *k;

	    source_ungetc (c);
	    read_id (buf, sizeof (buf));
	    k = is_keyword (buf);
	    if (k)
	      process_keyword (k);
	  }
	  break;

	default:
	  abort ();
	}
    }
}

static void
process_file (char *inname, char *outname)
{
  in = fopen (inname, "r");
  if (! in)
    system_error ("error opening input file", inname);
  in_line = 1;
  in_name = inname;
  
  out = fopen (outname, "w");
  if (! out)
    system_error ("error opening output file", outname);

  process_stream (in, out);

  /* I'm told the AIX C preprocessor doesn't like to #include empty files.  */
  putc ('\n', out);
  
  if (ferror (in))
    system_error ("error reading input file", inname);
  if (ferror (out))
    system_error ("error writing output file", outname);

  if (fclose (in) == EOF)
    system_error ("error closing input file", inname);
  if (fclose (out) == EOF)
    system_error ("error closing output file", outname);
}


/* The main function.  */

static void
version ()
{
  fprintf (stderr, "guile-snarf (Guile) %s\n", GUILE_VERSION);
  exit (0);
}

static void
usage ()
{
  fprintf (stderr, "Usage: %s [-o OUTFILE] INFILE\n", program_name);
}

static void
help ()
{
  fprintf (stderr, "guile-snarf -- extract declarations from Guile C code.\n");
  usage ();
  fprintf (stderr, "\
This program makes it easier to write C code which defines Scheme\n\
functions and variables and uses Scheme symbols in Guile.  Instead of\n\
writing code yourself to build the Scheme objects when your code is\n\
initialized, you can write brief declarations above your C functions\n\
indicating how they should be called from Scheme.  Then, if you run\n\
this program on your source file, it will write code for you that\n\
defines functions, builds Scheme objects, and so on.  You can then\n\
#include this program's output into your initialization function.\n\
\n\
`INFILE' is the file from which we should extract declarations and\n\
generate initialization code.\n\
\n\
`-o OUTFILE' specifies the file where we should place the output.  If\n\
omitted, it defaults to `INFILE', with its extension changed to `.x'.\n");
  exit (0);
}

static void
bad_argument ()
{
  usage ();
  fprintf (stderr, "Type `%s --help' for more information.\n", program_name);
  exit (1);
}

int
main (int argc, char **argv)
{
  char *infile, *outfile;

  program_name = strrchr (argv[0], '/');
  if (! program_name)
    program_name = argv[0];
  else
    program_name++;

  infile = 0;
  outfile = 0;
  argc--, argv++;
  while (argc > 0)
    {
      if (! strcmp (argv[0], "--version"))
	version ();
      else if (! strcmp (argv[0], "--help"))
	help ();
      else if (! strcmp (argv[0], "-o"))
	outfile = *++argv, argc--;
      else if (argv[0][0] == '-')
	{
	  fprintf (stderr, "%s: unrecognized switch: `%s'\n",
		   program_name, argv[0]);
	  bad_argument ();
	}
      else if (infile)
	{
	  fprintf (stderr, "%s: more than input file given: `%s' and `%s'\n",
		   program_name, infile, argv[0]);
	  bad_argument ();
	}
      else
	infile = argv[0];

      argc--, argv++;
    }

  if (! infile)
    {
      fprintf (stderr, "%s: no input file given\n", program_name);
      bad_argument ();
    }

  if (! outfile)
    {
      /* Choose a default output file name.  This should be the input
         file name, with the extension changed to `.x'.  */
      char *ext = strrchr (infile, '.');

      /* Make sure we haven't mistaken a dot in an earlier path
	 component for the start of the extension.  */
      if (ext && strchr (ext, '/'))
	ext = 0;

      if (ext)
	{
	  outfile = (char *) xmalloc (ext - infile + 3);
	  strcpy (outfile, infile);
	  strcpy (outfile + (ext - infile), ".x");
	}
      else
	{
	  outfile = (char *) xmalloc (strlen (infile) + 3);
	  strcpy (outfile, infile);
	  strcat (outfile, ".x");
	}
    }

  init_chartable ();
  init_keyword_hash_table ();
  process_file (infile, outfile);

  return 0;
}
