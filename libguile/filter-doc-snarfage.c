#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

static void init_state_machine (void);

static void process (void);

static void check_end_conditions (void);

int
main (int argc, char *argv[])
{
  init_state_machine ();
  process ();
  check_end_conditions ();
    
  return EXIT_SUCCESS;
}

typedef enum state_t
  {
    SKIP,
    SKIP_COOKIE,
        
    MULTILINE,

    MULTILINE_COOKIE,
    STRINGS,

    SINGLELINE,

  } state_t;

state_t state = SKIP;

static void die (const char *msg);
static void process_strings (void);
static void process_single_line (void);

void
process ()
{
  int want_cookie = 0;
  int ch;
  
  while ((ch = getc (stdin)) != EOF) {
    char c = (char)ch;

    switch (state) {
    case SKIP:
      if (c == '^') {
        if (want_cookie) {
          state = SKIP_COOKIE;
          want_cookie = 0;
        } else
          want_cookie = 1;
      } else if (c != ' ')
        want_cookie = 0;
      break;
    case SKIP_COOKIE:
      switch (c) {
      case '[':
        fputs ("(doc-check\n", stdout);
        state = SINGLELINE;
        break;
      case '{':
        fputs ("(doc-block (\n", stdout);
        state = MULTILINE;
        break;
      default:
        die ("bad snarf cookie");
        break;
      }
      break;
    case MULTILINE:
      if (c == '^') {
        if (want_cookie) {
          fputs ("\n)\n(\n", stdout);
          state = MULTILINE_COOKIE;
          want_cookie = 0;
        } else
          want_cookie = 1;
      } else {
        if (c != ' ')
          want_cookie = 0;
        putc (c, stdout);
      }
      break;
    case MULTILINE_COOKIE:
      switch (c) {
      case '(':
        state = STRINGS;
        break;
      case ' ':
        state = MULTILINE;
        break;
      case '}':
        fputs ("))\n", stdout);
        state = SKIP;
        break;
      default:
        die ("bad snarf cookie in multiline context");
        break;
      }
      break;
    case STRINGS:
      process_strings ();
      state = MULTILINE;
      break;
    case SINGLELINE:
      process_single_line ();
      fputs ("\n)\n", stdout);
      state = SKIP;
      break;
    default:
      abort ();
      break;
    }
  }
}

void
init_state_machine ()
{}

void
die (const char *msg)
{
  fprintf (stderr, "%s\n", msg);
  exit (EXIT_FAILURE);
}

void
check_end_conditions ()
{
  if (state != SKIP)
    die ("something is unterminated");
}

typedef enum str_state_t
  {
    STR_SKIP,
    STR_INSIDE,
    STR_HAD_ESCAPE,
    STR_EXIT
  } str_state_t;

void
process_strings ()
{
  /* read well-formed strings up to a ')', and break them up in the
     process if they are too long */
  int count = 0;
  int ch;
  str_state_t state = STR_SKIP;

  fputs ("docstring\n", stdout);

#define PUTC(c) putc (c, stdout); if (++count >= 512) { fputs ("\"\nstring \"", stdout); count = 0; }

  while (!(((ch = getc (stdin)) == EOF)
           || (state == STR_EXIT))) {
    char c = (char) ch;

    switch (state) {
    case STR_SKIP:
      switch (c) {
      case '"':
        fputs ("\nstring ", stdout);
        count = 0;
        PUTC (c);
        state = STR_INSIDE;
        break;
      case ')':
        state = STR_EXIT;
        break;
      default:
        if (!isspace (c))
          die ("stray stuff where should be only strings");
        break;
      }
      break;
    case STR_INSIDE:
      switch (c) {
      case '\\':
        putc (c, stdout);
        ++count;
        state = STR_HAD_ESCAPE;
        break;
      case '"':
        putc (c, stdout);
        state = STR_SKIP;
        break;
      default:
        PUTC (c);
        break;
      }
      break;
    case STR_HAD_ESCAPE:
      PUTC (c);
      state = STR_INSIDE;
      break;
    default:
      abort ();
      break;
    }
  }

  if (state != STR_EXIT)
    die ("docstrings don't terminate");
}

void
process_single_line ()
{
  /* read up to a ']' */
  int ch;
  while (!(((ch = getc (stdin)) == EOF)
           || ((char) ch == ']'))) {
    char c = (char) ch;

    putc (c, stdout);
  }

  if ((char) ch != ']')
    die ("bad checking snarfage");
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
