/* need copyright assignment from Daniel Risacher */

#include "_scm.h"
#ifdef HAVE_LIBREADLINE
#include <libguile.h>
#include <gh.h>
#include <readline/readline.h>
#include <readline/history.h>

SCM_PROC (s_readline, "readline", 0, 1, 0, scm_readline);
SCM
scm_readline (SCM txt)
{
  SCM ret;
  char* s;
  char* prompt;

  if (! SCM_UNBNDP (txt))
    {
      SCM_ASSERT ((SCM_NIMP(txt) && SCM_STRINGP(txt)), txt, SCM_ARG1,
		  s_readline);
      SCM_COERCE_SUBSTR (txt);
    }

  SCM_DEFER_INTS;
  prompt = SCM_UNBNDP (txt) ? "" : SCM_CHARS (txt);

  s = readline(prompt);
  if (s)
    ret = gh_str02scm(s);
  else 
    ret = SCM_EOF_VAL;

  free (s);
  SCM_ALLOW_INTS;

  return ret;
}

SCM_PROC (s_add_history, "add-history", 1, 0, 0, scm_add_history);
SCM
scm_add_history (SCM txt)
{
  char* s;
  SCM_ASSERT ((SCM_NIMP(txt) && SCM_STRINGP(txt)), txt, SCM_ARG1,
	      s_add_history);
  SCM_COERCE_SUBSTR (txt);

  SCM_DEFER_INTS;
  s = SCM_CHARS(txt);
  add_history(s);
  SCM_ALLOW_INTS;

  return SCM_UNSPECIFIED;
}

void
scm_init_readline ()
{
#include "readline.x"
  scm_add_feature ("readline");
}

#endif 
