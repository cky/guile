/* Wrapper to implement ANSI C's memmove using BSD's bcopy. */
/* This function is in the public domain.  --Per Bothner. */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */

#include <sys/types.h>

#ifdef __STDC__
#define PTR void *
#define CPTR const void *
PTR memmove (PTR, CPTR, size_t);
#else
#define PTR char *
#define CPTR char *
PTR memmove ();
#endif

PTR
memmove (PTR s1, CPTR s2, size_t n)
{
  bcopy (s2, s1, n);
  return s1;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
