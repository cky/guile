/* this file is #include'd (many times) by numbers.c */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#ifndef UNSIGNED_ITYPE
# if UNSIGNED
#   define UNSIGNED_ITYPE ITYPE
# else
#   define UNSIGNED_ITYPE unsigned ITYPE
# endif
#endif

#define UNSIGNED_ITYPE_MAX (~((UNSIGNED_ITYPE)0))

#ifndef SIZEOF_ITYPE
#error SIZEOF_ITYPE must be defined.
#endif

#if UNSIGNED
#  if SIZEOF_ITYPE == SIZEOF_UNSIGNED_SHORT
#    define BIGMPZ_FITSP mpz_fits_ushort_p
#  elif SIZEOF_ITYPE == SIZEOF_UNSIGNED_INT
#    define BIGMPZ_FITSP mpz_fits_uint_p
#  elif SIZEOF_ITYPE == SIZEOF_UNSIGNED_LONG
#    define BIGMPZ_FITSP mpz_fits_ulong_p
#  else
#    define BIGMPZ_FITSP ((int (*)(void *)) 0)
#  endif /* sizeof checks */
#else
/* UNSIGNED is not defined */
#  if SIZEOF_ITYPE == SIZEOF_SHORT
#    define BIGMPZ_FITSP mpz_fits_sshort_p
#  elif SIZEOF_ITYPE == SIZEOF_INT
#    define BIGMPZ_FITSP mpz_fits_sint_p
#  elif SIZEOF_ITYPE == SIZEOF_LONG
#    define BIGMPZ_FITSP mpz_fits_slong_p
#  else
#    define BIGMPZ_FITSP ((int (*)(void *)) 0)
#  endif /* sizeof checks */
#endif /* UNSIGNED check */

/* We rely heavily on the compiler's optimizer to remove branches that
   have constant value guards. */

ITYPE
NUM2INTEGRAL (SCM num, unsigned long int pos, const char *s_caller)
{
  if (SCM_INUMP (num))
    { /* immediate */    
      scm_t_signed_bits n = SCM_INUM (num);
      
      if (UNSIGNED && (n < 0))
        scm_out_of_range (s_caller, num);
      
      if (SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS)
        /* the target type is large enough to hold any possible inum */
        return (ITYPE) n;
      else
        {
          /* an inum can be out of range, so check */
          if (UNSIGNED) /* n is known to be >= 0 */
            {
              if (((scm_t_bits) n) > UNSIGNED_ITYPE_MAX)
                scm_out_of_range (s_caller, num);
            }
          else if (((ITYPE) n) != n)
            scm_out_of_range (s_caller, num);
          return (ITYPE) n;
        }
    }
  else if (SCM_BIGP (num))
    { /* bignum */
      if (SIZEOF_ITYPE < SIZEOF_SCM_T_BITS)
        scm_out_of_range (s_caller, num);
      else
        {
          /* make sure the result will fit */
          if (BIGMPZ_FITSP)
            {
              int fits_p = BIGMPZ_FITSP (SCM_I_BIG_MPZ (num));
              scm_remember_upto_here_1 (num);
              if (!fits_p)
                scm_out_of_range (s_caller, num);
            }
          else
            {
              size_t numbits;
              if (UNSIGNED)
                {
                  int sgn = mpz_sgn (SCM_I_BIG_MPZ (num));
                  scm_remember_upto_here_1 (num);
                  if (sgn < 0)
                    scm_out_of_range (s_caller, num);
                }
              
              numbits = mpz_sizeinbase (SCM_I_BIG_MPZ (num), 2);
              scm_remember_upto_here_1 (num);
              if (numbits > (sizeof (ITYPE) * SCM_CHAR_BIT))
                scm_out_of_range (s_caller, num);
            }
          
          if (UNSIGNED && (SIZEOF_ITYPE <= SIZEOF_UNSIGNED_LONG))
            {
              ITYPE result = (ITYPE) mpz_get_ui (SCM_I_BIG_MPZ (num));
              scm_remember_upto_here_1 (num);
              return result;
            }
          else if ((!UNSIGNED) && (SIZEOF_ITYPE <= SIZEOF_LONG))
            {
              ITYPE result = (ITYPE) mpz_get_si (SCM_I_BIG_MPZ (num));
              scm_remember_upto_here_1 (num);
              return result;
            }
          else
            {
              int sgn = mpz_sgn (SCM_I_BIG_MPZ (num));
              ITYPE result = 0;
              size_t count;

              mpz_export (&result,
                          &count,
#ifdef WORDS_BIGENDIAN
                          1,
#else
                          -1,
#endif
                          SIZEOF_ITYPE,
                          0,
                          0,
                          SCM_I_BIG_MPZ (num));
              /* mpz_export doesn't handle sign */
              if (sgn < 0) result = - result;
              scm_remember_upto_here_1 (num);              
              return result;
            }
        }
    }
  else
    scm_wrong_type_arg (s_caller, pos, num);
}


SCM
INTEGRAL2NUM (ITYPE n)
{
  /* If we know the size of the type, determine at compile time
     whether we need to perform the FIXABLE test or not.  This is not
     done to get more optimal code out of the compiler (it can figure
     this out on its own already), but to avoid a spurious warning.
     If we don't know the size, assume that the test must be done.
  */

  /* have to use #if here rather than if because of gcc warnings about
     limited range */
#if SIZEOF_ITYPE < SIZEOF_SCM_T_BITS
    return SCM_MAKINUM ((scm_t_signed_bits) n);
#else /* not SIZEOF_ITYPE < SIZEOF_SCM_T_BITS */ 
    if (UNSIGNED)
      {
        if (SCM_POSFIXABLE (n))
          return SCM_MAKINUM ((scm_t_signed_bits) n);
      }
    else
      {
        if (SCM_FIXABLE (n))
          return SCM_MAKINUM ((scm_t_signed_bits) n);
      }
      return INTEGRAL2BIG (n);
#endif /* not SIZEOF_ITYPE < SIZEOF_SCM_T_BITS */ 
}

SCM
INTEGRAL2BIG (ITYPE n)
{
  if (UNSIGNED && (SIZEOF_ITYPE <= SIZEOF_LONG))
    {
      SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
      mpz_init_set_ui (SCM_I_BIG_MPZ (z), n);
      return z;
    }
  else if ((!UNSIGNED) && (SIZEOF_ITYPE <= SIZEOF_UNSIGNED_LONG))
    {
      SCM z = scm_double_cell (scm_tc16_big, 0, 0, 0);
      mpz_init_set_si (SCM_I_BIG_MPZ (z), n);
      return z;
    }
  else 
    {
      int neg_input = 0;
      SCM result = scm_i_mkbig (); 

      /* mpz_import doesn't handle sign -- have to use #if here rather
         than if b/c gcc warnings for ushort, etc. */
#if !UNSIGNED
      if (n < 0)
	{
	  neg_input = 1;
	  n = - n;
	}
#endif

      mpz_import (SCM_I_BIG_MPZ (result),
                  1,
#ifdef WORDS_BIGENDIAN
                  1,
#else
                  -1,
#endif
                  SIZEOF_ITYPE,
                  0,
                  0,
                  &n);

      /* mpz_import doesn't handle sign */
      if (!UNSIGNED)
        {
          if (neg_input)
            mpz_neg (SCM_I_BIG_MPZ (result), SCM_I_BIG_MPZ (result));
        }
      return result;
    }
}

/* clean up */
#undef INTEGRAL2NUM
#undef INTEGRAL2BIG
#undef NUM2INTEGRAL
#undef UNSIGNED
#undef ITYPE
#undef SIZEOF_ITYPE
#undef UNSIGNED_ITYPE
#undef UNSIGNED_ITYPE_MAX
#undef BIGMPZ_FITSP

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
