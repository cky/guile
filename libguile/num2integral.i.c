/* this file is #include'd (many times) by numbers.c */

ITYPE
NUM2INTEGRAL (SCM num, unsigned long int pos, const char *s_caller)
{
  if (SCM_INUMP (num))
    { /* immediate */
    
      scm_t_signed_bits n = SCM_INUM (num);

#ifdef UNSIGNED
      if (n < 0)
        scm_out_of_range (s_caller, num);
#endif
    
      if (sizeof (ITYPE) >= sizeof (scm_t_signed_bits))
        /* can't fit anything too big for this type in an inum
           anyway */
        return (ITYPE) n;
      else
        { /* an inum can be out of range, so check */
          if (n > (scm_t_signed_bits)MAX_VALUE
#ifndef UNSIGNED
              || n < (scm_t_signed_bits)MIN_VALUE
#endif
              )
            scm_out_of_range (s_caller, num);
          else
            return (ITYPE) n;
        }
    }
  else if (SCM_BIGP (num))
    { /* bignum */
    
      ITYPE res = 0;
      size_t l;

      for (l = SCM_NUMDIGS (num); l--;)
        {
          ITYPE new = SCM_I_BIGUP (ITYPE, res) + SCM_BDIGITS (num)[l];
          if (new < res
#ifndef UNSIGNED
              && !(new == MIN_VALUE && l == 0)
#endif
              )
            scm_out_of_range (s_caller, num);
          res = new;
        }
    
      if (SCM_BIGSIGN (num))
#ifdef UNSIGNED
        scm_out_of_range (s_caller, num);
#else
        {
          res = -res;
          if (res <= 0)
            return res;
          else
            scm_out_of_range (s_caller, num);
        }
#endif
      else
        {
          if (res >= 0)
            return res;
          else
            scm_out_of_range (s_caller, num);
        }

      return res;
    }
  else if (SCM_REALP (num))
    { /* inexact */
    
      double u = SCM_REAL_VALUE (num);
      ITYPE res = u;
      if ((double) res == u)
        return res;
      else
        scm_out_of_range (s_caller, num);
    }
  else
    scm_wrong_type_arg (s_caller, pos, num);
}

SCM
INTEGRAL2NUM (ITYPE n)
{
  if (sizeof (ITYPE) < sizeof (scm_t_signed_bits)
      ||
#ifndef UNSIGNED  
      SCM_FIXABLE (n)
#else
      SCM_POSFIXABLE (n)
#endif 
      )
    return SCM_MAKINUM ((scm_t_signed_bits) n);

#ifdef SCM_BIGDIG
  return INTEGRAL2BIG (n);
#else
  return scm_make_real ((double) n);
#endif
}

#ifdef SCM_BIGDIG

SCM
INTEGRAL2BIG (ITYPE n)
{
  SCM res;
  int neg_p;
  unsigned int n_digits;
  size_t i;
  SCM_BIGDIG *digits;
      
#ifndef UNSIGNED  
  neg_p = (n < 0);
  if (neg_p) n = -n;
#else
  neg_p = 0;
#endif

#ifndef UNSIGNED
  if (n == MIN_VALUE)
    /* special case */
    n_digits =
      (sizeof (ITYPE) + sizeof (SCM_BIGDIG) - 1) / sizeof (SCM_BIGDIG);
  else
#endif
    {
      ITYPE tn;
      for (tn = n, n_digits = 0;
           tn;
           ++n_digits, tn = SCM_BIGDN (tn))
        ;
    }

  i = 0;
  res = scm_i_mkbig (n_digits, neg_p);
  digits = SCM_BDIGITS (res);

  while (i < n_digits)
    {
      digits[i++] = SCM_BIGLO (n);
      n = SCM_BIGDN (n);
    }
    
  return res;
}

#endif

/* clean up */
#undef INTEGRAL2NUM
#undef INTEGRAL2BIG
#undef NUM2INTEGRAL
#undef UNSIGNED
#undef ITYPE
#undef MIN_VALUE
#undef MAX_VALUE

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
