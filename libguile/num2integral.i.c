/* this file is #include'd (many times) by numbers.c */

#ifndef UNSIGNED_ITYPE
#ifdef UNSIGNED
#define UNSIGNED_ITYPE ITYPE
#else
#define UNSIGNED_ITYPE unsigned ITYPE
#endif
#endif

#define UNSIGNED_ITYPE_MAX (~((UNSIGNED_ITYPE)0))

#ifndef SIZEOF_ITYPE
#define SIZEOF_ITYPE (2*SIZEOF_SCM_T_BITS)
#endif

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
    
#if SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS
      /* the target type is large enough to hold any possible inum */
      return (ITYPE) n;
#else
      /* an inum can be out of range, so check */
#ifdef UNSIGNED
      /* n is known to be >= 0 */
      if ((scm_t_bits) n > UNSIGNED_ITYPE_MAX)
	scm_out_of_range (s_caller, num);
#else
      if (((ITYPE)n) != n)
	scm_out_of_range (s_caller, num);
#endif
      return (ITYPE) n;
#endif /* SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS */
    }
  else if (SCM_BIGP (num))
    { /* bignum */
#if SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS
      
      UNSIGNED_ITYPE pos_res = 0;
      size_t l;

#ifdef UNSIGNED
      if (SCM_BIGSIGN (num))
        scm_out_of_range (s_caller, num);
#endif

      for (l = SCM_NUMDIGS (num); l--;)
        {
	  if (pos_res > SCM_BIGDN (UNSIGNED_ITYPE_MAX))
            scm_out_of_range (s_caller, num);
	  pos_res = SCM_I_BIGUP (ITYPE, pos_res) + SCM_BDIGITS (num)[l];
        }
    
#ifdef UNSIGNED
      return pos_res;
#else
      if (SCM_BIGSIGN (num))
        {
          ITYPE res = -((ITYPE)pos_res);
          if (res <= 0)
            return res;
          else
            scm_out_of_range (s_caller, num);
        }
      else
        {
	  ITYPE res = (ITYPE)pos_res;
          if (res >= 0)
            return res;
          else
            scm_out_of_range (s_caller, num);
        }
#endif

#else /* SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS */
      scm_out_of_range (s_caller, num);
#endif
      
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

#if SIZEOF_ITYPE >= SIZEOF_SCM_T_BITS
#ifndef UNSIGNED
  if (SCM_FIXABLE (n))
#else
  if (SCM_POSFIXABLE (n))
#endif
#endif
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
  /* If n is still negative here, it must be the minimum value of the
     type (assuming twos-complement, but we are tied to that anyway).
     If this is the case, we can not count the number of digits by
     right-shifting n until it is zero.
  */
  if (n < 0)
    {
      /* special case */
      n_digits = 
	(sizeof (ITYPE) + sizeof (SCM_BIGDIG) - 1) / sizeof (SCM_BIGDIG);
    }
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
#ifdef UNSIGNED
#undef UNSIGNED
#endif
#undef ITYPE
#undef SIZEOF_ITYPE
#undef UNSIGNED_ITYPE
#undef UNSIGNED_ITYPE_MAX

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
