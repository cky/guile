/* this file is #include'd (x times) by convert.c */

/* FIXME: Should we use exported wrappers for malloc (and free), which
 * allow windows DLLs to call the correct freeing function? */


/* Convert a vector, weak vector, (if possible string, substring), list
   or uniform vector into an C array.  If result array in argument 2 is 
   NULL, malloc() a new one.  If out of memory, return NULL.  */
#define FUNC_NAME SCM2CTYPES_FN
CTYPE *
SCM2CTYPES (SCM obj, CTYPE *data)
{
  long i, n;
  SCM val;

  SCM_ASSERT (SCM_NIMP (obj) || SCM_NFALSEP (scm_list_p (obj)), 
	      obj, SCM_ARG1, FUNC_NAME);

  if (SCM_NFALSEP (scm_list_p (obj)))
    {
      SCM list = obj;
      for (n = 0; SCM_NFALSEP (scm_pair_p (list)); list = SCM_CDR (list), n++)
	{
	  val = SCM_CAR (list);
#if defined (CTYPEMIN) && defined (CTYPEMAX)
          if (SCM_INUMP (val))
            {
              long v = SCM_INUM (val);
	      SCM_ASSERT_RANGE (SCM_ARG1, obj, v >= CTYPEMIN && v <= CTYPEMAX);
            }
          else
#elif defined (FLOATTYPE1)
          if (!SCM_INUMP (val) && !(SCM_BIGP (val) || SCM_REALP (val)))
#else
	  if (!SCM_INUMP (val) && !SCM_BIGP (val))
#endif
            SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
        }
      if (data == NULL)
        data = (CTYPE *) malloc (n * sizeof (CTYPE));
      if (data == NULL)
        return NULL;

      list = obj;
      for (i = 0; SCM_NFALSEP (scm_pair_p (list)); list = SCM_CDR (list), i++)
	{
          val = SCM_CAR (list);
	  if (SCM_INUMP (val))
            data[i] = SCM_INUM (val);
          else if (SCM_BIGP (val))
	    data[i] = (CTYPE) scm_num2long (val, SCM_ARG1, FUNC_NAME);
#ifdef FLOATTYPE1
          else
            data[i] = (CTYPE) SCM_REAL_VALUE (val);
#endif
	}
      return data;
    }

  switch (SCM_TYP7 (obj))
    {
    case scm_tc7_vector:
    case scm_tc7_wvect:
      n = SCM_VECTOR_LENGTH (obj);
      for (i = 0; i < n; i++)
        {
          val = SCM_VELTS (obj)[i];

#if defined (CTYPEMIN) && defined (CTYPEMAX)
          if (SCM_INUMP (val))
            {
              long v = SCM_INUM (val);
	      SCM_ASSERT_RANGE (SCM_ARG1, obj, v >= CTYPEMIN && v <= CTYPEMAX);
            }
          else
#elif defined (FLOATTYPE1)
          if (!SCM_INUMP (val) && !(SCM_BIGP (val) || SCM_REALP (val)))
#else
	  if (!SCM_INUMP (val) && !SCM_BIGP (val))
#endif
            SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
        }
      if (data == NULL)
        data = (CTYPE *) malloc (n * sizeof (CTYPE));
      if (data == NULL)
        return NULL;
      for (i = 0; i < n; i++)
	{
          val = SCM_VELTS (obj)[i];
	  if (SCM_INUMP (val))
            data[i] = (CTYPE) SCM_INUM (val);
          else if (SCM_BIGP (val))
	    data[i] = (CTYPE) scm_num2long (val, SCM_ARG1, FUNC_NAME);
#ifdef FLOATTYPE1
          else
            data[i] = (CTYPE) SCM_REAL_VALUE (val);
#endif
	}
      break;

#ifdef HAVE_ARRAYS
    case ARRAYTYPE1:
#ifdef ARRAYTYPE2
    case ARRAYTYPE2:
#endif
      n = SCM_UVECTOR_LENGTH (obj);
      if (data == NULL)
        data = (CTYPE *) malloc (n * sizeof (CTYPE));
      if (data == NULL)
        return NULL;
#ifdef FLOATTYPE2
      if (SCM_TYP7 (obj) == ARRAYTYPE2)
	{
	  for (i = 0; i < n; i++)
	    data[i] = ((FLOATTYPE2 *) SCM_UVECTOR_BASE (obj))[i];
	}
      else
#endif
      memcpy (data, (CTYPE *) SCM_UVECTOR_BASE (obj), n * sizeof (CTYPE));
      break;
#endif /* HAVE_ARRAYS */

#ifdef STRINGTYPE
    case scm_tc7_string:
      n = SCM_STRING_LENGTH (obj);
      if (data == NULL)
        data = (CTYPE *) malloc (n * sizeof (CTYPE));
      if (data == NULL)
        return NULL;
      memcpy (data, SCM_STRING_CHARS (obj), n * sizeof (CTYPE));
      break;
#endif /* STRINGTYPE */

    default:
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
    }
  return data;
}
#undef FUNC_NAME


#if HAVE_ARRAYS

/* Converts a C array into a uniform vector, returns SCM_UNDEFINED if out
   of memory. */
#define FUNC_NAME CTYPES2UVECT_FN
SCM
CTYPES2UVECT (const CTYPE *data, long n)
{
  char *v;

  SCM_ASSERT_RANGE (SCM_ARG2, scm_long2num (n), 
		    n > 0 && n <= SCM_UVECTOR_MAX_LENGTH);
  if ((v = (char *) SCM_MUST_MALLOC_TYPE_NUM (CTYPE, n)) == NULL)
    return SCM_UNDEFINED;
  memcpy (v, data, n * sizeof (CTYPE));
  return scm_alloc_cell (SCM_MAKE_UVECTOR_TAG (n, UVECTTYPE), (scm_t_bits) v);
}
#undef FUNC_NAME

#ifdef UVECTTYPE2
#define FUNC_NAME CTYPES2UVECT_FN2
SCM
CTYPES2UVECT2 (const unsigned CTYPE *data, long n)
{
  char *v;

  SCM_ASSERT_RANGE (SCM_ARG2, scm_long2num (n), 
		    n > 0 && n <= SCM_UVECTOR_MAX_LENGTH);
  if ((v = (char *) SCM_MUST_MALLOC_TYPE_NUM (unsigned CTYPE, n)) == NULL)
    return SCM_UNDEFINED;
  memcpy (v, data, n * sizeof (unsigned CTYPE));
  return scm_alloc_cell (SCM_MAKE_UVECTOR_TAG (n, UVECTTYPE2), (scm_t_bits) v);
}
#undef FUNC_NAME
#endif /* UVECTTYPE2 */

#endif /* HAVE_ARRAYS */

/* Converts a C array into a vector. */
#define FUNC_NAME CTYPES2SCM_FN
SCM
CTYPES2SCM (const CTYPE *data, long n)
{
  long i;
  SCM v, *velts;

  SCM_ASSERT_RANGE (SCM_ARG2, scm_long2num (n), 
		    n > 0 && n <= SCM_VECTOR_MAX_LENGTH);
  v = scm_c_make_vector (n, SCM_UNSPECIFIED);
  velts = SCM_VELTS (v);
  for (i = 0; i < n; i++)
#ifdef FLOATTYPE1
    velts[i] = scm_make_real ((double) data[i]);
#elif defined (CTYPEFIXABLE)
    velts[i] = SCM_MAKINUM (data[i]);
#else
    velts[i] = (SCM_FIXABLE (data[i]) ? SCM_MAKINUM (data[i]) : 
		scm_i_long2big (data[i]));
#endif
  return v;
}
#undef FUNC_NAME

/* cleanup of conditionals */
#undef SCM2CTYPES
#undef SCM2CTYPES_FN
#undef CTYPES2SCM
#undef CTYPES2SCM_FN
#undef CTYPE
#undef CTYPES2UVECT
#undef CTYPES2UVECT_FN
#ifdef CTYPEFIXABLE
#undef CTYPEFIXABLE
#endif
#undef UVECTTYPE
#ifdef UVECTTYPE2
#undef CTYPES2UVECT2
#undef CTYPES2UVECT_FN2
#undef UVECTTYPE2
#endif
#ifdef CTYPEMIN
#undef CTYPEMIN
#endif
#ifdef CTYPEMAX
#undef CTYPEMAX
#endif
#undef ARRAYTYPE1
#ifdef ARRAYTYPE2
#undef ARRAYTYPE2
#endif
#ifdef FLOATTYPE1
#undef FLOATTYPE1
#endif
#ifdef FLOATTYPE2
#undef FLOATTYPE2
#endif
#ifdef STRINGTYPE
#undef STRINGTYPE
#endif

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
