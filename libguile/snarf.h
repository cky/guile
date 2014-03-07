/* classes: h_files */

#ifndef SCM_SNARF_H
#define SCM_SNARF_H

/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
 *   2004, 2006, 2009, 2010, 2011, 2014 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/* Macros for snarfing initialization actions from C source. */

/* Casting to a function that can take any number of arguments.  */
#define SCM_FUNC_CAST_ARBITRARY_ARGS  scm_t_subr


#ifdef SCM_ALIGNED
/* We support static allocation of some `SCM' objects.  */
# define SCM_SUPPORT_STATIC_ALLOCATION
#endif

/* C preprocessor token concatenation.  */
#define scm_i_paste(x, y)      x ## y
#define scm_i_paste3(a, b, c)  a ## b ## c



/* Generic macros to be used in user macro definitions.
 *
 * For example, in order to define a macro which creates ints and
 * initializes them to the result of foo (), do:
 *
 *   #define SCM_FOO(NAME) \
 *     SCM_SNARF_HERE (int NAME) \
 *     SCM_SNARF_INIT (NAME = foo ())
 *
 * The SCM_SNARF_INIT text goes into the corresponding .x file
 * up through the first occurrence of SCM_SNARF_DOC_START on that
 * line, if any.
 *
 * Some debugging options can cause the preprocessor to echo #define
 * directives to its output. Keeping the snarfing markers on separate
 * lines prevents guile-snarf from inadvertently snarfing the definition
 * of SCM_SNARF_INIT if those options are in effect.
 */

#ifdef SCM_MAGIC_SNARF_INITS
# define SCM_SNARF_HERE(X)
# define SCM_SNARF_INIT_PREFIX ^^
# define SCM_SNARF_INIT(X) SCM_SNARF_INIT_PREFIX X ^:^
# define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
#else
# ifdef SCM_MAGIC_SNARF_DOCS
#  define SCM_SNARF_HERE(X)
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING) \
^^ { \
cname CNAME ^^ \
fname FNAME ^^ \
type TYPE ^^ \
location __FILE__ __LINE__ ^^ \
arglist ARGLIST ^^ \
argsig REQ OPT VAR ^^ \
DOCSTRING ^^ }
# else
#  define SCM_SNARF_HERE(X) X
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
# endif
#endif

#define SCM_DEFINE_GSUBR(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#ifdef SCM_SUPPORT_STATIC_ALLOCATION

/* Static subr allocation.  */
/* FIXME: how to verify that req + opt + rest < 11, all are positive, etc? */
#define SCM_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING)	\
SCM_SYMBOL (scm_i_paste (FNAME, __name), PRIMNAME);			\
SCM_SNARF_HERE(							        \
  SCM_UNUSED static const char scm_i_paste (s_, FNAME) [] = PRIMNAME;	\
  SCM_API SCM FNAME ARGLIST;						\
  SCM_IMMUTABLE_POINTER (scm_i_paste (FNAME, __subr_foreign),           \
                         (scm_t_bits) &FNAME); /* the subr */           \
  SCM_STATIC_SUBR_OBJVECT (scm_i_paste (FNAME, __raw_objtable),         \
                           /* FIXME: directly be the foreign */         \
                           SCM_BOOL_F);                                 \
  /* FIXME: be immutable. grr */                                        \
  SCM_STATIC_PROGRAM (scm_i_paste (FNAME, __subr),			\
                      SCM_BOOL_F,                                       \
                      SCM_PACK (&scm_i_paste (FNAME, __raw_objtable)),  \
                      SCM_BOOL_F);                                      \
  SCM FNAME ARGLIST							\
)									\
SCM_SNARF_INIT(							\
  /* Initialize the foreign.  */                                        \
  scm_i_paste (FNAME, __raw_objtable)[2] = scm_i_paste (FNAME, __subr_foreign); \
  /* Initialize the procedure name (an interned symbol).  */		\
  scm_i_paste (FNAME, __raw_objtable)[3] = scm_i_paste (FNAME, __name); \
  /* Initialize the objcode trampoline.  */                             \
  SCM_SET_CELL_OBJECT (scm_i_paste (FNAME, __subr), 1,                  \
                       scm_subr_objcode_trampoline (REQ, OPT, VAR));    \
									\
  /* Define the subr.  */						\
  scm_define (scm_i_paste (FNAME, __name), scm_i_paste (FNAME, __subr)); \
)									\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#else /* !SCM_SUPPORT_STATIC_ALLOCATION */

/* Always use the generic subr case.  */
#define SCM_DEFINE SCM_DEFINE_GSUBR

#endif /* !SCM_SUPPORT_STATIC_ALLOCATION */


#define SCM_PRIMITIVE_GENERIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
static SCM g_ ## FNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
g_ ## FNAME = SCM_PACK (0); \
scm_c_define_gsubr_with_generic (s_ ## FNAME, REQ, OPT, VAR, \
                    		 (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME, \
				 &g_ ## FNAME); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_DEFINE_PUBLIC(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
SCM_SNARF_HERE(\
SCM_UNUSED static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
SCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                    (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
scm_c_export (s_ ## FNAME, NULL); \
)\
SCM_SNARF_DOCS(primitive, FNAME, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)

#define SCM_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(SCM_UNUSED static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN))

#define SCM_REGISTER_PROC(RANAME, STR, REQ, OPT, VAR, CFN)  \
SCM_SNARF_HERE(SCM_UNUSED static const char RANAME[]=STR) \
SCM_SNARF_INIT(scm_c_define_gsubr (RANAME, REQ, OPT, VAR, \
                                   (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN);) \
SCM_SNARF_DOCS(register, CFN, STR, (), REQ, OPT, VAR, \
               "implemented by the C function \"" #CFN "\"")

#define SCM_GPROC(RANAME, STR, REQ, OPT, VAR, CFN, GF)  \
SCM_SNARF_HERE(\
SCM_UNUSED static const char RANAME[]=STR;\
static SCM GF \
)SCM_SNARF_INIT(\
GF = SCM_PACK (0);  /* Dirk:FIXME:: Can we safely use #f instead of 0? */ \
scm_c_define_gsubr_with_generic (RANAME, REQ, OPT, VAR, \
                                 (SCM_FUNC_CAST_ARBITRARY_ARGS) CFN, &GF) \
)

#ifdef SCM_SUPPORT_STATIC_ALLOCATION

# define SCM_SYMBOL(c_name, scheme_name)				\
SCM_SNARF_HERE(								\
  SCM_IMMUTABLE_STRING (scm_i_paste (c_name, _string), scheme_name);	\
  static SCM c_name)							\
SCM_SNARF_INIT(								\
  c_name = scm_string_to_symbol (scm_i_paste (c_name, _string))		\
)

# define SCM_GLOBAL_SYMBOL(c_name, scheme_name)				\
SCM_SNARF_HERE(								\
  SCM_IMMUTABLE_STRING (scm_i_paste (c_name, _string), scheme_name);	\
  SCM c_name)								\
SCM_SNARF_INIT(								\
  c_name = scm_string_to_symbol (scm_i_paste (c_name, _string))		\
)

#else /* !SCM_SUPPORT_STATIC_ALLOCATION */

# define SCM_SYMBOL(c_name, scheme_name)				\
SCM_SNARF_HERE(static SCM c_name)					\
SCM_SNARF_INIT(c_name = scm_from_locale_symbol (scheme_name))

# define SCM_GLOBAL_SYMBOL(c_name, scheme_name)				\
SCM_SNARF_HERE(SCM c_name)						\
SCM_SNARF_INIT(c_name = scm_from_locale_symbol (scheme_name))

#endif /* !SCM_SUPPORT_STATIC_ALLOCATION */

#define SCM_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_from_locale_keyword (scheme_name))

#define SCM_GLOBAL_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_from_locale_keyword (scheme_name))

#define SCM_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, SCM_BOOL_F);)

#define SCM_GLOBAL_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, SCM_BOOL_F);)

#define SCM_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, init_val);)

#define SCM_GLOBAL_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, init_val);)

#define SCM_MUTEX(c_name) \
SCM_SNARF_HERE(static scm_t_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_mutex_init (&c_name, &scm_i_plugin_mutex))

#define SCM_GLOBAL_MUTEX(c_name) \
SCM_SNARF_HERE(scm_t_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_mutex_init (&c_name, &scm_i_plugin_mutex))

#define SCM_REC_MUTEX(c_name) \
SCM_SNARF_HERE(static scm_t_rec_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_rec_mutex_init (&c_name, &scm_i_plugin_rec_mutex))

#define SCM_GLOBAL_REC_MUTEX(c_name) \
SCM_SNARF_HERE(scm_t_rec_mutex c_name) \
SCM_SNARF_INIT(scm_i_plugin_rec_mutex_init (&c_name, &scm_i_plugin_rec_mutex))

#define SCM_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(static scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_GLOBAL_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(static SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_GLOBAL_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(static size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_GLOBAL_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(static int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_GLOBAL_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(static SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_GLOBAL_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(static SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)

#define SCM_GLOBAL_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)


/* Low-level snarfing for static memory allocation.  */

#ifdef SCM_SUPPORT_STATIC_ALLOCATION

#define SCM_IMMUTABLE_CELL(c_name, car, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED const scm_t_cell			\
       c_name ## _raw_scell =						\
  {                                                                     \
    SCM_PACK (car),                                                     \
    SCM_PACK (cdr)                                                      \
  };                                                                    \
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw_scell)

#define SCM_IMMUTABLE_DOUBLE_CELL(c_name, car, cbr, ccr, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED const scm_t_cell			\
  c_name ## _raw_cell [2] =						\
    {									\
      { SCM_PACK (car), SCM_PACK (cbr) },				\
      { SCM_PACK (ccr), SCM_PACK (cdr) }				\
    };									\
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw_cell)

#define SCM_STATIC_DOUBLE_CELL(c_name, car, cbr, ccr, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED scm_t_cell                          \
  c_name ## _raw_cell [2] =						\
    {									\
      { SCM_PACK (car), SCM_PACK (cbr) },				\
      { SCM_PACK (ccr), SCM_PACK (cdr) }				\
    };									\
  static SCM_UNUSED SCM c_name = SCM_PACK (& c_name ## _raw_cell)

#define SCM_IMMUTABLE_STRINGBUF(c_name, contents)	\
  static SCM_UNUSED const				\
  struct						\
  {							\
    scm_t_bits word_0;					\
    scm_t_bits word_1;					\
    const char buffer[sizeof (contents)];		\
  }							\
  c_name =						\
    {							\
      scm_tc7_stringbuf | SCM_I_STRINGBUF_F_SHARED,	\
      sizeof (contents) - 1,				\
      contents						\
    }

#define SCM_IMMUTABLE_STRING(c_name, contents)				\
  SCM_IMMUTABLE_STRINGBUF (scm_i_paste (c_name, _stringbuf), contents);	\
  SCM_IMMUTABLE_DOUBLE_CELL (c_name,					\
			     scm_tc7_ro_string,				\
			     (scm_t_bits) &scm_i_paste (c_name,		\
							_stringbuf),	\
			     (scm_t_bits) 0,				\
                             (scm_t_bits) (sizeof (contents) - 1))

#define SCM_IMMUTABLE_POINTER(c_name, ptr)		\
  SCM_IMMUTABLE_CELL (c_name, scm_tc7_pointer, ptr)

/* for primitive-generics, add a foreign to the end */
#define SCM_STATIC_SUBR_OBJVECT(c_name, foreign)                        \
  static SCM_ALIGNED (8) SCM c_name[4] =                                \
  {                                                                     \
    SCM_PACK (scm_tc7_vector | (2 << 8)),                               \
    SCM_PACK (0),                                                       \
    foreign,                                                            \
    SCM_BOOL_F, /* the name */                                          \
  }

#define SCM_STATIC_PROGRAM(c_name, objcode, objtable, freevars)         \
  static SCM_ALIGNED (8) SCM_UNUSED SCM                                 \
       scm_i_paste (c_name, _raw_cell)[] =                              \
  {                                                                     \
    SCM_PACK (scm_tc7_program | SCM_F_PROGRAM_IS_PRIMITIVE),            \
    objcode,                                                            \
    objtable,                                                           \
    freevars                                                            \
  };                                                                    \
  static SCM_UNUSED const SCM c_name =                                  \
    SCM_PACK (& scm_i_paste (c_name, _raw_cell))

#endif /* SCM_SUPPORT_STATIC_ALLOCATION */


/* Documentation.  */

#ifdef SCM_MAGIC_SNARF_DOCS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) ^^ argpos _arg _pos __LINE__ ^^
#endif /* SCM_MAGIC_SNARF_DOCS */

#endif  /* SCM_SNARF_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
