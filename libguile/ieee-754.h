/* Copyright (C) 1992, 1995, 1996, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#ifndef SCM_IEEE_754_H
#define SCM_IEEE_754_H 1

/* Based on glibc's <ieee754.h> and modified by Ludovic Courtès to include
   all possible IEEE-754 double-precision representations.  */


/* IEEE 754 simple-precision format (32-bit).  */

union scm_ieee754_float
  {
    float f;

    struct
      {
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
      } big_endian;

    struct
      {
      	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
      } little_endian;
  };



/* IEEE 754 double-precision format (64-bit).  */

union scm_ieee754_double
  {
    double d;

    struct
      {
	/* Big endian.  */

	unsigned int negative:1;
	unsigned int exponent:11;
	/* Together these comprise the mantissa.  */
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
      } big_endian;

    struct
      {
	/* Both byte order and word order are little endian.  */

	/* Together these comprise the mantissa.  */
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
      } little_little_endian;

    struct
      {
	/* Byte order is little endian but word order is big endian.  Not
	   sure this is very wide spread.  */
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
	unsigned int mantissa1:32;
      } little_big_endian;

  };


#endif /* SCM_IEEE_754_H */
