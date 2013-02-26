/* Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

void test_ffi_v_ (void);
void test_ffi_v_ (void)
{
  return;
}

void test_ffi_v_u8 (scm_t_uint8 a);
void test_ffi_v_u8 (scm_t_uint8 a)
{
  return;
}

void test_ffi_v_s64 (scm_t_int64 a);
void test_ffi_v_s64 (scm_t_int64 a)
{
  return;
}

scm_t_int8 test_ffi_s8_ (void);
scm_t_int8 test_ffi_s8_ (void)
{
  return -100;
}
scm_t_int8 test_ffi_s8_u8 (scm_t_uint8 a);
scm_t_int8 test_ffi_s8_u8 (scm_t_uint8 a)
{
  return -100 + a;
}

scm_t_int8 test_ffi_s8_s64 (scm_t_int64 a);
scm_t_int8 test_ffi_s8_s64 (scm_t_int64 a)
{
  return -100 + a;
}

scm_t_uint8 test_ffi_u8_ (void);
scm_t_uint8 test_ffi_u8_ (void)
{
  return 200;
}

scm_t_uint8 test_ffi_u8_u8 (scm_t_uint8 a);
scm_t_uint8 test_ffi_u8_u8 (scm_t_uint8 a)
{
  return 200 + a;
}

scm_t_uint8 test_ffi_u8_s64 (scm_t_int64 a);
scm_t_uint8 test_ffi_u8_s64 (scm_t_int64 a)
{
  return 200 + a;
}

scm_t_int16 test_ffi_s16_ (void);
scm_t_int16 test_ffi_s16_ (void)
{
  return -20000;
}

scm_t_int16 test_ffi_s16_u8 (scm_t_uint8 a);
scm_t_int16 test_ffi_s16_u8 (scm_t_uint8 a)
{
  return -20000 + a;
}

scm_t_int16 test_ffi_s16_s64 (scm_t_int64 a);
scm_t_int16 test_ffi_s16_s64 (scm_t_int64 a)
{
  return -20000 + a;
}

scm_t_uint16 test_ffi_u16_ (void);
scm_t_uint16 test_ffi_u16_ (void)
{
  return 40000;
}

scm_t_uint16 test_ffi_u16_u8 (scm_t_uint8 a);
scm_t_uint16 test_ffi_u16_u8 (scm_t_uint8 a)
{
  return 40000 + a;
}

scm_t_uint16 test_ffi_u16_s64 (scm_t_int64 a);
scm_t_uint16 test_ffi_u16_s64 (scm_t_int64 a)
{
  return 40000 + a;
}

scm_t_int32 test_ffi_s32_ (void);
scm_t_int32 test_ffi_s32_ (void)
{
  return -2000000000;
}

scm_t_int32 test_ffi_s32_u8 (scm_t_uint8 a);
scm_t_int32 test_ffi_s32_u8 (scm_t_uint8 a)
{
  return -2000000000 + a;
}

scm_t_int32 test_ffi_s32_s64 (scm_t_int64 a);
scm_t_int32 test_ffi_s32_s64 (scm_t_int64 a)
{
  return -2000000000 + a;
}

scm_t_uint32 test_ffi_u32_ (void);
scm_t_uint32 test_ffi_u32_ (void)
{
  return 4000000000U;
}

scm_t_uint32 test_ffi_u32_u8 (scm_t_uint8 a);
scm_t_uint32 test_ffi_u32_u8 (scm_t_uint8 a)
{
  return 4000000000U + a;
}

scm_t_uint32 test_ffi_u32_s64 (scm_t_int64 a);
scm_t_uint32 test_ffi_u32_s64 (scm_t_int64 a)
{
  return 4000000000U + a;
}

/* FIXME: use 64-bit literals */
scm_t_int64 test_ffi_s64_ (void);
scm_t_int64 test_ffi_s64_ (void)
{
  return -2000000000;
}

scm_t_int64 test_ffi_s64_u8 (scm_t_uint8 a);
scm_t_int64 test_ffi_s64_u8 (scm_t_uint8 a)
{
  return -2000000000 + a;
}

scm_t_int64 test_ffi_s64_s64 (scm_t_int64 a);
scm_t_int64 test_ffi_s64_s64 (scm_t_int64 a)
{
  return -2000000000 + a;
}

scm_t_uint64 test_ffi_u64_ (void);
scm_t_uint64 test_ffi_u64_ (void)
{
  return 4000000000UL;
}

scm_t_uint64 test_ffi_u64_u8 (scm_t_uint8 a);
scm_t_uint64 test_ffi_u64_u8 (scm_t_uint8 a)
{
  return 4000000000UL + a;
}

scm_t_uint64 test_ffi_u64_s64 (scm_t_int64 a);
scm_t_uint64 test_ffi_u64_s64 (scm_t_int64 a)
{
  return 4000000000UL + a;
}


scm_t_int64 test_ffi_sum (scm_t_int8 a, scm_t_int16 b,
                          scm_t_int32 c, scm_t_int64 d);
scm_t_int64 test_ffi_sum (scm_t_int8 a, scm_t_int16 b,
                          scm_t_int32 c, scm_t_int64 d)
{
  return d + c + b + a;
}


scm_t_int64 test_ffi_sum_many (scm_t_uint8 a, scm_t_uint16 b,
                               scm_t_uint32 c, scm_t_uint64 d,
                               scm_t_int8 e, scm_t_int16 f,
                               scm_t_int32 g, scm_t_int64 h,
                               scm_t_int8 i, scm_t_int16 j,
                               scm_t_int32 k, scm_t_int64 l);
scm_t_int64 test_ffi_sum_many (scm_t_uint8 a, scm_t_uint16 b,
                               scm_t_uint32 c, scm_t_uint64 d,
                               scm_t_int8 e, scm_t_int16 f,
                               scm_t_int32 g, scm_t_int64 h,
                               scm_t_int8 i, scm_t_int16 j,
                               scm_t_int32 k, scm_t_int64 l)
{
  return l + k + j + i + h + g + f + e + d + c + b + a;
}


struct foo
{
  scm_t_int8 a;
  scm_t_int16 b;
  scm_t_int32 c;
  scm_t_int64 d;
};
scm_t_int64 test_ffi_sum_struct (struct foo foo);
scm_t_int64 test_ffi_sum_struct (struct foo foo)
{
  return foo.d + foo.c + foo.b + foo.a;
}


void* test_ffi_memcpy (void *dest, void *src, scm_t_int32 n);
void* test_ffi_memcpy (void *dest, void *src, scm_t_int32 n)
{
  return memcpy (dest, src, n);
}

int test_ffi_callback_1 (int (*f) (int), int x);
int test_ffi_callback_1 (int (*f) (int), int x)
{
  return f (x) + 7;
}

double test_ffi_callback_2 (double (*f) (float, int, double),
			    float x, int y, double z);
double test_ffi_callback_2 (double (*f) (float, int, double),
			    float x, int y, double z)
{
  return f (x, y, z);
}
