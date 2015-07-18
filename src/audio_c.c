/*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdint.h>

#define INT24_MAX ((1 << 23) - 1)
#define INT24_MIN (-INT24_MAX)

typedef uint8_t int24_t[3];

static inline void int24_of_int32(int32_t x, int24_t d) {
  d[0] = x;
  d[1] = x >> 8;
  d[2] = x >> 16;
}
static inline int32_t int32_of_int24(int24_t x) {
  int32_t tmp = x[0] | (x[1] << 8) | (x[2] << 16);

  return INT24_MAX < tmp ? (0xff000000 | tmp) : tmp; 
}

#ifdef __linux__
  #include <byteswap.h>
#endif

#if defined(__APPLE__)
  #include <libkern/OSByteOrder.h>
  #define bswap_16 OSSwapInt16
  #define bswap_32 OSSwapInt32
#endif

#ifndef bswap_16
  #define bswap_16(x) \
    ((int16_t)((((int16_t)(x) & 0xff00) >> 8) | \
               (((int16_t)(x) & 0x00ff) << 8)))
#endif

#ifndef bswap_32
  #define bswap_32(x) \
    ((int32_t)((((int32_t)(x)  & 0xff000000) >> 24) | \
                (((int32_t)(x) & 0x00ff0000) >>  8) | \
                (((int32_t)(x) & 0x0000ff00) <<  8) | \
                (((int32_t)(x) & 0x000000ff) << 24)))
#endif

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

/* Optimized implementation of Array.blit for float arrays.
 * See http://caml.inria.fr/mantis/view.php?id=2787
 */
CAMLprim value caml_float_array_blit(value _src, value _src_off,
                                     value _dst, value _dst_off, value _len) {
  int src_off = Int_val(_src_off) ;
  int dst_off = Int_val(_dst_off) ;
  int len = Int_val(_len) ;
  int i ;
  for (i=0 ; i<len ; i++)
    Store_double_field(_dst,dst_off+i,Double_field(_src,src_off+i)) ;
  return Val_unit ;
}

static inline int16_t clip(double s)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT16_MIN;
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT16_MAX;
  }
  else
    return (s * INT16_MAX);
}

static inline int32_t s32_clip(double s)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT32_MIN;
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return INT32_MAX;
  }
  else
    return (s * INT32_MAX);
}

static inline void s24_clip(double s, int24_t x)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return int24_of_int32(INT24_MIN, x);
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return int24_of_int32(INT24_MAX, x);
  }
  else
    return int24_of_int32(s * INT24_MAX, x);
}

static inline uint8_t u8_clip(double s)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return 0;
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return 255;
  }
  else
    return (s * 127. + 128.);
}

#define u8tof(x)  (((double)x-INT8_MAX)/INT8_MAX)
#define s16tof(x) (((double)x)/INT16_MAX)
#define s24tof(x) (((double)int32_of_int24(x))/INT24_MAX)
#define s32tof(x) (((double)x)/INT32_MAX)

#define get_u8(src,offset,nc,c,i)    u8tof(((uint8_t*)src)[offset+i*nc+c])
#define get_s24le(src,offset,nc,c,i) s24tof(((int24_t*)src)[offset/3+i*nc+c])

#ifdef BIGENDIAN
#define get_s16le(src,offset,nc,c,i) s16tof(bswap_16(((int16_t*)src)[offset/2+i*nc+c]))
#define get_s16be(src,offset,nc,c,i) s16tof(((int16_t*)src)[offset/2+i*nc+c])
#define get_s32le(src,offset,nc,c,i) s32tof(bswap_32(((int32_t*)src)[offset/4+i*nc+c]))
#else
#define get_s16le(src,offset,nc,c,i) s16tof(((int16_t*)src)[offset/2+i*nc+c])
#define get_s16be(src,offset,nc,c,i) s16tof(bswap_16(((int16_t*)src)[offset/2+i*nc+c]))
#define get_s32le(src,offset,nc,c,i) s32tof(((int32_t*)src)[offset/4+i*nc+c])
#endif

CAMLprim value caml_float_pcm_to_s32le(value a, value _offs, value _dst, value _dst_offs, value _len)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = 4 * len * nc;
  value src;
  int32_t *dst = (int32_t*)String_val(_dst);

  if (nc == 0) CAMLreturn(Val_int(0));

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_s32le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Field(a, c);
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = s32_clip(Double_field(src, i + offs));
#ifdef BIGENDIAN
      dst[i*nc+c] = bswap_32(dst[i*nc+c]);
#endif
    }
  }

  CAMLreturn(Val_int(dst_len));
}

CAMLprim value caml_float_pcm_to_s24le(value a, value _offs, value _dst, value _dst_offs, value _len)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = 3 * len * nc;
  value src;
  int24_t *dst = (int24_t*)String_val(_dst);

  if (nc == 0) CAMLreturn(Val_int(0));

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_s24le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Field(a, c);
    for (i = 0; i < len; i++)
      s24_clip(Double_field(src, i + offs), dst[i*nc+c]);
  }

  CAMLreturn(Val_int(dst_len));
}

CAMLprim value caml_float_pcm_to_s16(value a, value _offs, value _dst, value _dst_offs, value _len, int little_endian)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = 2 * len * nc;
  value src;
  int16_t *dst = (int16_t*)String_val(_dst);

  if (nc == 0) CAMLreturn(Val_int(0));

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_s16: destination buffer too short");

  if (little_endian == 1)
    for (c = 0; c < nc; c++)
    {
      src = Field(a, c);
      for (i = 0; i < len; i++)
      {
        dst[i*nc+c] = clip(Double_field(src, i + offs));
#ifdef BIGENDIAN
        dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
      }
    }
  else
    for (c = 0; c < nc; c++)
    {
      src = Field(a, c);
      for (i = 0; i < len; i++)
      {
        dst[i*nc+c] = clip(Double_field(src, i + offs));
#ifndef BIGENDIAN
        dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
    }
   }

  CAMLreturn(Val_int(dst_len));
}

CAMLprim value caml_float_pcm_to_s16le(value a, value _offs, value _dst, value _dst_offs, value _len)
{
  return caml_float_pcm_to_s16(a, _offs, _dst, _dst_offs, _len, 1);
}

CAMLprim value caml_float_pcm_to_s16be(value a, value _offs, value _dst, value _dst_offs, value _len)
{
  return caml_float_pcm_to_s16(a, _offs, _dst, _dst_offs, _len, 0);
}

CAMLprim value caml_float_pcm_to_u8(value a, value _offs,
                                    value _dst, value _dst_offs, value _len)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = len * nc;
  value src;
  uint8_t *dst = (uint8_t*)String_val(_dst);

  if (nc == 0) CAMLreturn(Val_int(0));

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_u8: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Field(a, c);
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = u8_clip(Double_field(src, i + offs));
    }
   }

  CAMLreturn(Val_int(dst_len));
}

CAMLprim value caml_float_pcm_of_u8_native(
    value _src, value _offset,
    value _dst, value _dst_off, value _length)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  int dst_off = Int_val(_dst_off) ;
  int nc = Wosize_val(_dst) ;
  int dst_len ;
  int i,c ;

  if (nc == 0) CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = Field(_dst,c) ;
    for (i=0 ; i<len; i++) {
      Store_double_field(dstc, dst_off+i, get_u8(src,offset,nc,c,i)) ;
    }
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_of_u8_byte(value* argv, int argn)
{
  return caml_float_pcm_of_u8_native(argv[0],argv[1],argv[2],argv[3],argv[4]);
}

CAMLprim value caml_float_pcm_convert_s32le_native(value _src, value _offset, value _dst, value _dst_off, value _length)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  int dst_off = Int_val(_dst_off) ;
  int nc = Wosize_val(_dst) ;
  int dst_len ;
  int i,c ;

  if (nc == 0) CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = Field(_dst,c) ;
    for (i=0 ; i<len; i++)
      Store_double_field(dstc, dst_off+i, get_s32le(src,offset,nc,c,i)) ;
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_convert_s32le_byte(value* argv, int argn)
{
  return caml_float_pcm_convert_s32le_native(argv[0],argv[1],argv[2],argv[3],argv[4]);
}

CAMLprim value caml_float_pcm_convert_s24le_native(value _src, value _offset, value _dst, value _dst_off, value _length)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  int dst_off = Int_val(_dst_off) ;
  int nc = Wosize_val(_dst) ;
  int dst_len ;
  int i,c ;

  if (nc == 0) CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = Field(_dst,c) ;
    for (i=0 ; i<len; i++)
      Store_double_field(dstc, dst_off+i, get_s24le(src,offset,nc,c,i)) ;
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_convert_s24le_byte(value* argv, int argn)
{
  return caml_float_pcm_convert_s24le_native(argv[0],argv[1],argv[2],argv[3],argv[4]);
}

CAMLprim value caml_float_pcm_convert_s16_native(value _src, value _offset, value _dst, value _dst_off, value _length, int little_endian)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  int dst_off = Int_val(_dst_off) ;
  int nc = Wosize_val(_dst) ;
  int dst_len ;
  int i,c ;

  if (nc == 0) CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (little_endian == 1)
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<len; i++)
        Store_double_field(dstc, dst_off+i, get_s16le(src,offset,nc,c,i)) ;
    }
  else
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<len; i++)
        Store_double_field(dstc, dst_off+i, get_s16be(src,offset,nc,c,i)) ;
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_convert_s16le_native(value _src, value _offset, value _dst, value _dst_off, value _length)
{
  return caml_float_pcm_convert_s16_native(_src, _offset, _dst, _dst_off, _length, 1);
}

CAMLprim value caml_float_pcm_convert_s16le_byte(value* argv, int argn)
{
  return caml_float_pcm_convert_s16le_native(argv[0],argv[1],argv[2],argv[3],argv[4]);
}

CAMLprim value caml_float_pcm_convert_s16be_native(value _src, value _offset, value _dst, value _dst_off, value _length)
{
  return caml_float_pcm_convert_s16_native(_src, _offset, _dst, _dst_off, _length, 0);
}

CAMLprim value caml_float_pcm_convert_s16be_byte(value* argv, int argn)
{
  return caml_float_pcm_convert_s16be_native(argv[0],argv[1],argv[2],argv[3],argv[4]);
}
