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
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <stdint.h>

#define INT24_MAX ((1 << 23) - 1)
#define INT24_MIN (-INT24_MAX)

#ifndef Bytes_val
#define Bytes_val String_val
#endif

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

CAMLprim value caml_float_pcm_to_s32le(value a, value _dst, value _offs)
{
  CAMLparam3(a, _dst, _offs);
  int c, i;
  int offs = Int_val(_offs);
  int nc = Wosize_val(a);
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(a, 0))->dim[0];
  float *src;
  int32_t *dst = (int32_t*)Bytes_val(_dst);

  if (caml_string_length(_dst) < offs + len*nc*4)
    caml_invalid_argument("pcm_to_s32le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Caml_ba_data_val(Field(a, c));
    caml_release_runtime_system();
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = s32_clip(src[i + offs]);
#ifdef BIGENDIAN
      dst[i*nc+c] = bswap_32(dst[i*nc+c]);
#endif
    }
    caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_to_s24le(value a, value _dst, value _offs)
{
  CAMLparam3(a, _dst, _offs);
  int c, i;
  int offs = Int_val(_offs);
  int nc = Wosize_val(a);
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(a, 0))->dim[0];
  float *src;
  int24_t *dst = (int24_t*)Bytes_val(_dst);

  if (caml_string_length(_dst) < offs + len*nc*3)
    caml_invalid_argument("pcm_to_s24le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Caml_ba_data_val(Field(a, c));
    caml_release_runtime_system();
    for (i = 0; i < len; i++)
      s24_clip(src[offs+i], dst[i*nc+c]);
    caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_to_s16(value _le, value a, value _dst, value _dst_offs)
{
  CAMLparam4(_le, a, _dst, _dst_offs);
  int little_endian = Bool_val(_le);
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(a);
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(a,0))->dim[0];
  float *src;
  int16_t *dst = (int16_t*)Bytes_val(_dst);
  int c, i;

  if (caml_string_length(_dst) < dst_offs + len*nc*2)
    caml_invalid_argument("pcm_to_s16: destination buffer too short");

  if (little_endian == 1)
    for (c = 0; c < nc; c++)
    {
      src = Caml_ba_data_val(Field(a, c));
      for (i = 0; i < len; i++)
      {
        dst[i*nc+c] = clip(src[i]);
#ifdef BIGENDIAN
        dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
      }
    }
  else
    for (c = 0; c < nc; c++)
    {
      src = Caml_ba_data_val(Field(a, c));
      for (i = 0; i < len; i++)
      {
        dst[i*nc+c] = clip(src[i]);
#ifndef BIGENDIAN
        dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
    }
   }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_convert_s16(value _le, value _src, value _offset, value _dst)
{
  CAMLparam4(_le, _src, _offset, _dst) ;
  int little_endian = Bool_val(_le);
  const char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int nc = Wosize_val(_dst) ;
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(_dst, 0))->dim[0];
  float *dstc;
  int i,c ;

  if ((offset + len)*nc*2 > caml_string_length(_src))
    caml_invalid_argument("convert_native: output buffer too small");

  if (little_endian == 1)
    for (c=0 ; c<nc ; c++) {
      dstc = (float*)Caml_ba_data_val(Field(_dst,c));
      caml_release_runtime_system();
      for (i=0 ; i<len; i++)
        dstc[i] = get_s16le(src,offset,nc,c,i);
      caml_acquire_runtime_system();
    }
  else
    for (c=0 ; c<nc ; c++) {
      dstc = (float*)Caml_ba_data_val(Field(_dst,c));
      caml_release_runtime_system();
      for (i=0 ; i<len; i++)
        dstc[i] = get_s16be(src,offset,nc,c,i);
      caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_to_u8(value a, value _dst, value _dst_offs)
{
  CAMLparam3(a, _dst, _dst_offs);
  int c, i;
  int dst_offs = Int_val(_dst_offs);
  int nc = Wosize_val(a);
  if (nc == 0)
    CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(a,0))->dim[0];
  int dst_len = len * nc;
  float *src;
  uint8_t *dst = (uint8_t*)Bytes_val(_dst);

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_u8: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = (float*)Caml_ba_data_val(Field(a, c));
    caml_release_runtime_system();
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = u8_clip(src[i]);
    }
    caml_acquire_runtime_system();
   }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_of_u8(value _src, value _offset, value _dst)
{
  CAMLparam3(_src, _offset, _dst);
  const char* src = String_val(_src);
  int offset = Int_val(_offset);
  int nc = Wosize_val(_dst);
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(_dst,0))->dim[0];
  assert(nc > 0);
  int i,c;
  float * dstc;

  if (len + offset > caml_string_length(_src))
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = (float*)Caml_ba_data_val(Field(_dst,c));
    caml_release_runtime_system();
    for (i=0 ; i<len; i++)
      dstc[i] = get_u8(src,offset,nc,c,i) ;
    caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_convert_s32le(value _src, value _offset, value _dst)
{
  CAMLparam3(_src, _offset, _dst) ;
  const char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int nc = Wosize_val(_dst) ;
  if (nc == 0) CAMLreturn(Val_unit);
  int len = Caml_ba_array_val(Field(_dst,0))->dim[0];
  int i,c ;
  float *dstc;

  if (caml_string_length(_src) < offset + len*nc*4)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = Caml_ba_data_val(Field(_dst,c));
    caml_release_runtime_system();
    for (i=0 ; i<len; i++)
      dstc[i] = get_s32le(src,offset,nc,c,i);
    caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit) ;
}

CAMLprim value caml_float_pcm_convert_s24le(value _src, value _offset, value _dst)
{
  CAMLparam3(_src, _offset, _dst) ;
  const char* src = String_val(_src) ;
  int nc = Wosize_val(_dst) ;
  if (nc == 0) CAMLreturn(Val_unit);
  int offset = Int_val(_offset) ;
  int len = Caml_ba_array_val(Field(_dst,0))->dim[0];
  int i,c ;
  float* dstc;

  if (caml_string_length(_src) < offset + len*nc*3)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c=0 ; c<nc ; c++) {
    dstc = Caml_ba_data_val(Field(_dst,c));
    caml_release_runtime_system();
    for (i=0 ; i<len; i++)
      dstc[i] = get_s24le(src,offset,nc,c,i);
    caml_acquire_runtime_system();
  }

  CAMLreturn(Val_unit) ;
}
