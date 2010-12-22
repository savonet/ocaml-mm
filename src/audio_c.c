#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdint.h>
static inline int16_t bswap_16 (int16_t x) { return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)); }

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
#define get_u8(src,offset,nc,c,i)    u8tof(((uint8_t*)src)[offset+i*nc+c])
#define s16tof(x) (((double)x)/INT16_MAX)
#ifdef BIGENDIAN
#define get_s16le(src,offset,nc,c,i) s16tof(bswap_16(((int16_t*)src)[offset/2+i*nc+c]))
#else
#define get_s16le(src,offset,nc,c,i) s16tof(((int16_t*)src)[offset/2+i*nc+c])
#endif

CAMLprim value caml_float_pcm_to_s16le(value a, value _offs, value _dst, value _dst_offs, value _len)
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

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_16le: destination buffer too short");

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

  CAMLreturn(Val_int(dst_len));
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

CAMLprim value caml_float_pcm_of_u8_resample_native(
    value _src, value _offset, value _length,
    value _ratio, value _dst, value _dst_off)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  double ratio = Double_val(_ratio) ;
  int dst_off = Int_val(_dst_off) ;
  int dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;
  int newlen = (int)(ratio*len) ;
  int i,c ;
  int nc = Wosize_val(_dst) ;

  if (dst_off + newlen > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (ratio==1) {
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_u8(src,offset,nc,c,i)) ;
      }
    }
  }else{
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_u8(src,offset,nc,c,((int)(i/ratio)))) ;
      }
    }
  }

  CAMLreturn(Val_int(dst_off+newlen)) ;
}

CAMLprim value caml_float_pcm_of_u8_resample_byte(value* argv, int argn)
{
  return caml_float_pcm_of_u8_resample_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]);
}


CAMLprim value caml_float_pcm_convert_s16le_native(value _src, value _offset, value _length, value _ratio, value _dst, value _dst_off)
{
  CAMLparam2(_src, _dst) ;
  CAMLlocal1(dstc) ;
  char* src = String_val(_src) ;
  int offset = Int_val(_offset) ;
  int len = Int_val(_length) ;
  double ratio = Double_val(_ratio) ;
  int dst_off = Int_val(_dst_off) ;
  int dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize ;
  int newlen = (int)(ratio*len) ;
  int i,c ;
  int nc = Wosize_val(_dst) ;

  if (dst_off + newlen > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (ratio==1) {
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_s16le(src,offset,nc,c,i)) ;
      }
    }
  }else{
    for (c=0 ; c<nc ; c++) {
      dstc = Field(_dst,c) ;
      for (i=0 ; i<newlen; i++) {
        Store_double_field(dstc, dst_off+i, get_s16le(src,offset,nc,c,((int)(i/ratio)))) ;
      }
    }
  }

  CAMLreturn(Val_int(dst_off+newlen)) ;
}

CAMLprim value caml_float_pcm_convert_s16le_byte(value* argv, int argn)
{
  return caml_float_pcm_convert_s16le_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]);
}
