#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <limits.h>
#include <stdint.h>
static inline int16_t bswap_16 (int16_t x) { return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)); }

#include <assert.h>
#include <stdio.h>
#include <string.h>

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

static inline short clip(double s)
{
  if (s < -1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return SHRT_MIN;
  }
  else if (s > 1)
  {
#ifdef DEBUG
    printf("Wrong sample: %f\n", s);
#endif
    return SHRT_MAX;
  }
  else
    return (s * (SHRT_MAX - 1));
}

CAMLprim value caml_float_pcm_to_16le(value a, value _offs, value _len, value _dst, value _dst_offs)
{
  CAMLparam2(a, _dst);
  int c, i;
  int offs = Int_val(_offs);
  int dst_offs = Int_val(_dst_offs);
  int len = Int_val(_len);
  int nc = Wosize_val(a);
  int dst_len = 2 * len * nc;
  value src;
  short *dst = (short*)String_val(_dst);

  if (caml_string_length(_dst) < dst_offs + dst_len)
    caml_invalid_argument("pcm_to_16le: destination buffer too short");

  for (c = 0; c < nc; c++)
  {
    src = Field(a, c);
    for (i = 0; i < len; i++)
    {
      dst[i*nc+c] = clip(Double_field(src, i + offs));
#ifdef LIQ_BIG_ENDIAN
      dst[i*nc+c] = bswap_16(dst[i*nc+c]);
#endif
    }
   }

  CAMLreturn(Val_int(dst_len));
}

CAMLprim value caml_float_pcm_from_16le(value _buf, value _boffs, value a, value _aoffs, value _len)
{
  CAMLparam2(a, _buf);
  CAMLlocal1(cbuf);
  short *buf = (short*)String_val(_buf);
  int aoffs = Int_val(_aoffs);
  int boffs = Int_val(_boffs);
  int len = Int_val(_len);
  int i, c;
  int chans = Wosize_val(a);

  if (Wosize_val(Field(a, 0)) / Double_wosize - boffs < len)
    caml_invalid_argument("from_s16le: buffer too small");

  for(c = 0; c < chans; c++)
  {
    cbuf = Field(a, c);
      for(i = 0; i < len; i++)
        Store_double_field(cbuf, i + aoffs, ((double)buf[chans*i+c+boffs])/32768);
  }

  CAMLreturn(Val_unit);
}
