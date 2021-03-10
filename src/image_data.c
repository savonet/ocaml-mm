#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdlib.h>
#include <string.h>

#include "image_data.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

CAMLprim value caml_data_aligned(value _alignment, value _len)
{
  CAMLparam2(_alignment, _len);
  CAMLlocal1(ans);
  int alignment = Int_val(_alignment);
  int len = Int_val(_len);
  unsigned char * data;

  data = memalign(alignment, len);
  if (data == NULL) caml_raise_out_of_memory();
  ans = caml_ba_alloc_dims(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,data,len);

  CAMLreturn(ans);
}

CAMLprim value caml_data_of_string(value s)
{
  CAMLparam1(s);
  CAMLlocal1(ans);
  long len = caml_string_length(s);
  unsigned char* data = malloc(len);
  memcpy(data, String_val(s), len);
  ans = caml_ba_alloc_dims(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8, 1, data, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_to_string(value _data)
{
  CAMLparam1(_data);
  CAMLlocal1(ans);
  unsigned char* data = Caml_ba_data_val(_data);
  long len = Caml_ba_array_val(_data)->dim[0];
  ans = caml_alloc_string(len);
  memcpy(Bytes_val(ans), data, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_copy(value _src)
{
  CAMLparam1(_src);
  CAMLlocal1(ans);
  unsigned char* src = Caml_ba_data_val(_src);
  long len = Caml_ba_array_val(_src)->dim[0];
  unsigned char* dst = malloc(len);
  memcpy(dst, src, len);
  ans = caml_ba_alloc_dims(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8, 1, dst, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_blit_off(value _src, value _soff, value _dst, value _doff, value _len)
{
  CAMLparam5(_src, _soff, _dst, _doff, _len);
  int soff = Int_val(_soff);
  int doff = Int_val(_doff);
  int len = Int_val(_len);
  unsigned char* src = Caml_ba_data_val(_src);
  unsigned char* dst = Caml_ba_data_val(_dst);
  memcpy(dst+doff, src+soff, len);
  CAMLreturn(Val_unit);
}
