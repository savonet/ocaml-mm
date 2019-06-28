#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <malloc.h>
#include <string.h>

#include "image_data.h"

/*
CAMLprim value caml_data_alloc(value _len)
{
  CAMLparam1(_len);
  CAMLlocal1(ans);
  long len = Int_val(_len);
  void *data = malloc(len);
  if (data == NULL) caml_raise_out_of_memory();
  ans = caml_ba_alloc_dims(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,data,len);
  CAMLreturn(ans);
}
*/

#ifdef HAVE_MEMALIGN
/* some systems have memalign() but no declaration for it */
void * memalign(size_t align, size_t size);
#else
/* assume malloc alignment is sufficient */
#define memalign(align,size) malloc (size)
#endif

#define ALIGN(a) a=((a+ALIGNMENT_BYTES-1)/ALIGNMENT_BYTES)*ALIGNMENT_BYTES

/* This function creates a 16 bytes aligned plane. It returns a big array along
  with the new stride. */
CAMLprim value caml_data_aligned_plane(value _height, value _stride)
{
  CAMLparam0();
  CAMLlocal2(v,ans);
  long height = Long_val(_height);
  long stride = Long_val(_stride);

  // round up values..
  ALIGN(stride);
  long len = height*stride;  

  // Init plane..
  void *data;
  data = memalign(ALIGNMENT_BYTES,len);
  if (data == NULL) caml_raise_out_of_memory();
  v = caml_ba_alloc_dims(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,data,len);

  // We return
  ans = caml_alloc_tuple(2);
  Store_field(ans,0,Val_int(stride));
  Store_field(ans,1,v);
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
