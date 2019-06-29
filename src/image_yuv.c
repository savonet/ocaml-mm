#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <string.h>

#include "image_data.h"

CAMLprim value caml_yuv_of_string(value yuv, value s)
{
  CAMLparam2(yuv, s);
  CAMLlocal1(tmp);

  tmp = Field(yuv,0);
  int width = Int_val(Field(yuv,1));
  int height = Int_val(Field(yuv,2));
  unsigned char *y = Caml_ba_data_val(Field(Field(tmp,0),0));
  int y_stride = Int_val(Field(Field(tmp,0),1));
  tmp = Field(tmp,1);
  unsigned char *u = Caml_ba_data_val(Field(tmp,0));
  unsigned char *v = Caml_ba_data_val(Field(tmp,1));
  int uv_stride = Int_val(Field(tmp,2));

  int datalen = width*height*6/4;

  unsigned char *data = (unsigned char*)memalign(ALIGNMENT_BYTES, datalen);
  if (data == NULL) caml_raise_out_of_memory();
  memcpy(data, String_val(s), datalen);
  int i;

  caml_enter_blocking_section();
  for (i = 0; i < height; i++)
    memcpy(y+i*y_stride, data+i*width, width);
  for (i = 0; i < height/2; i++) {
    memcpy(u+i*uv_stride, data+datalen*4/6+i*width/2, width/2);
    memcpy(v+i*uv_stride, data+datalen*5/6+i*width/2, width/2);
  }
  caml_leave_blocking_section();

  free(data);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_blank(value f)
{
  CAMLparam1(f);
  struct caml_ba_array *ba;

  ba = Caml_ba_array_val(f);
  assert(0);
  // TODO: this is wrong, black is not zero
  memset(ba->data,0,ba->dim[0]);

  CAMLreturn(Val_unit);
}
