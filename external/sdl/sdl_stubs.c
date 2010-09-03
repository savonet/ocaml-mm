#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>

#include "image_c.h"

CAMLprim value caml_sdl_rgb_to32(value _rgb, value _surf, value shift)
{
  CAMLparam3(_rgb, _surf, shift);
  int sr = Int_val(Field(shift, 0));
  int sg = Int_val(Field(shift, 1));
  int sb = Int_val(Field(shift, 2));
  frame rgb;
  frame_of_value(_rgb, &rgb);
  uint32 *surf = Caml_ba_data_val(_surf);
  int i, j;
  int w = rgb.width;
  int h = rgb.height;

  for (j = 0; j < h; j++)
    for (i = 0; i < w; i++)
      surf[j*w+i] = htonl(*Int_pixel(&rgb,i,j)) >> 8;

  CAMLreturn(Val_unit);
}
