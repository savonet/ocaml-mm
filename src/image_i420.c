#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "image_pixel.h"
#include "image_rgb.h"

#define I420_data(v) (Caml_ba_data_val(Field(v,0)))
#define I420_width(v) (Int_val(Field(v,1)))
#define I420_height(v) (Int_val(Field(v,2)))
#define I420_alpha(v) (Is_block(Field(v,3))?Caml_ba_data_val(Field(Field(v,3),0)):NULL)

// studio swing
/* #define YofRGB(r,g,b) CLIP(((66 * r + 129 * g +  25 * b + 128) >> 8) +  16) */
/* #define UofRGB(r,g,b) CLIP(((-38 * r -  74 * g + 112 * b + 128) >> 8) + 128) */
/* #define UofRGB(r,g,b) CLIP(((112 * r -  94 * g -  18 * b + 128) >> 8) + 128) */
/* #define RofYUV(y,u,v) CLIP((298 * (y - 16) + 409 * (v - 128) + 128) >> 8) */
/* #define GofYUV(y,u,v) CLIP((298 * (y - 16) - 100 * (u - 128) - 208 * (v - 128) + 128) >> 8) */
/* #define BofYUV(y,u,v) CLIP((298 * (y - 16) + 516 * (u - 128) + 128) >> 8) */

// full swing
#define YofRGB(r,g,b) CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)
#define UofRGB(r,g,b) CLIP((36962 * (b - CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)) >> 16) + 128)
#define VofRGB(r,g,b) CLIP((46727 * (r - CLIP((19595 * r + 38470 * g + 7471 * b) >> 16)) >> 16) + 128)
#define RofYUV(y,u,v) CLIP(y + (91881 * v >> 16) - 179)
#define GofYUV(y,u,v) CLIP(y - ((22544 * u + 46793 * v) >> 16) + 135)
#define BofYUV(y,u,v) CLIP(y + (116129 * u >> 16) - 226)

#ifndef WIN32
#define max(a,b) (a>b)?a:b
#define min(a,b) (a<b)?a:b
#endif

CAMLprim value caml_yuv_of_rgb(value rgb)
{
  CAMLparam1(rgb);
  CAMLlocal1(ans);
  int r = Int_val(Field(rgb, 0));
  int g = Int_val(Field(rgb, 1));
  int b = Int_val(Field(rgb, 2));

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(YofRGB(r,g,b)));
  Store_field(ans, 1, Val_int(UofRGB(r,g,b)));
  Store_field(ans, 2, Val_int(VofRGB(r,g,b)));
  CAMLreturn(ans);
}

CAMLprim value caml_rgb_of_yuv(value yuv)
{
  CAMLparam1(yuv);
  CAMLlocal1(ans);
  int y = Int_val(Field(yuv, 0));
  int u = Int_val(Field(yuv, 1));
  int v = Int_val(Field(yuv, 2));

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, Val_int(RofYUV(y,u,v)));
  Store_field(ans, 1, Val_int(GofYUV(y,u,v)));
  Store_field(ans, 2, Val_int(BofYUV(y,u,v)));
  CAMLreturn(ans);
}

CAMLprim value caml_i420_fill(value img, value yuv)
{
  CAMLparam2(img, yuv);
  int y = Int_val(Field(yuv, 0));
  int u = Int_val(Field(yuv, 1));
  int v = Int_val(Field(yuv, 2));
  int width = I420_width(img);
  int height = I420_height(img);
  unsigned char *data = I420_data(img);
  int len = width*height;
  memset(data, y, len);
  memset(data+len, u, len/4);
  memset(data+len*5/4, v, len/4);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_i420_to_int_image(value img)
{
  CAMLparam1(img);
  CAMLlocal2(ans,tmp);
  int width = I420_width(img);
  int height = I420_height(img);
  unsigned char *data = I420_data(img);
  unsigned char *alpha = I420_alpha(img);
  int len = width*height;
  int i,j;
  int y,u,v,r,g,b,a;
  ans = caml_alloc_tuple(height);
  for (j = 0; j < height; j++)
    {
      tmp = caml_alloc_tuple(width);
      for (i = 0; i < width; i++)
        {
          y = data[j*width+i];
          u = data[len+(j/2)*(width/2)+i/2];
          v = data[len*5/4+(j/2)*(width/2)+i/2];
          r = RofYUV(y,u,v);
          g = GofYUV(y,u,v);
          b = BofYUV(y,u,v);
          if (alpha)
            {
              a = alpha[j*width+i];
              r = r*a/0xff;
              g = g*a/0xff;
              b = b*a/0xff;
            }
          Store_field(tmp, i, Val_int((r<<16)+(g<<8)+b));
        }
      Store_field(ans, j, tmp);
    }
  CAMLreturn(ans);
}

CAMLprim value caml_i420_of_rgba32(value _rgb, value img)
{
  CAMLparam1(_rgb);
  CAMLlocal1(ans);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int width = rgb.width;
  int height = rgb.height;
  long len = width*height;
  unsigned char *data = I420_data(img);
  unsigned char *alpha = I420_alpha(img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < height; j++)
    for (i = 0; i < width; i++)
      {
        int r = Red(&rgb,i,j);
        int g = Green(&rgb,i,j);
        int b = Blue(&rgb,i,j);
        int a = Alpha(&rgb,i,j);
        data[j*width+i] = YofRGB(r,g,b);
        // TODO: don't do u/v twice
        data[len+(j/2)*(width/2)+i/2] = UofRGB(r,g,b);
        data[len*5/4+(j/2)*(width/2)+i/2] = VofRGB(r,g,b);
        alpha[j*width+i] = a;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_i420_scale(value _src, value _dst)
{
  CAMLparam2(_src, _dst);
  int swidth = I420_width(_src);
  int sheight = I420_height(_src);
  unsigned char *src = I420_data(_src);
  unsigned char *salpha = I420_alpha(_src);
  int dwidth = I420_width(_dst);
  int dheight = I420_height(_dst);
  unsigned char *dst = I420_data(_dst);
  unsigned char *dalpha = I420_alpha(_dst);
  int slen = swidth*sheight;
  int dlen = dwidth*dheight;
  int i,j,is,js;

  assert(!salpha || dalpha);

  caml_enter_blocking_section();
  for (j = 0; j < dheight; j++)
    for (i = 0; i < dwidth; i++)
      {
        is = i*swidth/dwidth;
        js = j*sheight/dheight;
        dst[j*dwidth+i] = src[js*swidth+is];
        // TODO: don't do u/v twice
        dst[dlen+(j/2)*(dwidth/2)+i/2] = src[slen+(js/2)*(swidth/2)+is/2];
        dst[dlen*5/4+(j/2)*(dwidth/2)+i/2] = src[slen*5/4+(js/2)*(swidth/2)+is/2];
        if (salpha) dalpha[j*dwidth+i] = salpha[js*swidth+is];
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_i420_scale_coef(value src, value dst, value xscale, value yscale)
{
  CAMLparam4(src, dst, xscale, yscale);
  int src_width = I420_width(src);
  int src_height = I420_height(src);
  int src_len = src_width * src_height;
  unsigned char* src_data = I420_data(src);
  unsigned char* src_alpha = I420_alpha(src);
  int dst_width = I420_width(dst);
  int dst_height = I420_height(dst);
  int dst_len = dst_width * dst_height;
  unsigned char* dst_data = I420_data(dst);
  unsigned char* dst_alpha = I420_alpha(dst);
  // x scaling (xn: numerator, xd: denominator)
  int xn = Int_val(Field(xscale, 0));
  int xd = Int_val(Field(xscale, 1));
  // y scaling
  int yn = Int_val(Field(yscale, 0));
  int yd = Int_val(Field(yscale, 1));
  // offsets
  int ox = (dst_width - src_width * xn / xd) / 2;
  int oy = (dst_height - src_height * yn / yd) / 2;
  int i, j;

  assert(ox >= 0 && oy >= 0);

  caml_enter_blocking_section();
  // TODO: blank
  /* if (ox != 0 || oy != 0) rgb_blank(&dst); */
  for (j = oy; j < dst_height - oy; j++)
    for (i = ox; i < dst_width - ox; i++) {
      int is = (i-ox)*xd/xn;
      int js = (j-oy)*yd/yn;
      // TODO: don't do u/v twice
      dst_data[j*dst_width+i] = src_data[js*src_width+is];
      dst_data[dst_len+(j/2)*(dst_width/2)+(i/2)] = src_data[src_len+(js/2)*(src_width/2)+(is/2)];
      dst_data[dst_len*5/4+(j/2)*(dst_width/2)+(i/2)] = src_data[src_len*5/4+(js/2)*(src_width/2)+(is/2)];
      if (src_alpha)
        {
          dst_alpha[j*dst_width+i] = src_alpha[js*src_width+is];
        }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}


CAMLprim value caml_i420_add(value _src, value _x, value _y, value _dst)
{
  CAMLparam4(_src, _x, _y, _dst);
  int x = Int_val(_x);
  int y = Int_val(_y);
  int swidth = I420_width(_src);
  int sheight = I420_height(_src);
  unsigned char *src = I420_data(_src);
  unsigned char *salpha = I420_alpha(_src);
  int dwidth = I420_width(_dst);
  int dheight = I420_height(_dst);
  unsigned char *dst = I420_data(_dst);
  unsigned char *dalpha = I420_alpha(_dst);
  int slen = swidth*sheight;
  int dlen = dwidth*dheight;
  int ia = max(x,0);
  int ib = min(x+swidth,dwidth);
  int ja = max(y,0);
  int jb = min(y+sheight,dheight);
  int i,j;

  caml_enter_blocking_section();
  if (salpha == NULL)
    for (j = ja; j < jb; j++)
      for (i = ia; i < ib; i++)
        {
          int is = i-x;
          int js = j-y;
          dst[j*dwidth+i] = src[js*swidth+is];
          // TODO: don't do u/v twice
          dst[dlen+(j/2)*(dwidth/2)+i/2] = src[slen+(js/2)*(swidth/2)+is/2];
          dst[dlen*5/4+(j/2)*(dwidth/2)+i/2] = src[slen*5/4+(js/2)*(swidth/2)+is/2];
          if (dalpha != NULL) dalpha[j*dwidth+i] = 0xff;
        }
  else
    for (j = ja; j < jb; j++)
      for (i = ia; i < ib; i++)
        {
          int is = i-x;
          int js = j-y;
          int ys = src[js*swidth+is];
          int us = src[slen+(js/2)*(swidth/2)+is/2];
          int vs = src[slen*5/4+(js/2)*(swidth/2)+is/2];
          int a = salpha[js*swidth+is];
          int yd = dst[j*dwidth+i];
          int ud = dst[dlen+(j/2)*(dwidth/2)+i/2];
          int vd = dst[dlen*5/4+(j/2)*(dwidth/2)+i/2];
          dst[j*dwidth+i] = CLIP((ys * a + yd * (0xff - a)) / 0xff);
          // TODO: don't do u/v twice
          dst[dlen+(j/2)*(dwidth/2)+i/2] = CLIP((us * a + ud * (0xff - a)) / 0xff);
          dst[dlen*5/4+(j/2)*(dwidth/2)+i/2] = CLIP((vs * a + vd * (0xff - a)) / 0xff);
          if (dalpha != NULL) dalpha[j*dwidth+i] = 0xff-((0xff-a)*(0xff-dalpha[j*dwidth+i]))/0xff;
        }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_i420_get_pixel_rgba(value img, value _i, value _j)
{
  CAMLparam3(img, _i, _j);
  CAMLlocal1(ans);
  int width = I420_width(img);
  int height = I420_height(img);
  int len = width*height;
  unsigned char *data = I420_data(img);
  unsigned char *alpha = I420_alpha(img);
  int i = Int_val(_i);
  int j = Int_val(_j);

  int y = data[j*width+i];
  int u = data[len+(j/2)*(width/2)+i/2];
  int v = data[len*5/4+(j/2)*(width/2)+i/2];
  int a = alpha ? alpha[j*width+i] : 0xff;
  int r = RofYUV(y,u,v);
  int g = GofYUV(y,u,v);
  int b = BofYUV(y,u,v);

  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, Val_int(r));
  Store_field(ans, 1, Val_int(g));
  Store_field(ans, 2, Val_int(b));
  Store_field(ans, 3, Val_int(a));
  CAMLreturn(ans);
}

CAMLprim value caml_i420_set_pixel_rgba(value img, value _i, value _j, value c)
{
  CAMLparam4(img, _i, _j, c);
  int width = I420_width(img);
  int height = I420_height(img);
  int i = Int_val(_i);
  int j = Int_val(_j);
  int len = width*height;
  unsigned char *data = I420_data(img);
  unsigned char *alpha = I420_alpha(img);
  int r = Int_val(Field(c,0));
  int g = Int_val(Field(c,1));
  int b = Int_val(Field(c,2));
  int a = Int_val(Field(c,3));

  data[j*width+i] = YofRGB(r,g,b);
  data[len+(j/2)*(width/2)+i/2] = UofRGB(r,g,b);
  data[len*5/4+(j/2)*(width/2)+i/2] = VofRGB(r,g,b);
  if (alpha) alpha[j*width+i] = a;

  CAMLreturn(Val_unit);
}

CAMLprim value print_pointers(value img) {
  CAMLparam1(img);
  unsigned char *src = I420_data(img);
  unsigned char *alpha = I420_alpha(img);
  printf("POINTERS:\nsrc  : %p\nalpha: %p\n\n", src, alpha);
  CAMLreturn(Val_unit);
}
