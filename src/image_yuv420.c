#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "image_pixel.h"
#include "image_rgb.h"
#include "image_yuv420.h"

#define max(a,b) (a>b)?a:b
#define min(a,b) (a<b)?a:b
#define round(r,n) (((n+(r-1))/r)*r)

CAMLprim value caml_yuv420_fill(value img, value p)
{
  CAMLparam2(img, p);
  int y = Int_val(Field(p, 0));
  int u = Int_val(Field(p, 1));
  int v = Int_val(Field(p, 2));
  int height = YUV420_height(img);
  int y_stride = YUV420_y_stride(img);
  int uv_stride = YUV420_uv_stride(img);
  memset(YUV420_y(img), y, height*y_stride);
  memset(YUV420_u(img), u, round(2,height/2)*uv_stride);
  memset(YUV420_v(img), v, round(2,height/2)*uv_stride);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_to_int_image(value img)
{
  CAMLparam1(img);
  CAMLlocal2(ans,tmp);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i,j;
  int y,u,v,r,g,b,a;
  ans = caml_alloc_tuple(yuv.height);
  for (j = 0; j < yuv.height; j++)
    {
      tmp = caml_alloc_tuple(yuv.width);
      for (i = 0; i < yuv.width; i++)
        {
          y = Y(yuv,i,j);
          u = U(yuv,i,j);
          v = V(yuv,i,j);
          r = RofYUV(y,u,v);
          g = GofYUV(y,u,v);
          b = BofYUV(y,u,v);
          if (yuv.alpha)
            {
              a = A(yuv,i,j);
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

CAMLprim value caml_yuv420_of_rgb24_string(value img, value s)
{
  CAMLparam2(img,s);
  yuv420 yuv;
  yuv420_of_value(&yuv,img);
  // We don't copy so we cannot release the lock
  unsigned char *data = (unsigned char *)String_val(s);
  int i,j;

  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++)
      {
        int r = data[3*(j*yuv.width+i)+0];
        int g = data[3*(j*yuv.width+i)+1];
        int b = data[3*(j*yuv.width+i)+2];
        Y(yuv,i,j) = YofRGB(r,g,b);
        // TODO: don't do u/v twice
        U(yuv,i,j) = UofRGB(r,g,b);
        V(yuv,i,j) = VofRGB(r,g,b);
      }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_of_rgba32(value _rgb, value img)
{
  CAMLparam2(_rgb,img);
   frame rgb;
  frame_of_value(_rgb, &rgb);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      {
        int r = Red(&rgb,i,j);
        int g = Green(&rgb,i,j);
        int b = Blue(&rgb,i,j);
        Y(yuv,i,j) = YofRGB(r,g,b);
        // TODO: don't do u/v twice
        U(yuv,i,j) = UofRGB(r,g,b);
        V(yuv,i,j) = VofRGB(r,g,b);
        A(yuv,i,j) = Alpha(&rgb,i,j);
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_to_rgba32(value img, value _rgb)
{
  CAMLparam2(img,_rgb);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++)
      {
        int y = Y(yuv,i,j);
        int u = U(yuv,i,j);
        int v = V(yuv,i,j);
        Red(&rgb,i,j) = RofYUV(y,u,v);
        Green(&rgb,i,j) = GofYUV(y,u,v);
        Blue(&rgb,i,j) = BofYUV(y,u,v);
        Alpha(&rgb,i,j) = yuv.alpha ? A(yuv,i,j) : 0xff;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_scale(value _src, value _dst)
{
  CAMLparam2(_src, _dst);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  int i,j,is,js;

  assert(!src.alpha || dst.alpha);

  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++)
      {
        is = i*src.width/dst.width;
        js = j*src.height/dst.height;
        Y(dst,i,j) = Y(src,is,js);
        // TODO: don't do u/v twice
        U(dst,i,j) = U(src,is,js);
        V(dst,i,j) = V(src,is,js);
        if (src.alpha) A(dst,i,j) = A(src,is,js);
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_scale_coef(value _src, value _dst, value xscale, value yscale)
{
  CAMLparam4(_src, _dst, xscale, yscale);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  // x scaling (xn: numerator, xd: denominator)
  int xn = Int_val(Field(xscale, 0));
  int xd = Int_val(Field(xscale, 1));
  // y scaling
  int yn = Int_val(Field(yscale, 0));
  int yd = Int_val(Field(yscale, 1));
  // offsets
  int ox = (dst.width - src.width * xn / xd) / 2;
  int oy = (dst.height - src.height * yn / yd) / 2;
  int i, j;

  assert(ox >= 0 && oy >= 0);

  caml_enter_blocking_section();
  // TODO: blank
  /* if (ox != 0 || oy != 0) rgb_blank(&dst); */
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++) {
      int is = (i-ox)*xd/xn;
      int js = (j-oy)*yd/yn;
      Y(dst,i,j) = Y(src,is,js);
      // TODO: don't do u/v twice
      U(dst,i,j) = U(src,is,js);
      V(dst,i,j) = V(src,is,js);
      if (src.alpha) A(dst,i,j) = A(src,is,js);
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}


CAMLprim value caml_yuv420_add(value _src, value _x, value _y, value _dst)
{
  CAMLparam4(_src, _x, _y, _dst);
  int x = Int_val(_x);
  int y = Int_val(_y);
  yuv420 src, dst;
  yuv420_of_value(&src, _src);
  yuv420_of_value(&dst, _dst);
  int ia = max(x,0);
  int ib = min(x+src.width,dst.width);
  int ja = max(y,0);
  int jb = min(y+src.height,dst.height);
  int i,j;

  caml_enter_blocking_section();
  if (src.alpha == NULL)
    for (j = ja; j < jb; j++)
      for (i = ia; i < ib; i++)
        {
          int is = i-x;
          int js = j-y;
          Y(dst,i,j) = Y(src,is,js);
          // TODO: don't do u/v twice
          U(dst,i,j) = U(src,is,js);
          V(dst,i,j) = V(src,is,js);
          if (dst.alpha) A(dst,i,j) = 0xff;
        }
  else
    for (j = ja; j < jb; j++)
      for (i = ia; i < ib; i++)
        {
          int is = i-x;
          int js = j-y;
          int a = A(src,is,js);

          if (a == 0) {}
          else if (a == 0xff)
            {
              Y(dst,i,j) = Y(src,is,js);
              U(dst,i,j) = U(src,is,js);
              V(dst,i,j) = V(src,is,js);
              if (dst.alpha) A(dst,i,j) = 0xff;
            }
          else
            {
              Y(dst,i,j) = CLIP((Y(src,is,js) * a + Y(dst,i,j) * (0xff - a)) / 0xff);
              // TODO: don't do u/v twice
              U(dst,i,j) = CLIP((U(src,is,js) * a + U(dst,i,j) * (0xff - a)) / 0xff);
              V(dst,i,j) = CLIP((V(src,is,js) * a + V(dst,i,j) * (0xff - a)) / 0xff);
              if (dst.alpha) A(dst,i,j) = 0xff-((0xff-a)*(0xff-A(dst,i,j)))/0xff;
            }
        }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv420_get_pixel_rgba(value img, value _i, value _j)
{
  CAMLparam3(img, _i, _j);
  CAMLlocal1(ans);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i = Int_val(_i);
  int j = Int_val(_j);

  int y = Y(yuv,i,j);
  int u = U(yuv,i,j);
  int v = V(yuv,i,j);
  int a = yuv.alpha ? A(yuv,i,j) : 0xff;
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

CAMLprim value caml_yuv420_set_pixel_rgba(value img, value _i, value _j, value c)
{
  CAMLparam4(img, _i, _j, c);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i = Int_val(_i);
  int j = Int_val(_j);
  int r = Int_val(Field(c,0));
  int g = Int_val(Field(c,1));
  int b = Int_val(Field(c,2));
  int a = Int_val(Field(c,3));

  Y(yuv,i,j) = YofRGB(r,g,b);
  U(yuv,i,j) = UofRGB(r,g,b);
  V(yuv,i,j) = VofRGB(r,g,b);
  if (yuv.alpha) A(yuv,i,j) = a;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_randomize(value img)
{
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      Y(yuv,i,j) = rand();
      U(yuv,i,j) = rand();
      V(yuv,i,j) = rand();
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_greyscale(value img)
{
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv, img);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++) {
      /* int y = Y(yuv,i,j); */
      U(yuv,i,j) = 0x7f;
      V(yuv,i,j) = 0x7f;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

#define PIXEL_PRECISON 0x10000
CAMLprim value caml_yuv_scale_alpha(value img, value _a)
{
  CAMLparam2(img, _a);
  yuv420 yuv;
  yuv420_of_value(&yuv,img);
  int a = Double_val(_a) * PIXEL_PRECISON;
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++)
      A(yuv, i, j) = CLIP(A(yuv, i, j) * a / PIXEL_PRECISON);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_disk_alpha(value img, value _x, value _y, value _r)
{
  CAMLparam4(img, _x, _y, _r);
  yuv420 yuv;
  yuv420_of_value(&yuv,img);
  int x = Int_val(_x);
  int y = Int_val(_y);
  int radius = Int_val(_r);
  radius = radius * radius;
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < yuv.height; j++)
    for (i = 0; i < yuv.width; i++)
    {
      int r = (i - x) * (i - x) + (j - y) * (j - y);
      if (r > radius) A(yuv, i, j) = 0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

#define crop(x, m) (x > m ? m : (x < 0 ? 0 : x))

CAMLprim value caml_yuv_box_alpha_native(value img, value _x, value _y, value _w, value _h, value _a)
{
  CAMLparam1(img);
  yuv420 yuv;
  yuv420_of_value(&yuv,img);
  int x = crop(Int_val(_x), yuv.width);
  int y = crop(Int_val(_y), yuv.height);
  int w = crop(Int_val(_w), yuv.width);
  int h = max(Int_val(_h), yuv.height);
  int a = CLIP(Double_val(_a) * PIXEL_PRECISON);
  int i, j;

  caml_enter_blocking_section();
  for (j = y; j < h; j++)
    for (i = x; i < w; i++)
      A(yuv, i, j) = a;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_box_alpha_bytecode(value * argv, int argn) {
  return caml_yuv_box_alpha_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
