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
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#ifdef WIN32
#include <winsock2.h>
#else
#include <arpa/inet.h>
#endif

#include "config.h"

// MMX invert is broken at the moment..
#undef HAVE_MMX

#ifdef HAVE_MMX
#include <mmintrin.h>
#endif

// For OCaml < 3.10
#ifndef caml_ba_array
#define caml_ba_array caml_bigarray
#define Caml_ba_array_val(v) ((struct caml_ba_array *) Data_custom_val(v))
#define Caml_ba_data_val(v) (Caml_ba_array_val(v)->data)
#define caml_ba_alloc alloc_bigarray
#define CAML_BA_C_LAYOUT BIGARRAY_C_LAYOUT
#define CAML_BA_UINT8 BIGARRAY_UINT8
#define CAML_BA_MANAGED BIGARRAY_MANAGED
#endif

#include "image_c.h"

#ifndef WIN32
#define max(a,b) (a>b)?a:b
#define min(a,b) (a<b)?a:b
#endif

// Stolen from gavl,
// Stolen from a52dec..

#ifdef HAVE_MEMALIGN
/* some systems have memalign() but no declaration for it */
void * memalign(size_t align, size_t size);
#else
/* assume malloc alignment is sufficient */
#define memalign(align,size) malloc (size)
#endif

#define ALIGNMENT_BYTES 16
#define ALIGN(a) a=((a+ALIGNMENT_BYTES-1)/ALIGNMENT_BYTES)*ALIGNMENT_BYTES

/* This function creates a 16 bytes 
 * aligned plane. It returns a big array 
 * along with the new stride. */
CAMLprim value caml_rgb_aligned_plane(value _height, value _stride)
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
#ifdef HAVE_MEMALIGN
  data = memalign(ALIGNMENT_BYTES,len);
  if (data == NULL) caml_raise_out_of_memory();
  v = caml_ba_alloc(CAML_BA_MANAGED|CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,data,&len);
#else
  v = caml_ba_alloc(CAML_BA_C_LAYOUT|CAML_BA_UINT8,1,NULL,&len);
  data = Caml_ba_data_val(v);
#endif

  // We return
  ans = caml_alloc_tuple(2);
  Store_field(ans,0,Val_int(stride));
  Store_field(ans,1,v);
  CAMLreturn(ans);
}

/* Remark, this returns an integer, which means that it might be ordered in
   little-endian... */
static inline int int_rgb8_of_pixel(frame *rgb, int i, int j)
{
  int p = Int_pixel(rgb,i,j);
  /* Endianness... */
  p = ntohl(p);
  unsigned char a = p & 0xff;

  if (a == 0xff)
    return (p >> 8);
  else if (a == 0)
    return 0;
  else
    {
      /* TODO: why doesn't this work? */
      //return ((p >> 8) * a / 0xff);
      int r = (p >> 24) & 0xff;
      int g = (p >> 16) & 0xff;
      int b = (p >> 8) & 0xff;
      int c = ((r * a / 0xff) << 16) + ((g * a / 0xff) << 8) + (b * a / 0xff);
      return c;
    }
}

static void rgb_free(frame *f)
{
  free(f->data);
}

static inline void rgb_blank(frame *rgb)
{
  memset(rgb->data, 0, Rgb_data_size(rgb));
}

static frame *rgb_copy(frame *src, frame *dst)
{
  dst->width = src->width;
  dst->height = src->height;
  dst->stride = src->stride;
  dst->data = memalign(ALIGNMENT_BYTES, Rgb_data_size(src));
  if (!dst->data) caml_raise_out_of_memory();
  memcpy(dst->data, src->data, Rgb_data_size(src));

  return dst;
}

CAMLprim value caml_rgb_blank(value _rgb)
{
  frame rgb;

  rgb_blank(frame_of_value(_rgb,&rgb));

  return Val_unit;
}

CAMLprim value caml_rgb_blit(value _src, value _dst)
{
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  assert_same_dim(&src, &dst);
  memcpy(dst.data, src.data, Rgb_data_size(&src));

  return Val_unit;
}

CAMLprim value caml_rgb_blit_off(value _src, value _dst, value _dx, value _dy, value _blank)
{
  CAMLparam2(_src, _dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(_dx),
      dy = Int_val(_dy);
  int blank = Bool_val(_blank);
  int i, j, c;
  int istart = max(0, dx),
      iend = min(dst.width, src.width + dx),
      jstart = max(0, dy),
      jend = min(dst.height, src.height + dy);

  caml_enter_blocking_section();
  /* Blank what's outside src */
  if (blank)
  /*
    for (j = 0; j < dst.height; j++)
    {
      for (i = 0; i < dst.width; i++)
      {
        if (j < jend && j > jstart && i == istart)
        {
          if (iend == dst.width)
            break;
          else
            i = iend;
        }
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&dst, c, i, j) = 0;
      }
    }
  */
    /* This one seems to be much faster... */
    rgb_blank(&dst);
  /* Copy src to dst for the rest */
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i-dx), (j-dy));
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_blit_off_scale(value _src, value _dst, value d, value dim, value _blank)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(Field(d, 0)),
      dy = Int_val(Field(d, 1)),
      w = Int_val(Field(dim, 0)),
      h = Int_val(Field(dim, 1));
  int blank = Bool_val(_blank);
  int i, j, c;
  int istart = max(0, dx),
      iend = min(dst.width, w + dx),
      jstart = max(0, dy),
      jend = min(dst.height, h + dy);

  caml_enter_blocking_section();
  if (blank)
    rgb_blank(&dst);
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i-dx)*src.width/w, (j-dy)*src.height/h);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}


CAMLprim value caml_rgb_fill(value f, value col)
{
  CAMLparam1(f);
  frame rgb;
  frame_of_value(f,&rgb);
  int r = Int_val(Field(col, 0)),
      g = Int_val(Field(col, 1)),
      b = Int_val(Field(col, 2)),
      a = Int_val(Field(col, 3));
  int i,j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      Red(&rgb,i,j)   = r;
      Green(&rgb,i,j) = g;
      Blue(&rgb,i,j)  = b;
      Alpha(&rgb,i,j) = a;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

// TODO: Implements ASM version of these conversions,
// See:  http://svn.netlabs.org/repos/wvgui/trunk/yuv/

#define CLIP(color)  (unsigned char)(((color)>0xff)?0xff:(((color)<0)?0:(color)))
#define POS(x,o)     ((x << 1)+o)
#define PIX(p,s,x,y) p[y*s+x]

static inline void YUV420_to_RGB(unsigned char *ysrc, int y_stride, unsigned char *usrc,
                   unsigned char *vsrc, int uv_stride, frame *rgb)
{
/* From libv4l code. */
  int i,j;

  for (i = 0; i < rgb->height / 2; i++) {
    for (j = 0; j < rgb->width / 2; j++) {
      /* fast slightly less accurate multiplication free code */
      int u1 = (((PIX(usrc,uv_stride,j,i) - 128) << 7) +  (PIX(usrc,uv_stride,j,i) - 128)) >> 6;
      int rg = (((PIX(usrc,uv_stride,j,i) - 128) << 1) +  (PIX(usrc,uv_stride,j,i) - 128) +
                ((PIX(vsrc,uv_stride,j,i) - 128) << 2) + ((PIX(vsrc,uv_stride,j,i) - 128) << 1)) >> 3;
      int v1 = (((PIX(vsrc,uv_stride,j,i) - 128) << 1) +  (PIX(vsrc,uv_stride,j,i) - 128)) >> 1;

      Red(rgb,POS(j,0),POS(i,0))   = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) + v1);
      Green(rgb,POS(j,0),POS(i,0)) = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) - rg);
      Blue(rgb,POS(j,0),POS(i,0))  = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,0)) + u1);
      Alpha(rgb,POS(j,0),POS(i,0)) = 0xff;

      Red(rgb,POS(j,1),POS(i,0))   = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) + v1);
      Green(rgb,POS(j,1),POS(i,0)) = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) - rg);
      Blue(rgb,POS(j,1),POS(i,0))  = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,0)) + u1);
      Alpha(rgb,POS(j,1),POS(i,0)) = 0xff;

      Red(rgb,POS(j,0),POS(i,1))   = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) + v1);
      Green(rgb,POS(j,0),POS(i,1)) = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) - rg);
      Blue(rgb,POS(j,0),POS(i,1))  = CLIP(PIX(ysrc,y_stride,POS(j,0),POS(i,1)) + u1);
      Alpha(rgb,POS(j,0),POS(i,1)) = 0xff;

      Red(rgb,POS(j,1),POS(i,1))   = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) + v1);
      Green(rgb,POS(j,1),POS(i,1)) = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) - rg);
      Blue(rgb,POS(j,1),POS(i,1))  = CLIP(PIX(ysrc,y_stride,POS(j,1),POS(i,1)) + u1);
      Alpha(rgb,POS(j,1),POS(i,1)) = 0xff;

    }
  }
}

// TODO: implement multiplication-free version of
// this conversion, as well as ASM optimized ones..

/* From Kamelia's source code.
 * http://sourceforge.net/projects/kamaelia
 */
void RGB_to_YUV420(frame *rgb,
                   unsigned char *y_output,
                   unsigned char *u_output,
                   unsigned char *v_output)
{
    int R, G, B, A;
    int Y, U, V;

    int row;
    int col;

    int *uline;
    int *vline;
    int *ubuf;
    int *vbuf;

    int *ulineptr;
    int *vlineptr;
    int *ubufptr;
    int *vbufptr;

    int halfwidth;
    halfwidth = rgb->width>>1;

    // allocate temporary buffers for filtering U and V components to allow
    // sensible downsampling
    uline = vline = ubuf = vbuf = NULL;

    uline = (int *)calloc( rgb->width+2, sizeof(int) );
    vline = (int *)calloc( rgb->width+2, sizeof(int) );
    ubuf  = (int *)calloc( halfwidth*(rgb->height+2), sizeof(int) );
    vbuf  = (int *)calloc( halfwidth*(rgb->height+2), sizeof(int) );

    assert (uline && vline && ubuf && vbuf);

    // pre-pad buffers with default 'zero' values (128)
    uline[0] = uline[rgb->width+1] = 128;
    vline[0] = vline[rgb->width+1] = 128;
    for(col=0; col<halfwidth; col++)
    {
        ubuf[col] = ubuf[col + halfwidth*(rgb->height+1)] = 128;
        vbuf[col] = ubuf[col + halfwidth*(rgb->height+1)] = 128;
    }

    // offset base addresses
    uline = uline + 1;
    vline = vline + 1;
    ubuf = ubuf + halfwidth;
    vbuf = vbuf + halfwidth;

    ubufptr = ubuf;
    vbufptr = vbuf;
    for (row=0; row<rgb->height; row++)
    {
        ulineptr = uline;
        vlineptr = vline;

        for(col=0; col<rgb->width; col++)
        {
            // even numbered pixel
            R = (int)Red(rgb,col,row);
            G = (int)Green(rgb,col,row);
            B = (int)Blue(rgb,col,row);
            A = (int)Alpha(rgb,col,row);
            if (A != 0xff)
              {
                R = R * A / 0xff;
                G = G * A / 0xff;
                B = B * A / 0xff;
              }

            Y = (( 66*R + 129*G +  25*B + 128)>>8)+ 16;
            U = ((-38*R -  74*G + 112*B + 128)>>8)+128;
            V = ((112*R -  94*G -  18*B + 128)>>8)+128;

            *(y_output++) = (unsigned char)( (Y<0) ? 0 : ((Y>255) ? 255 : Y) );
            *(ulineptr++) = U;
            *(vlineptr++) = V;
        }

        for(col=0; col<rgb->width; col=col+2)
        {
            *(ubufptr++) = ( uline[col-1] + 2*uline[col] + uline[col+1] )>>2;
            *(vbufptr++) = ( vline[col-1] + 2*vline[col] + vline[col+1] )>>2;
        }
    }

    ubufptr = ubuf;
    vbufptr = vbuf;
    for (row=0; row<rgb->height; row=row+2)
    {
        for(col=0; col<halfwidth; col++)
        {
            U = ( ubufptr[-halfwidth] + 2*(*ubufptr) + ubufptr[+halfwidth] )>>2;
            V = ( vbufptr[-halfwidth] + 2*(*vbufptr) + vbufptr[+halfwidth] )>>2;

            *(u_output++) = (unsigned char)( (U<0) ? 0 : ((U>255) ? 255 : U) );
            *(v_output++) = (unsigned char)( (V<0) ? 0 : ((V>255) ? 255 : V) );

            ubufptr++;
            vbufptr++;
        }
        ubufptr += halfwidth;
        vbufptr += halfwidth;
    }

    uline = uline - 1;
    vline = vline - 1;
    ubuf = ubuf - halfwidth;
    vbuf = vbuf - halfwidth;

    free(uline);
    free(vline);
    free(ubuf);
    free(vbuf);
}

CAMLprim value caml_rgb_of_YUV420(value yuv, value dst)
{
  CAMLparam2(yuv,dst);
  frame rgb;
  frame_of_value(dst, &rgb);
  value y_val = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(y_val, 0));
  int y_stride = Int_val(Field(y_val, 1));
  value uv_val = Field(yuv, 1);
  unsigned char *u = Caml_ba_data_val(Field(uv_val, 0));
  unsigned char *v = Caml_ba_data_val(Field(uv_val, 1));
  int uv_stride = Int_val(Field(uv_val, 2));

  /* TODO: check the size of the data */
  caml_enter_blocking_section();
  YUV420_to_RGB(y, y_stride, u, v, uv_stride, &rgb);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_yuv_blank(value f)
{
  CAMLparam1(f);
  struct caml_ba_array *ba;

  ba = Caml_ba_array_val(f);
  memset(ba->data,0,ba->dim[0]);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_to_YUV420(value f, value yuv)
{
  CAMLparam2(f,yuv);
  frame rgb;
  frame_of_value(f, &rgb);
  value tmp = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(tmp,0));
  tmp = Field(yuv,1);
  unsigned char *u = Caml_ba_data_val(Field(tmp,0));
  unsigned char *v = Caml_ba_data_val(Field(tmp,1));

  caml_enter_blocking_section();
  RGB_to_YUV420(&rgb, y, u, v);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_RGBA8_to_Gray8(value _rgb, value _gray)
{
  CAMLparam2(_rgb,_gray);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  uint8 *gray = Caml_ba_data_val(_gray);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      gray[j*rgb.width+i] =
        (
         (int)Red(&rgb,i,j) +
         Green(&rgb,i,j) +
         Blue(&rgb,i,j)
        )/3;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_of_rgb8_string(value _rgb, value _data)
{
  CAMLparam2(_rgb, _data);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int datalen = rgb.height * rgb.width * 3;
  char *data = (char*)malloc(datalen);
  int i, j;

  if (!data) caml_raise_out_of_memory();

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      Red(&rgb,i,j) = data[3 * (j * rgb.width + i) + 0];
      Green(&rgb,i,j) = data[3 * (j * rgb.width + i) + 1];
      Blue(&rgb,i,j) = data[3 * (j * rgb.width + i) + 2];
      Alpha(&rgb,i,j) = 0xff;
    }
  caml_leave_blocking_section();

  memcpy(String_val(_data), data, datalen);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_get_pixel(value f, value _x, value _y)
{
  CAMLparam1(f);
  CAMLlocal1(ans);
  frame rgb;
  frame_of_value(f, &rgb);
  int x = Int_val(_x);
  int y = Int_val(_y);

  unsigned char pix[Rgb_elems_per_pixel] = Pixel(&rgb,x,y);
  int i;

  ans = caml_alloc_tuple(Rgb_elems_per_pixel);
  for (i = 0; i < Rgb_elems_per_pixel; i++)
    Store_field(ans, i, Val_int(pix[i]));

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_set_pixel(value f, value _x, value _y, value _rgb)
{
  frame rgb;
  frame_of_value(f, &rgb);
  int x = Int_val(_x),
      y = Int_val(_y);
  int r = Int_val(Field(_rgb, 0));
  int g = Int_val(Field(_rgb, 1));
  int b = Int_val(Field(_rgb, 2));
  int a = Int_val(Field(_rgb, 3));

  Red(&rgb,x,y) = r;
  Green(&rgb,x,y) = g;
  Blue(&rgb,x,y) = b;
  Alpha(&rgb,x,y) = a;

  return Val_unit;
}

CAMLprim value caml_rgb_randomize(value f)
{
  CAMLparam1(f);
  frame rgb;
  frame_of_value(f, &rgb);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++) {
      Alpha(&rgb,i,j) = 0xff;
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb,c,i,j) = rand();
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_scale(value _src, value _dst, value xscale, value yscale)
{
  CAMLparam4(_dst, _src, xscale, yscale);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);
  int i, j;
  int xn = Int_val(Field(xscale, 0)),
      xd = Int_val(Field(xscale, 1)),
      yn = Int_val(Field(yscale, 0)),
      yd = Int_val(Field(yscale, 1));
  int ox = (dst.width - src.width * xn / xd) / 2,
      oy = (dst.height - src.height * yn / yd) / 2;

  assert(ox >= 0 && oy >= 0);

  caml_enter_blocking_section();
  if (ox != 0 || oy != 0)
    rgb_blank(&dst);
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++)
      //for (c = 0; c < Rgb_elems_per_pixel; c++)
      //Color(&dst, c, i, j) = Color(&src, c, (i - ox) * xd / xn, (j - oy) * yd / yn);
      Copy_pixel(&dst,i,j,&src,(i-ox)*xd/xn,(j-oy)*yd/yn);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_bilinear_scale(value _src, value _dst, value xscale, value yscale)
{
  CAMLparam2(_src, _dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);
  int i, j, c, i2, j2;
  /* Scaling coefficients. */
  float ax = Double_val(xscale),
        ay = Double_val(yscale);
  /* Since the scaled image might not fill dst, we center it. These are the
     offsets of the scaled image in dst. */
  int ox = (dst.width - src.width * ax) / 2,
      oy = (dst.height - src.height * ay) / 2;
  float dx, dy;
  int p00, p01, p10, p11;

  assert(ox >= 0 && oy >= 0);

  caml_enter_blocking_section();
  /* TODO: only blank what is necessary. */
  if (ox != 0 || oy != 0)
    rgb_blank(&dst);
  for (j = oy; j < dst.height + oy; j++)
    for (i = ox; i < dst.width + ox; i++)
    {
      dx = (i - ox) / ax; // Corresponding pixel in src
      i2 = floor(dx);     // Nearest pixel on the left
      dx -= i2;           // Distance to the nearest pixel on the left
      dy = (j - oy) / ay;
      j2 = floor(dy);
      dy -= j2;
      if (i2+1 < src.width && j2+1 < src.height)
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          {
            p00 = Color(&src, c, i2  , j2  );
            p10 = Color(&src, c, i2+1, j2  );
            p01 = Color(&src, c, i2  , j2+1);
            p11 = Color(&src, c, i2+1, j2+1);
            Color(&dst,c,i,j) = CLIP(p00*(1-dx)*(1-dy) + p10*dx*(1-dy) + p01*(1-dx)*dy + p11*dx*dy);
          }
      else
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&dst,c,i,j) = (i2 < src.width && j2 < src.height)?Color(&src, c, i2, j2):0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

/*
CAMLprim value caml_rgb_scale(value _dst, value _src)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, i * src.width / dst.width, j * src.height / dst.height);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_proportional_scale(value _dst, value _src)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;
  int cn, cd, ox, oy;

  if (dst.height * src.width < src.height * dst.width)
  {
    cn = dst.height;
    cd = src.height;
    ox = (dst.width - src.width * cn / cd) / 2;
    oy = 0;
  }
  else
  {
    cn = dst.width;
    cd = src.width;
    ox = 0;
    oy = (dst.height - src.height * cn / cd) / 2;
  }

  caml_enter_blocking_section();
  rgb_blank(&dst);
  for (j = oy; j < dst.height - oy; j++)
    for (i = ox; i < dst.width - ox; i++)
      for (c = 0; c < Rgb_elems_per_pixel; c++)
        Color(&dst, c, i, j) = Color(&src, c, (i - ox) * cd / cn, (j - oy) * cd / cn);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
*/

static void bmp_pint32(char *dst, int n)
{
  dst[0] = n & 0xff;
  dst[1] = (n >> 8) & 0xff;
  dst[2] = (n >> 16) & 0xff;
  dst[3] = (n >> 24) & 0xff;
}

static void bmp_pint16(char *dst, int n)
{
  dst[0] = n & 0xff;
  dst[1] = (n >> 8) & 0xff;
}

/* See http://en.wikipedia.org/wiki/BMP_file_format */
CAMLprim value caml_rgb_to_bmp(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal1(ans);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int len = Rgb_num_pix(&rgb);
  char *bmp = malloc(54 + 3 * len);
  int i, j;
  unsigned char a;

  if (!bmp) caml_raise_out_of_memory();
  caml_enter_blocking_section();
  bmp[0]='B';                       /* Magic number */
  bmp[1]='M';
  bmp_pint32(bmp+2 , 54 + 3 * len); /* File size */
  bmp_pint16(bmp+6 , 0);            /* Reserved */
  bmp_pint16(bmp+8 , 0);            /* Reserved */
  bmp_pint32(bmp+10, 54);           /* Data offset */
  bmp_pint32(bmp+14, 40);           /* Second header size */
  bmp_pint32(bmp+18, rgb.width);   /* Width */
  bmp_pint32(bmp+22, rgb.height);  /* Height */
  bmp_pint16(bmp+26, 1);            /* Nb of color planes */
  bmp_pint16(bmp+28, 24);           /* BPP */
  bmp_pint32(bmp+30, 0);            /* Compression */
  bmp_pint32(bmp+34, 3 * len);      /* Image size */
  bmp_pint32(bmp+38, 2834);         /* Horizontal resolution */
  bmp_pint32(bmp+42, 2834);         /* Vertical resolution */
  bmp_pint32(bmp+46, 0);            /* Number of colors */
  bmp_pint32(bmp+50, 0);            /* Number of important colors */

  for(j = 0; j < rgb.height; j++)
    for(i = 0; i < rgb.width; i++)
    {
      a = Alpha(&rgb, i, j);
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 0 + 54] = Blue(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 1 + 54] = Green(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 2 + 54] = Red(&rgb, i, j) * a / 0xff;
    }
  caml_leave_blocking_section();

  ans = caml_alloc_string(54 + 3 * len);
  memcpy(String_val(ans), bmp, 54 + 3 * len);
  free(bmp);

  CAMLreturn(ans);
}

/* TODO: share code with to_bmp */
CAMLprim value caml_image_to_rgb24(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal1(ans);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int len = Rgb_num_pix(&rgb);
  char *bmp = malloc(3 * len);
  int i, j;
  unsigned char a;

  if (!bmp) caml_raise_out_of_memory();
  caml_enter_blocking_section();
  for(j = 0; j < rgb.height; j++)
    for(i = 0; i < rgb.width; i++)
    {
      a = Alpha(&rgb, i, j);
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 0] = Red(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 1] = Green(&rgb, i, j) * a / 0xff;
      bmp[3 * ((rgb.height - j - 1) * rgb.width + i) + 2] = Blue(&rgb, i, j) * a / 0xff;
    }
  caml_leave_blocking_section();

  ans = caml_alloc_string(3 * len);
  memcpy(String_val(ans), bmp, 3 * len);
  free(bmp);

  CAMLreturn(ans);
}

CAMLprim value caml_rgb_to_color_array(value _rgb)
{
  CAMLparam1(_rgb);
  CAMLlocal2(ans, line);
  frame rgb;
  frame_of_value(_rgb,&rgb);

  int i, j, c;
  //unsigned char a;

  ans = caml_alloc_tuple(rgb.height);
  for(j=0; j < rgb.height; j++)
  {
    line = caml_alloc_tuple(rgb.width);
    for(i=0; i < rgb.width; i++)
    {
      /*
      a = Alpha(&rgb, i, j);
      c = ((Red(&rgb,i,j) * a / 0xff) << 16)
        + ((Green(&rgb,i,j) * a / 0xff) << 8)
        + (Blue(&rgb,i,j) * a / 0xff);
      */
      c = int_rgb8_of_pixel(&rgb,i,j);
      Store_field(line, i, Val_int(c));
    }
    Store_field(ans, j, line);
  }

  CAMLreturn(ans);
}

CAMLprim value caml_mm_RGBA8_box_blur(value _rgb)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int i, j;

  caml_enter_blocking_section();
  for (j = 1; j < rgb.height - 1; j++)
    for (i = 1; i < rgb.width - 1; i++)
      {
        Red(&rgb,i,j) = (Red(&rgb,i-1,j) + Red(&rgb,i+1,j) + Red(&rgb,i,j-1) + Red(&rgb,i,j+1)) / 4;
        Green(&rgb,i,j) = (Green(&rgb,i-1,j) + Green(&rgb,i+1,j) + Green(&rgb,i,j-1) + Green(&rgb,i,j+1)) / 4;
        Blue(&rgb,i,j) = (Blue(&rgb,i-1,j) + Blue(&rgb,i+1,j) + Blue(&rgb,i,j-1) + Blue(&rgb,i,j+1)) / 4;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_greyscale(value _rgb, value _sepia)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);

  int sepia = Bool_val(_sepia);
  int i,j;
  unsigned char c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      c = (Red(&rgb,i,j)
        +  Green(&rgb,i,j)
        +  Blue(&rgb,i,j)) / 3;
      if (sepia)
      {
        Red(&rgb,i,j)   = c;
        Green(&rgb,i,j) = c * 201 / 0xff;
        Blue(&rgb,i,j)  = c * 158 / 0xff;
      }
      else
      {
        Red(&rgb,i,j)   = c;
        Green(&rgb,i,j) = c;
        Blue(&rgb,i,j)  = c;
      }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_add(value _dst, value _src)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int i, j, c;
  unsigned char sa;

  assert_same_dim(&src, &dst);
  caml_enter_blocking_section();
  for (j = 0; j < dst.height; j++)
    for (i = 0; i < dst.width; i++)
      {
      sa = Alpha(&src,i,j);
      if (sa == 0xff)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = Color(&src, c, i, j);
          Alpha(&dst, i, j) = 0xff;
        }
      else if (sa != 0)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = CLIP(Color(&src, c, i, j) * sa / 0xff + Color(&dst, c, i, j) * (0xff - sa) / 0xff);
          Alpha(&dst, i, j) = CLIP(sa + (0xff - sa) * Alpha(&dst, i, j));
        }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_add_off(value _src, value _dst, value _dx, value _dy)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(_dx),
      dy = Int_val(_dy);
  int i, j, c;
  unsigned char sa;
  int istart = max(0, dx),
      iend = min(dst.width, src.width + dx),
      jstart = max(0, dy),
      jend = min(dst.height, src.height + dy);

  caml_enter_blocking_section();
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
    {
      sa = Alpha(&src, (i-dx), (j-dy));
      if (sa == 0xff)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = Color(&src, c, (i-dx), (j-dy));
          Alpha(&dst, i, j) = 0xff;
        }
      else if (sa != 0)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = CLIP(Color(&src, c, (i-dx), (j-dy)) * sa / 0xff + Color(&dst, c, i, j) * (0xff - sa) / 0xff);
          Alpha(&dst, i, j) = CLIP(sa + (0xff - sa) * Alpha(&dst, i, j));
        }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_add_off_scale(value _src, value _dst, value d, value dim)
{
  CAMLparam2(_src,_dst);
  frame src,dst;
  frame_of_value(_src, &src);
  frame_of_value(_dst, &dst);

  int dx = Int_val(Field(d, 0)),
      dy = Int_val(Field(d, 1)),
      w = Int_val(Field(dim, 0)),
      h = Int_val(Field(dim, 1));
  int i, j, c;
  unsigned char sa;
  int istart = max(0, dx),
      iend = min(dst.width, w + dx),
      jstart = max(0, dy),
      jend = min(dst.height, h + dy);

  caml_enter_blocking_section();
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
    {
      sa = Alpha(&src, (i-dx)*src.width/w, (j-dy)*src.height/h);
      if (sa == 0xff)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = Color(&src, c, (i-dx)*src.width/w, (j-dy)*src.height/h);
          Alpha(&dst, i, j) = 0xff;
        }
      else if (sa != 0)
        {
          for (c = 0; c < Rgb_colors; c++)
            Color(&dst, c, i, j) = CLIP(Color(&src, c, (i-dx)*src.width/w, (j-dy)*src.height/h) * sa / 0xff + Color(&dst, c, i, j) * (0xff - sa) / 0xff);
          Alpha(&dst, i, j) = CLIP(sa + (0xff - sa) * Alpha(&dst, i, j));
        }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}


CAMLprim value caml_rgb_invert(value _rgb)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);

  caml_enter_blocking_section();
#ifdef HAVE_MMX
  /* See http://www.codeproject.com/KB/recipes/mmxintro.aspx?display=Print
   *     http://msdn.microsoft.com/en-us/library/698bxz2w(VS.80).aspx */
  unsigned char a1, a2;
  int i,j;
  __m64 *data = (__m64*)rgb.data;
  __m64 tmp;
  _mm_empty();
  __m64 f = _mm_set_pi8(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
  for (j = 0; j < rgb.height; j ++)
    for (i = 0; i < rgb.width/2; i++)
    {
      tmp = _mm_subs_pu8(f, *data);
      a1 = rgb.data[3];
      a2 = rgb.data[7];
      *data = tmp;
      rgb.data[3] = a1;
      rgb.data[7] = a2;
      if (2 * i == rgb.width - 1)
        data += rgb.stride - 4 * rgb.width + 1;
      else
        data += 8;
    }
  _mm_empty();
#else
  int i, j, c;
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb, c, i, j) = 0xff - Color(&rgb, c, i, j);
#endif
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

#define SO_PREC 0x10000
CAMLprim value caml_rgb_scale_opacity(value _rgb, value _x)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int x = Double_val(_x) * SO_PREC;
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      Alpha(&rgb, i, j) = CLIP(Alpha(&rgb, i, j) * x / SO_PREC);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_disk_opacity(value _rgb, value _x, value _y, value _r)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  int x = Int_val(_x);
  int y = Int_val(_y);
  int radius = Int_val(_r);
  int i, j, r;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      r = sqrt((double)(i - x) * (i - x) + (j - y) * (j - y));
      if (r > radius)
        Alpha(&rgb, i, j) = 0;
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_rotate(value _rgb, value _angle)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame old;
  rgb_copy(&rgb,&old);
  double a = Double_val(_angle);
  double sina = sin(a);
  double cosa = cos(a);
  int ox = rgb.width / 2,
      oy = rgb.height / 2;
  int i, j, c,
      i2, j2;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
    {
      i2 = (i - ox) * cosa + (j - oy) * sina + ox;
      j2 = -(i - ox) * sina + (j - oy) * cosa + oy;
      if (!Is_outside(&old, i2, j2))
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&rgb, c, i, j) = Color(&old, c, i2, j2);
      else
        Alpha(&rgb, i, j) = 0;
    }
  caml_leave_blocking_section();

  rgb_free(&old);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_affine(value _rgb, value _ax, value _ay, value _ox, value _oy)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame old;
  rgb_copy(&rgb,&old);
  double ax = Double_val(_ax),
         ay = Double_val(_ay);
  int i, j, i2, j2, c;
  int ox = Int_val(_ox),
      oy = Int_val(_oy);
  int dx = rgb.width / 2,  /* Center of scaling */
      dy = rgb.height / 2;
  int istart = max(0, (ox - dx) * ax + dx),
      iend = min(rgb.width, (rgb.width + ox + dx) * ax + dx),
      jstart = max(0, (oy - dy) * ay + dy),
      jend = min(rgb.height, (rgb.height + ox + dx) * ax + dx);

  caml_enter_blocking_section();
  rgb_blank(&rgb);
  for (j = jstart; j < jend; j++)
    for (i = istart; i < iend; i++)
    {
      i2 = (i - dx) / ax + dx - ox;
      j2 = (j - dy) / ay + dy - oy;
      /* TODO: this test shouldn't be needed */
      if (!Is_outside(&old, i2, j2))
        for (c = 0; c < Rgb_elems_per_pixel; c++)
          Color(&rgb, c, i, j) = Color(&old, c, i2, j2);
    }
  caml_leave_blocking_section();

  rgb_free(&old);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_mask(value _rgb, value _mask)
{
  CAMLparam2(_rgb,_mask);
  frame rgb;
  frame_of_value(_rgb,&rgb);
  frame mask;
  frame_of_value(_mask,&mask);
  int i, j;

  assert_same_dim(&rgb, &mask);
  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      Alpha(&rgb, i, j) =
        CLIP(sqrt(
              Red(&mask, i, j) * Red(&mask, i, j) +
              Green(&mask, i, j) * Green(&mask, i, j) +
              Blue(&mask, i, j) * Blue(&mask, i, j))) *
        Alpha(&mask, i, j) / 0xff;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_lomo(value _rgb)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int i, j, c;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      for (c = 0; c < Rgb_colors; c++)
        Color(&rgb, c, i, j) = CLIP((1 - cos(Color(&rgb, c, i, j) * 3.1416 / 255)) * 255);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_color_to_alpha_simple(value _rgb, value color, value _prec)
{
  CAMLparam2(_rgb, color);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int r = Int_val(Field(color, 0)),
      g = Int_val(Field(color, 1)),
      b = Int_val(Field(color, 2));
  int prec = Int_val(_prec);
  int i, j;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      if (abs(Red(&rgb, i, j) - r) <= prec && abs(Green(&rgb, i, j) - g) <= prec && abs(Blue(&rgb, i, j) - b) <= prec)
        Alpha(&rgb, i, j) = 0;
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_color_to_alpha(value _rgb, value color, value _prec, value _sharp)
{
  CAMLparam2(_rgb, color);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  int r = Int_val(Field(color, 0)),
      g = Int_val(Field(color, 1)),
      b = Int_val(Field(color, 2));
  float prec = Double_val(_prec);
  //float sharp = Double_val(_sharp);
  int i, j;
  double rr, gg, bb, aa;
  double d;

  caml_enter_blocking_section();
  for (j = 0; j < rgb.height; j++)
    for (i = 0; i < rgb.width; i++)
      {
        rr = Red(&rgb, i, j);
        gg = Green(&rgb, i, j);
        bb = Blue(&rgb, i, j);
        aa = Alpha(&rgb, i, j);
        d = sqrt((rr*rr+gg*gg+bb*bb)/(double)(0xff*0xff));
        /* We only change if we are in the radius */
        /* if(d <= prec)
           Alpha(&rgb,i,j) = (int)(0xff) */
        /* TODO */
        assert(0);

      if (abs(Red(&rgb, i, j) - r) <= prec && abs(Green(&rgb, i, j) - g) <= prec && abs(Blue(&rgb, i, j) - b) <= prec)
        Alpha(&rgb, i, j) = 0;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_blur_alpha(value _rgb)
{
  CAMLparam1(_rgb);
  frame rgb;
  frame_of_value(_rgb, &rgb);
  frame old;
  rgb_copy(&rgb,&old);
  int w = 1;
  int i, j, k, l;
  int a;

  caml_enter_blocking_section();
  for (j = w; j < rgb.height - w; j++)
    for (i = w; i < rgb.width - w; i++)
    {
      a = 0;
      for (l = -w; l <= w; l++)
        for (k = -w; k <= w; k++)
          a += Alpha(&old, i+k, j+l);
      Alpha(&rgb, i, j) = a / ((2*w+1)*(2*w+1));
    }
  rgb_free(&old);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

static inline int compare_images(int width, int height, uint8 *old, uint8 *new, int dx, int dy)
{
  int s = 0;
  int i, j;
  int adx = abs(dx);
  int ady = abs(dy);

  for(j = adx; j < height-adx; j++)
    for(i = ady; i < width-ady; i++)
      s += abs((int)new[j*width+i] - (int)old[(j-dy)*width+(i-dx)]);

  return s;
}

CAMLprim value caml_mm_Gray8_motion_compute(value _bs, value _width, value _old, value _new)
{
  CAMLparam2(_old, _new);
  // Block size
  int bs = Int_val(_bs);
  // Previous and current image
  int len = Caml_ba_array_val(_new)->dim[0];
  uint8 *old = Caml_ba_data_val(_old);
  uint8 *new = Caml_ba_data_val(_new);
  // Dimensions of the image
  int w = Int_val(_width);
  int h = len / w;
  // Offsets of blocks
  int dx, dy;
  // Iterators over offsets: radius, angle (parametrize a diamond)
  int dr, da;
  // Scores
  int s00, s10, s01, s11;
  // Best score
  int best;
  // Motion
  int mx = 0, my = 0;

  caml_enter_blocking_section();
  best = INT_MAX;
  for (dr = 0; dr <= bs; dr++)
    {
      if (best == 0)
        break;
      for (da = 0; da <= dr; da++)
        {
          if (best == 0)
            break;
          dx = da;
          dy = dr-da;
          // TODO: compute only once for dx = dy = 0
          s00 = compare_images(w, h, old, new, dx,  dy);
          s01 = compare_images(w, h, old, new, dx, -dy);
          s10 = compare_images(w, h, old, new, -dx,  dy);
          s11 = compare_images(w, h, old, new, -dx, -dy);

          if (s00 < best)
            {
              mx = dx;
              my = dy;
              best = s00;
            }
          if (s01 < best)
            {
              mx = dx;
              my = -dy;
              best = s01;
            }
          if (s10 < best)
            {
              mx = -dx;
              my = dy;
              best = s10;
            }
          if (s11 < best)
            {
              mx = -dx;
              my = -dy;
              best = s11;
            }
        }
    }
  caml_leave_blocking_section();

  CAMLlocal1(ans);
  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(mx));
  Store_field(ans, 1, Val_int(my));
  CAMLreturn(ans);
}

static inline int compare_blocks(int width, int height, uint8 *old, uint8 *new, int bs, int x, int y, int dx, int dy)
{
  int s = 0;
  int i, j;

  for(j = 0; j < bs; j++)
    for(i = 0; i < bs; i++)
      s += abs((int)new[(y+j)*width+(x+i)] - (int)old[(y+j-dy)*width+(x+i-dx)]);

  return s;
}

static inline void swap_int(int *x, int *y)
{
  int t;
  t = *x;
  *x = *y;
  *y = t;
}

CAMLprim value caml_mm_RGBA8_draw_line(value _img, value c, value src, value dst)
{
  CAMLparam1(_img);
  frame img;
  frame_of_value(_img,&img);
  int sx = Int_val(Field(src,0));
  int sy = Int_val(Field(src,1));
  int dx = Int_val(Field(dst,0));
  int dy = Int_val(Field(dst,1));
  uint8 cr = Int_val(Field(c,0));
  uint8 cg = Int_val(Field(c,1));
  uint8 cb = Int_val(Field(c,2));
  uint8 ca = Int_val(Field(c,3));

  int i, j;

  caml_enter_blocking_section();
  int steep = (abs(dy - sy) > abs(dx - sx));
  if (steep)
    {
      swap_int(&sx, &sy);
      swap_int(&dx, &dy);
    }
  if (sx > dx)
    {
      swap_int(&sx, &dx);
      swap_int(&sy, &dy);
    }

  int deltax = dx - sx;
  int deltay = abs(dy - sy);
  int error = deltax / 2;
  int ystep = (sy < dy)?1:-1;
  j = sy;
  for (i = sx; i < dx; i++)
    {
      if (steep)
        {
          Red(&img,j,i) = cr;
          Green(&img,j,i) = cg;
          Blue(&img,j,i) = cb;
          Alpha(&img,j,i) = ca;
        }
      else
        {
          Red(&img,i,j) = cr;
          Green(&img,i,j) = cg;
          Blue(&img,i,j) = cb;
          Alpha(&img,i,j) = ca;
        }
      error -= deltay;
      if (error < 0)
        {
          j += ystep;
          error += deltax;
        }
    }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_mm_Gray8_motion_multi_compute(value _bs, value _width, value _old, value _new)
{
  CAMLparam2(_old, _new);
  // Block size
  int bs = Int_val(_bs);
  // Previous and current image
  int len = Caml_ba_array_val(_new)->dim[0];
  uint8 *old = Caml_ba_data_val(_old);
  uint8 *new = Caml_ba_data_val(_new);
  // Iterators over blocks
  int i, j;
  // Dimensions of the image
  int w = Int_val(_width);
  int h = len / w;
  // Offsets of blocks
  int dx, dy;
  // Iterators over offsets: radius, angle (parametrize a diamond)
  int dr, da;
  // Vector table width and height
  int vw = w/bs;
  int vh = h/bs;
  // Size of vector table
  intnat vlen = vw*vh*2;
  // Vector table of size vw*vh
  int *v = malloc(vlen*sizeof(int));
  // Current score
  int s00, s10, s01, s11;
  // Best score
  int best;

  caml_enter_blocking_section();
  memset(v,0,vlen*sizeof(int));
  for (j = 1; j < vh-1; j++)
    for (i = 1; i < vw-1; i++)
      {
        best = INT_MAX;
        for (dr = 0; dr <= bs; dr++)
          {
            if (best == 0)
              break;
            for (da = 0; da <= dr; da++)
              {
                if (best == 0)
                  break;
                dx = da;
                dy = dr-da;
                s00 = compare_blocks(w, h, old, new, bs, i*bs, j*bs,  dx,  dy);
                s01 = compare_blocks(w, h, old, new, bs, i*bs, j*bs,  dx, -dy);
                s10 = compare_blocks(w, h, old, new, bs, i*bs, j*bs, -dx,  dy);
                s11 = compare_blocks(w, h, old, new, bs, i*bs, j*bs, -dx, -dy);

                if (s00 < best)
                  {
                    v[2*(j*vw+i)] = dx;
                    v[2*(j*vw+i)+1] = dy;
                    best = s00;
                  }
                if (s01 < best)
                  {
                    v[2*(j*vw+i)] = dx;
                    v[2*(j*vw+i)+1] = -dy;
                    best = s01;
                  }
                if (s10 < best)
                  {
                    v[2*(j*vw+i)] = -dx;
                    v[2*(j*vw+i)+1] = dy;
                    best = s10;
                  }
                if (s11 < best)
                  {
                    v[2*(j*vw+i)] = -dx;
                    v[2*(j*vw+i)+1] = -dy;
                    best = s11;
                  }
              }
          }
        //if (!best)
        //  printf("found %03d %03d: %03d %03d @ %d\n",i,j,v[2*(j*vw+i)],v[2*(j*vw+i)+1],best);
      }
  caml_leave_blocking_section();

  value ans = caml_ba_alloc(CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_NATIVE_INT, 1, v, &vlen);
  CAMLreturn(ans);
}

CAMLprim value caml_rgb_motion_multi_median_denoise(value _vw, value _v)
{
  CAMLparam1(_v);
  int *v = Caml_ba_data_val(_v);
  int len = Caml_ba_array_val(_v)->dim[0] / 2;
  int vw = Int_val(_vw);
  int vh = len / vw;
  int i, j, c;
  int *oldv;

  caml_enter_blocking_section();
  oldv = malloc(len * 2 * sizeof(int));
  memcpy(oldv, v, len * 2 * sizeof(int));
  for (j = 1; j < vh - 1; j++)
    for (i = 1; i < vw - 1; i++)
      for (c = 0; c < 2; c++)
        {
          v[2*(j*vw+i)+c] =
            (oldv[2*(j*vw+i)+c] +
             oldv[2*(j*vw+i-1)+c] +
             oldv[2*(j*vw+i+1)+c] +
             oldv[2*((j-1)*vw+i)+c] +
             oldv[2*((j+1)*vw+i)+c]) / 5;
        }
  free(oldv);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_motion_multi_mean(value _width, value _v)
{
  CAMLparam1(_v);
  CAMLlocal1(ans);
  int *v = Caml_ba_data_val(_v);
  int len = Caml_ba_array_val(_v)->dim[0] / 2;
  int w = Int_val(_width);
  int h = len / w;
  int mx = 0, my = 0;
  int i, j;

  caml_enter_blocking_section();
  for (j = 1; j < h-1; j++)
    for (i = 1; i < w-1; i++)
      {
        mx += v[2*(j*w+i)];
        my += v[2*(j*w+i)+1];
      }
  len = (w-2)*(h-2);
  mx += len/2;
  my += len/2;
  mx /= len;
  my /= len;
  caml_leave_blocking_section();

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(mx));
  Store_field(ans, 1, Val_int(my));
  CAMLreturn(ans);
}

static inline void motion_plot(frame *img, int i, int j)
{
  //Int_pixel(img, i, j) = 0xffffffff;
  Red(img, i, j) = 255;
  /*
  Green(img, i, j) = 255;
  Blue(img, i, j) = 255;
  */
}

static inline void motion_besenham(frame* img, int sx, int sy, int dx, int dy)
{
  int i, j;
  int steep = (abs(dy - sy) > abs(dx - sx));

  if (steep)
    {
      swap_int(&sx, &sy);
      swap_int(&dx, &dy);
    }
  if (sx > dx)
    {
      swap_int(&sx, &dx);
      swap_int(&sy, &dy);
    }

  int deltax = dx - sx;
  int deltay = abs(dy - sy);
  int error = deltax / 2;
  int ystep = (sy < dy)?1:-1;
  j = sy;
  for (i = sx; i < dx; i++)
    {
      if (steep)
        motion_plot(img, j, i);
      else
        motion_plot(img, i, j);
      error -= deltay;
      if (error < 0)
        {
          j += ystep;
          error += deltax;
        }
    }
}

CAMLprim value caml_rgb_motion_multi_arrows(value _bs, value _v, value _img)
{
  CAMLparam2(_v, _img);
  int bs = Int_val(_bs);
  frame img;
  frame_of_value(_img, &img);
  int i, j;
  int w = img.width;
  int h = img.height;
  int vw = w/bs;
  int vh = h/bs;
  int x, y;
  int dx, dy;
  int ax, ay;
  int *v = Caml_ba_data_val(_v);

  caml_enter_blocking_section();
  for (j = 0; j < vh-1; j++)
    for (i = 0; i < vw-1; i++)
      {
        x = i * bs + bs / 2;
        y = j * bs + bs / 2;
        dx = v[2*(j*vw+i)];
        dy = v[2*(j*vw+i+1)];
        ax = x+dx;
        ay = y+dy;
        /*
        ax = max(0,ax);
        ay = max(0,ay);
        ax = min(w,ax);
        ay = min(h,ay);
        */
        motion_besenham(&img, x, y, ax, ay);
        Green(&img,x,y)=0xff;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_RGBA32_to_BGR32(value _src, value _src_stride, value _dst, value _dst_stride, value dim)
{
  CAMLparam2(_src, _dst);
  unsigned char *src = Caml_ba_data_val(_src);
  unsigned char *dst = Caml_ba_data_val(_dst);
  int src_stride = Int_val(_src_stride);
  int dst_stride = Int_val(_dst_stride);
  int width = Int_val(Field(dim,0));
  int height = Int_val(Field(dim,1));
  int i,j;
  int a;

  caml_enter_blocking_section();
  for (j=0; j<height; j++)
    for (i=0; i<width; i++)
      {
        a = src[j*src_stride+i*4+3];
        if (a == 0xff)
          {
            dst[j*dst_stride+i*4+0] = src[j*src_stride+i*4+2];
            dst[j*dst_stride+i*4+1] = src[j*src_stride+i*4+1];
            dst[j*dst_stride+i*4+2] = src[j*src_stride+i*4+0];
          }
        else if (a == 0)
          {
            dst[j*dst_stride+i*4+0] = 0;
            dst[j*dst_stride+i*4+1] = 0;
            dst[j*dst_stride+i*4+2] = 0;
          }
        else
          {
            dst[j*dst_stride+i*4+0] = src[j*src_stride+i*4+2] * a / 0xff;
            dst[j*dst_stride+i*4+1] = src[j*src_stride+i*4+1] * a / 0xff;
            dst[j*dst_stride+i*4+2] = src[j*src_stride+i*4+0] * a / 0xff;
          }
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_RGB24_to_RGBA32(value _src, value _src_stride, value _dst, value _dst_stride, value dim)
{
  CAMLparam2(_src, _dst);
  unsigned char *src = Caml_ba_data_val(_src);
  unsigned char *dst = Caml_ba_data_val(_dst);
  int src_stride = Int_val(_src_stride);
  int dst_stride = Int_val(_dst_stride);
  int width = Int_val(Field(dim,0));
  int height = Int_val(Field(dim,1));
  int i,j;

  caml_enter_blocking_section();
  for (j=0; j<height; j++)
    for (i=0; i<width; i++)
      {
        dst[j*dst_stride+i*4+0]=src[j*src_stride+i*3+0];
        dst[j*dst_stride+i*4+1]=src[j*src_stride+i*3+1];
        dst[j*dst_stride+i*4+2]=src[j*src_stride+i*3+2];
        dst[j*dst_stride+i*4+3]=0xff;
      }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_RGB32_to_RGBA32(value _src, value _src_stride, value _dst, value _dst_stride, value dim)
{
  CAMLparam2(_src, _dst);
  unsigned char *src = Caml_ba_data_val(_src);
  unsigned char *dst = Caml_ba_data_val(_dst);
  int src_stride = Int_val(_src_stride);
  int dst_stride = Int_val(_dst_stride);
  int width = Int_val(Field(dim,0));
  int height = Int_val(Field(dim,1));
  int i,j;

  caml_enter_blocking_section();
  if (src_stride == dst_stride)
    {
      memcpy(dst, src, width*src_stride);
      for (j=0; j<height; j++)
        for (i=0; i<width; i++)
          dst[j*dst_stride+i*4+3]=0xff;
    }
  else
    for (j=0; j<height; j++)
      for (i=0; i<width; i++)
        {
          dst[j*dst_stride+i*4+0]=src[j*src_stride+i*4+0];
          dst[j*dst_stride+i*4+1]=src[j*src_stride+i*4+1];
          dst[j*dst_stride+i*4+2]=src[j*src_stride+i*4+2];
          dst[j*dst_stride+i*4+3]=0xff;
        }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
