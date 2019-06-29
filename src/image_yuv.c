#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <string.h>

#define Y_val(yuv) Caml_ba_data_val(Field(yuv,0))
#define U_val(yuv) Caml_ba_data_val(Field(yuv,1))
#define V_val(yuv) Caml_ba_data_val(Field(yuv,2))
#define Ystride_val(yuv) Int_val(Field(yuv,3))
#define UVstride_val(yuv) Int_val(Field(yuv,4))
#define A_val(yuv) (Is_block(Field(yuv,5))?Caml_ba_data_val(Field(Field(yuv,5),0)):NULL)
#define Width_val(yuv) Int_val(Field(yuv,6))
#define Height_val(yuv) Int_val(Field(yuv 7))

/*
// TODO: Implements ASM version of these conversions,
// See:  http://svn.netlabs.org/repos/wvgui/trunk/yuv/

#define POS(x,o)     ((x << 1)+o)
#define PIX(p,s,x,y) p[y*s+x]

static inline void YUV420_to_RGB(unsigned char *ysrc, int y_stride, unsigned char *usrc,
                   unsigned char *vsrc, int uv_stride, frame *rgb)
{
// From libv4l code.
  int i,j;

  for (i = 0; i < rgb->height / 2; i++) {
    for (j = 0; j < rgb->width / 2; j++) {
      // fast slightly less accurate multiplication free code
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

// From Kamelia's source code.
// http://sourceforge.net/projects/kamaelia
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
  CAMLlocal2(y_val,uv_val);
  frame rgb;
  frame_of_value(dst, &rgb);
  y_val = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(y_val, 0));
  int y_stride = Int_val(Field(y_val, 1));
  uv_val = Field(yuv, 1);
  unsigned char *u = Caml_ba_data_val(Field(uv_val, 0));
  unsigned char *v = Caml_ba_data_val(Field(uv_val, 1));
  int uv_stride = Int_val(Field(uv_val, 2));

  // TODO: check the size of the data
  caml_enter_blocking_section();
  YUV420_to_RGB(y, y_stride, u, v, uv_stride, &rgb);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_rgb_to_YUV420(value f, value yuv)
{
  CAMLparam2(f,yuv);
  CAMLlocal1(tmp);
  frame rgb;
  frame_of_value(f, &rgb);
  tmp = Field(yuv, 0);
  unsigned char *y = Caml_ba_data_val(Field(tmp,0));
  tmp = Field(yuv,1);
  unsigned char *u = Caml_ba_data_val(Field(tmp,0));
  unsigned char *v = Caml_ba_data_val(Field(tmp,1));

  caml_enter_blocking_section();
  RGB_to_YUV420(&rgb, y, u, v);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}
*/
