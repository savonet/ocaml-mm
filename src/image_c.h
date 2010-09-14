#include <caml/bigarray.h>

typedef struct
{
  int width;  /* Width in pixels */
  int height; /* Height in pixels */
  int stride; /* Bytes per line */
  unsigned char *data;
} frame;

#define Rgb_num_pix(rgb)    (rgb)->width * (rgb)->height
#define Rgb_colors          3
#define Rgb_elems_per_pixel 4
#define Rgb_num_elem(rgb)   Rgb_elems_per_pixel * Rgb_num_pix(rgb)
#define Rgb_plane_size(rgb) (rgb)->stride * (rgb)->height
#define Rgb_data_size(rgb)  Rgb_plane_size(rgb) * sizeof(unsigned char)
#define Color(rgb,c,i,j)    (rgb)->data[j * (rgb)->stride + Rgb_elems_per_pixel * (i) + c]
#define Red(rgb,i,j)        Color(rgb,0,i,j)
#define Green(rgb,i,j)      Color(rgb,1,i,j)
#define Blue(rgb,i,j)       Color(rgb,2,i,j)
#define Alpha(rgb,i,j)      Color(rgb,3,i,j)
#define Pixel(rgb,i,j)      {Red(rgb,i,j),Green(rgb,i,j),Blue(rgb,i,j),Alpha(rgb,i,j)}
#define Is_outside(rgb,i,j) (i<0||j<0||i>=(rgb)->width||j>=(rgb)->height)
#define Space_clip_color(rgb,c,i,j) (Is_outside(rgb,i,j))?0:Color(rgb,c,i,j)
//For copying pixel by pixel
#define Int_pixel(rgb,i,j)  (((uint32*)(rgb)->data)+i+j*(rgb)->width)
#define Copy_pixel(dst,di,dj,src,si,sj) (*Int_pixel(dst,di,dj)=*Int_pixel(src,si,sj))

#define assert_same_dim(src, dst) { assert((dst)->width == (src)->width); assert((dst)->height == (src)->height); }

static frame *frame_of_value(value v, frame *f)
{
  value ba = Field(v,0);
  f->data = Caml_ba_data_val(ba);
  f->width = Int_val(Field(v,1));
  f->height = Int_val(Field(v,2));
  f->stride = Int_val(Field(v,3));

  return f;
}
