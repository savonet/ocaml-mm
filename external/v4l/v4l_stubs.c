#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <linux/videodev2.h>
#include <libv4l2.h>

#define CLEAR(x) memset(&(x), 0, sizeof(x))

static int xioctl(int fh, int request, void *arg)
{
  int r;

  do {
    r = ioctl(fh, request, arg);
  } while (-1 == r && EINTR == errno);

  return r;
}


CAMLprim value caml_v4l_open(value device, value w, value h, value stride)
{
  CAMLparam1(device);

  // TODO: error codes
  // TODO: flags
  int fd = v4l2_open(String_val(device),O_RDWR);

  // TODO: different formats ?
  struct v4l2_format  fmt;
  CLEAR(fmt);
  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.width        = Int_val(w);
  fmt.fmt.pix.height       = Int_val(h);
  fmt.fmt.pix.pixelformat  = V4L2_PIX_FMT_RGB24;
  fmt.fmt.pix.field        = V4L2_FIELD_INTERLACED;
  fmt.fmt.pix.bytesperline = Int_val(stride);
  xioctl(fd, VIDIOC_S_FMT, &fmt);
  // TODO: check returned sizes

  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_v4l_grab(value fd, value data)
{
  CAMLparam1(data);

  // TODO: error codes
  caml_enter_blocking_section();
  int ret = v4l2_read(Int_val(fd), Caml_ba_data_val(data), caml_ba_byte_size(Caml_ba_array_val(data)));
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_v4l_close(value fd)
{
  CAMLparam0();

  v4l2_close(Int_val(fd));

  CAMLreturn(Val_unit);
}
