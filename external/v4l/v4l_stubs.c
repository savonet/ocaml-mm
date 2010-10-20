#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include <assert.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <linux/videodev.h>
#include <linux/videodev2.h>
#include <libv4l2.h>

#define CLEAR(x) memset(&(x), 0, sizeof(x))

static int xioctl(int fh, int request, void *arg)
{
  int r;

  do {
    r = ioctl(fh, request, arg);
  } while (r == -1 && ((errno == EINTR) || (errno == EAGAIN)));

  assert(r != -1);

  return r;
}


CAMLprim value caml_v4l2_open(value device, value w, value h, value stride)
{
  CAMLparam1(device);

  // TODO: error codes
  // TODO: flags
  int fd = v4l2_open(String_val(device), O_RDWR | O_NONBLOCK);
  assert(fd >= 0);

  // TODO: different formats ?
  struct v4l2_format  fmt;
  CLEAR(fmt);
  fmt.type                 = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.width        = Int_val(w);
  fmt.fmt.pix.height       = Int_val(h);
  fmt.fmt.pix.pixelformat  = V4L2_PIX_FMT_RGB24;
  fmt.fmt.pix.field        = V4L2_FIELD_INTERLACED;
  //fmt.fmt.pix.bytesperline = Int_val(stride);
  xioctl(fd, VIDIOC_S_FMT, &fmt);
  // TODO: check returned sizes
  assert(fmt.fmt.pix.pixelformat == V4L2_PIX_FMT_RGB24);

  CAMLreturn(Val_int(fd));
}

/*
CAMLprim value caml_v4l2_grab(value fd, value data)
{
  CAMLparam1(data);
  int len = caml_ba_byte_size(Caml_ba_array_val(data));

  // TODO: error codes
  caml_enter_blocking_section();
  int ret = v4l2_read(Int_val(fd), Caml_ba_data_val(data), len);
  caml_leave_blocking_section();
  if (ret < 0)
    printf("error: %d\n", errno);
  assert(ret == len);

  CAMLreturn(Val_unit);
}
*/

CAMLprim value caml_v4l2_grab(value _fd, value data)
{
  CAMLparam1(data);
  int fd = Int_val(_fd);
  int len = caml_ba_byte_size(Caml_ba_array_val(data));
  char *buf = Caml_ba_data_val(data);
  char *mbuf;
  int mbuflen;
  struct v4l2_buffer vbuf;
  struct v4l2_requestbuffers req;
  enum v4l2_buf_type type;
  struct timeval tv;
  fd_set fds;
  int ret;

  caml_enter_blocking_section();

  req.count = 1;
  req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_MMAP;
  xioctl(fd, VIDIOC_REQBUFS, &req);

  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  vbuf.index  = 0;
  xioctl(fd, VIDIOC_QUERYBUF, &vbuf);

  mbuflen = vbuf.length;
  mbuf = v4l2_mmap(NULL, mbuflen, PROT_READ | PROT_WRITE, MAP_SHARED, fd, vbuf.m.offset);
  assert(mbuf != MAP_FAILED);

  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  vbuf.index = 0;
  xioctl(fd, VIDIOC_QBUF, &vbuf);

  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  xioctl(fd, VIDIOC_STREAMON, &type);

  do {
    FD_ZERO(&fds);
    FD_SET(fd, &fds);

    /* Timeout. */
    tv.tv_sec = 2;
    tv.tv_usec = 0;

    ret = select(fd + 1, &fds, NULL, NULL, &tv);
  } while ((ret == -1 && (errno == EINTR)));
  assert(ret != -1);

  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  xioctl(fd, VIDIOC_DQBUF, &vbuf);

  memcpy(buf, mbuf, vbuf.bytesused);

  xioctl(fd, VIDIOC_QBUF, &vbuf);

  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  xioctl(fd, VIDIOC_STREAMOFF, &type);

  v4l2_munmap(mbuf, mbuflen);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_v4l2_close(value fd)
{
  CAMLparam0();

  v4l2_close(Int_val(fd));

  CAMLreturn(Val_unit);
}

CAMLprim value caml_v4l1_open(value device, value w, value h, value stride)
{
  CAMLparam1(device);
  int fd;
  struct video_capability cap;
  struct video_window win;
  struct video_picture vpic;

  fd = open(String_val(device), O_RDONLY);
  assert(fd >= 0);
  assert(ioctl(fd, VIDIOCGCAP, &cap) >= 0);
  assert(ioctl(fd, VIDIOCGWIN, &win) >= 0);
  assert(ioctl(fd, VIDIOCGPICT, &vpic) >= 0);

  if (cap.type & VID_TYPE_MONOCHROME) {
    vpic.depth=8;
    vpic.palette=VIDEO_PALETTE_GREY;    /* 8bit grey */
    if(ioctl(fd, VIDIOCSPICT, &vpic) < 0) {
      vpic.depth=6;
      if(ioctl(fd, VIDIOCSPICT, &vpic) < 0) {
        vpic.depth=4;
        if(ioctl(fd, VIDIOCSPICT, &vpic) < 0) {
          //fprintf(stderr, "Unable to find a supported capture format.\n");
          close(fd);
          assert(0);
        }
      }
    }
  }
  else {
    vpic.depth=24;
    vpic.palette=VIDEO_PALETTE_RGB24;

    if(ioctl(fd, VIDIOCSPICT, &vpic) < 0) {
      vpic.palette=VIDEO_PALETTE_RGB565;
      vpic.depth=16;

      if(ioctl(fd, VIDIOCSPICT, &vpic)==-1) {
        vpic.palette=VIDEO_PALETTE_RGB555;
        vpic.depth=15;

        if(ioctl(fd, VIDIOCSPICT, &vpic)==-1) {
          //fprintf(stderr, "Unable to find a supported capture format.\n");
          //return -1;
          close(fd);
          assert(0);
        }
      }
    }
  }
  assert(!(cap.type & VID_TYPE_MONOCHROME));
  assert(vpic.depth == 24);
  assert(vpic.palette == VIDEO_PALETTE_RGB24);

  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_v4l1_grab(value fd, value data)
{
  CAMLparam1(data);
  int len = caml_ba_byte_size(Caml_ba_array_val(data));
  int ret;

  caml_enter_blocking_section();
  ret = read(fd, Caml_ba_data_val(data), len);
  caml_leave_blocking_section();

  if (ret < 0)
    printf("error: %d\n", errno);
  assert(ret == len);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_v4l1_close(value fd)
{
  CAMLparam0();

  close(Int_val(fd));

  CAMLreturn(Val_unit);
}
