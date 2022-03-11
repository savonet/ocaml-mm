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

/* See https://www.kernel.org/doc/html/v4.12/media/uapi/v4l/v4l2grab.c.html */

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
#include <unistd.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <linux/videodev2.h>
#include <libv4l2.h>

#define CLEAR(x) memset(&(x), 0, sizeof(x))

static int xioctl(int fh, int request, void *arg)
{
  int r;

  do {
    r = v4l2_ioctl(fh, request, arg);
  } while (r == -1 && (errno == EINTR || errno == EAGAIN));

  return r;
}


CAMLprim value caml_v4l2_open(value device, value w, value h)
{
  CAMLparam1(device);

  int r;
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
  /* fmt.fmt.pix.pixelformat  = V4L2_PIX_FMT_YUV420; */
  fmt.fmt.pix.field        = V4L2_FIELD_INTERLACED;
  r = xioctl(fd, VIDIOC_S_FMT, &fmt);
  assert (r != -1);
  // TODO: check returned sizes
  assert(fmt.fmt.pix.pixelformat == V4L2_PIX_FMT_RGB24);
  assert(fmt.fmt.pix.width == Int_val(w));
  assert(fmt.fmt.pix.height == Int_val(h));

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
  int r;

  caml_enter_blocking_section();

  // Initiate memory mapping
  // TODO: support more than 1
  CLEAR(req);
  req.count = 1;
  req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_MMAP;
  r = xioctl(fd, VIDIOC_REQBUFS, &req);

  if (r == -1) caml_failwith(strerror(errno));

  // Mmap buffer
  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  vbuf.index  = 0;
  r = xioctl(fd, VIDIOC_QUERYBUF, &vbuf);
  assert (r != -1);

  mbuflen = vbuf.length;
  mbuf = v4l2_mmap(NULL, mbuflen, PROT_READ | PROT_WRITE, MAP_SHARED, fd, vbuf.m.offset);
  assert(mbuf != MAP_FAILED);

  // Enqueue buffer
  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  vbuf.index = 0;
  r = xioctl(fd, VIDIOC_QBUF, &vbuf);
  assert (r != -1);

  // Start streaming
  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  r = xioctl(fd, VIDIOC_STREAMON, &type);
  assert (r != -1);

  // Capture image
  do {
    FD_ZERO(&fds);
    FD_SET(fd, &fds);

    /* Timeout. */
    tv.tv_sec = 2;
    tv.tv_usec = 0;

    r = select(fd + 1, &fds, NULL, NULL, &tv);
  } while (r == -1 && errno == EINTR);
  assert(r != -1);

  memset(&vbuf, 0, sizeof(vbuf));
  vbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  vbuf.memory = V4L2_MEMORY_MMAP;
  r = xioctl(fd, VIDIOC_DQBUF, &vbuf);
  assert (r != -1);

  memcpy(buf, mbuf, vbuf.bytesused);

  // Stop capture
  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  r = xioctl(fd, VIDIOC_STREAMOFF, &type);
  assert (r != -1);

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
