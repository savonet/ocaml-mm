/* Inspired of
 * http://dranger.com/ffmpeg/tutorial01.html
 * and

 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>

typedef struct {
  AVFormatContext *av_format_ctx;
  AVCodecContext *av_codec_ctx;
  AVCodec *av_codec;
  struct SwsContext *convert_ctx;
  int video_stream;
  AVFrame* av_frame;
  AVFrame* av_frame_rgb;
  uint8_t* buffer;
} ffmpeg_t;

#define FF_of_val(v) ((ffmpeg_t*)v)

CAMLprim value caml_ffmpeg_init(value unit)
{
  CAMLparam0();
  av_register_all();
  CAMLreturn(Val_unit);
}

/* TODO: add a finalizer!!!! */
CAMLprim value caml_ffmpeg_openfile(value fname)
{
  CAMLparam1(fname);
  ffmpeg_t *ffm = malloc(sizeof(ffmpeg_t));
  int i;
  int buflen;
  int width, height;

  /* Open the file */
  assert(av_open_input_file(&ffm->av_format_ctx, String_val(fname), NULL, 0, NULL) == 0);
  /* Retrieve stream information */
  assert(av_find_stream_info(ffm->av_format_ctx) >= 0);
  /* Dump info about the file on stderr */
  dump_format(ffm->av_format_ctx, 0, String_val(fname), 0);

  ffm->video_stream = -1;
  /* Find a video stream */
  for(i=0; i<ffm->av_format_ctx->nb_streams; i++)
    if(ffm->av_format_ctx->streams[i]->codec->codec_type==CODEC_TYPE_VIDEO) {
      ffm->video_stream = i;
      break;
    }
  assert(ffm->video_stream != -1);

  ffm->av_codec_ctx = ffm->av_format_ctx->streams[ffm->video_stream]->codec;
  /* Find a decoder */
  ffm->av_codec = avcodec_find_decoder(ffm->av_codec_ctx->codec_id);
  /* Is the codec supported? */
  assert(ffm->av_codec);
  /* Open the codec */
  assert(avcodec_open(ffm->av_codec_ctx, ffm->av_codec) >= 0);

  width = ffm->av_codec_ctx->width;
  height = ffm->av_codec_ctx->height;

  ffm->av_frame = avcodec_alloc_frame();
  ffm->av_frame_rgb = avcodec_alloc_frame();
  /* Allocate a suitable buffer */
  buflen = avpicture_get_size(PIX_FMT_RGB24, width, height);
  ffm->buffer = (uint8_t*)av_malloc(buflen * sizeof(uint8_t));
  /* Assign appropriate parts of buffer to image planes in av_frame_rgb */
  avpicture_fill((AVPicture*)ffm->av_frame_rgb, ffm->buffer, PIX_FMT_RGB24, width, height);
  /* Init conversion context */
  ffm->convert_ctx = sws_getContext(width, height, ffm->av_codec_ctx->pix_fmt, width, height, PIX_FMT_RGB24, SWS_BICUBIC, NULL, NULL, NULL);
  assert(ffm->convert_ctx);

  CAMLreturn((value)ffm);
}

CAMLprim value caml_ffmpeg_set_target_size(value _ffm, value _w, value _h)
{
  CAMLparam1(_ffm);
  ffmpeg_t* ffm = FF_of_val(_ffm);
  int w = Int_val(_w);
  int h = Int_val(_h);
  int width = ffm->av_codec_ctx->width;
  int height = ffm->av_codec_ctx->height;

  sws_freeContext(ffm->convert_ctx);
  ffm->convert_ctx = sws_getContext(width, height, ffm->av_codec_ctx->pix_fmt, w, h, PIX_FMT_RGB24, SWS_BICUBIC, NULL, NULL, NULL);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_ffmpeg_width(value ffm)
{
  CAMLparam1(ffm);
  CAMLreturn(Val_int(FF_of_val(ffm)->av_codec_ctx->width));
}

CAMLprim value caml_ffmpeg_height(value ffm)
{
  CAMLparam1(ffm);
  CAMLreturn(Val_int(FF_of_val(ffm)->av_codec_ctx->height));
}

CAMLprim value caml_ffmpeg_read_frame(value _ffm)
{
  CAMLparam1(_ffm);
  CAMLlocal1(ans);
  ffmpeg_t* ffm = FF_of_val(_ffm);
  AVPacket packet;
  int frame_finished;
  int width = ffm->av_codec_ctx->width;
  int height = ffm->av_codec_ctx->height;
  int ansbuflen = width*height*3;
  uint8_t *ansbuf = NULL;
  int j;

  caml_enter_blocking_section();
  while (av_read_frame(ffm->av_format_ctx, &packet) >= 0)
    {
      if(packet.stream_index == ffm->video_stream)
        {
          avcodec_decode_video(ffm->av_codec_ctx, ffm->av_frame, &frame_finished, packet.data, packet.size);
          if (frame_finished)
            {
              sws_scale(ffm->convert_ctx, (const uint8_t * const*)ffm->av_frame->data, ffm->av_frame->linesize, 0, height, ffm->av_frame_rgb->data, ffm->av_frame_rgb->linesize);
              ansbuf = malloc(ansbuflen);
              for (j = 0; j < height; j++)
                memcpy(ansbuf+j*width*3, ffm->av_frame_rgb->data[0]+j*ffm->av_frame_rgb->linesize[0], width*3);
              caml_leave_blocking_section();
              ans = caml_alloc_string(width*height*3);
              memcpy(String_val(ans), ansbuf, ansbuflen);
              free(ansbuf);
              CAMLreturn(ans);
            }
        }
      /* Free the packet allocated by av_read_frame */
      av_free_packet(&packet);
    }
  caml_leave_blocking_section();

  caml_raise_constant(*caml_named_value("ffmpeg_exn_end_of_stream"));
}

/* TODO: finalizer!!!! */
CAMLprim value caml_ffmpeg_close(value _ffm)
{
  CAMLparam1(_ffm);
  ffmpeg_t* ffm = FF_of_val(_ffm);

  sws_freeContext(ffm->convert_ctx);
  av_free(ffm->buffer);
  av_free(ffm->av_frame_rgb);
  av_free(ffm->av_frame);
  free(ffm);

  CAMLreturn(Val_unit);
}
