let rw channels samplerate ?(device="default") ?(playback=false) ?(capture=false) ?(blocking=true) ?(buffer_size=1024) ?(periods=4) () =
object (self)
  (* inherit Audio.IO.rw_bufferized *)

  method version = Alsa.get_version ()

  val dev =
    Alsa.Pcm.open_pcm
      device
      ((if playback then [Alsa.Pcm.Playback] else [])@
       (if capture then [Alsa.Pcm.Capture] else []))
      []

  method delay = Alsa.Pcm.get_delay dev

  method prepare = Alsa.Pcm.prepare dev

  method wait t = Alsa.Pcm.wait dev t

  method recover e = Alsa.Pcm.recover dev e

  val mutable buffer_size = buffer_size

  initializer
  let params = Alsa.Pcm.get_params dev in
    Alsa.Pcm.set_access dev params Alsa.Pcm.Access_rw_noninterleaved;
    Alsa.Pcm.set_format dev params Alsa.Pcm.Format_float;
    Alsa.Pcm.set_channels dev params channels;
    Alsa.Pcm.set_periods dev params periods Alsa.Dir_eq;
    assert (Alsa.Pcm.set_rate_near dev params samplerate Alsa.Dir_eq = samplerate);
    buffer_size <- Alsa.Pcm.set_buffer_size_near dev params buffer_size;
    Alsa.Pcm.set_params dev params;
    Alsa.Pcm.set_nonblock dev (not blocking)

  method read buf ofs len = Alsa.Pcm.readn_float dev buf ofs len

  method write buf ofs len = Alsa.Pcm.writen_float dev buf ofs len

  method close = Alsa.Pcm.close dev
end
