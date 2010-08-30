(** Operations on audio data. *)

val samples_of_seconds : int -> float -> int

val seconds_of_samples : int -> int -> float

val lin_of_dB : float -> float

val dB_of_lin : float -> float

(** Operations on samples. *)
module Sample : sig
  (** A sample. *)
  type t = float

  (** Clip a sample (ie ensure that it is between [-1.] and [1.]. *)
  val clip : t -> t
end

(** Operations on notes. *)
module Note : sig
  type t = int

  val a4 : int
  val c5 : int
  val c0 : int

  val create : int -> int -> t

  val freq : t -> float

  val of_freq : float -> t

  val name : t -> int

  val octave : t -> int

  (** Returns note number and octave. *)
  val modulo : t -> int * int

  val to_string : t -> string
end

(** Operations on mono buffers (with only one channel). *)
module Mono : sig
  (** A mono buffer. *)
  type buffer = float array

  val create : int -> buffer

  val duration : buffer -> int

  (** Clear a portion of a buffer (fill it with zeroes). *)
  val clear : buffer -> int -> int -> unit

  (** Fill the buffer with random data (ie produce white noise). *)
  val randomize : buffer -> int -> int -> unit

  val resample : float -> buffer -> int -> int -> buffer

  val clip : buffer -> int -> int -> unit

  (** [add b1 b2] adds to contents of [b2] to [b1]. *)
  val add : buffer -> int -> buffer -> int -> int -> unit

  (** Buffers of variable size. These are particularly useful for temporary
      buffers. *)
  module Extensible_buffer : sig
    type t

    val create : int -> t

    val duration : t -> int

    val prepare : t -> int -> buffer
  end

  (** Functions for analyzing audio data. *)
  module Analyze : sig
    (** Compute the RMS power of a portion of a buffer. *)
    val rms : buffer -> int -> int -> float

    (** Simple implementation of the FFT algorithm. For fastest implementations
	optimized libraries such as fftw are recommended. *)
    module FFT : sig
      (** Internal data for computing FFT. *)
      type t

      (** Initialize FFT for an analysis of [2^n] samples. *)
      val init : int -> t

      (** Duration of the FFT buffer analysis in samples. *)
      val duration : t -> int

      (** [complex_create buf ofs len] create a array of complex numbers of size
	  [len] by copying data from [buf] from ofset [ofs] (the imaginary part
	  is null). *)
      val complex_create : buffer -> int -> int -> Complex.t array

      (** Perform an FFT analysis. *)
      val fft : t -> Complex.t array -> unit

      (** Frequency associated to the [k]-th coefficient of an FFT. *)
      val band_freq : int -> t -> int -> float

      (** Windowing functions. Thses can be used to on complex buffers in order
	  to improve the quality of the FFT, see
	  http://en.wikipedia.org/wiki/Windowing_functions. *)
      module Window : sig
	val cosine : Complex.t array -> unit

	val hann : Complex.t array -> unit

	val hamming : Complex.t array -> unit

	val lanczos : Complex.t array -> unit

	val triangular : Complex.t array -> unit

	val bartlett_hann : Complex.t array -> unit

	val blackman : ?alpha:float -> t -> Complex.t array -> unit

	val nuttall : t -> Complex.t array -> unit

	val blackman_harris : t -> Complex.t array -> unit

	val blackman_nuttall : t -> Complex.t array -> unit
      end

      val notes : int -> t -> ?window:(Complex.t array -> unit) -> ?note_min:int -> ?note_max:int -> ?volume_min:float -> ?filter_harmonics:bool -> float array -> int -> int -> (Note.t * float) list

      val loudest_note : (Note.t * float) list -> (Note.t * float) option

    end
  end

  module Effect : sig
    (** A compander following the µ-law (see
	http://en.wikipedia.org/wiki/Mu-law).*)
    val compand_mu_law : float -> buffer -> int -> int -> unit

    class type t =
    object
      method process : buffer -> int -> int -> unit
    end

    val biquad_filter : int -> [ `Band_pass | `High_pass | `Low_pass ] -> float -> float -> t

    (** ADSR (Attack/Decay/Sustain/Release) envelopes. *)
    module ADSR : sig
      (** Attack/Decay/Sustain/Release times. *)
      type t

      val make : int -> float * float * float * float -> t

      (** Current state in the ADSR envelope. *)
      type state

      (** Initial state for processing. *)
      val init : unit -> state

      val release : state -> state

      val dead : state -> bool

      val process : t -> state -> buffer -> int -> int -> state
    end
  end

  (** Sound generators. *)
  module Generator : sig
    (** A sound generator. *)
    class type t =
    object
      method set_volume : float -> unit

      method set_frequency : float -> unit

      (** Fill a buffer with generated sound. *)
      method fill : buffer -> int -> int -> unit

      (** Same as [fill] but adds the sound to the buffer. *)
      method fill_add : buffer -> int -> int -> unit

      (** Release the generator (used for generator with envelopes). *)
      method release : unit

      (** Is the generator still producing sound? This should become false soon
	  after release has been triggered. *)
      method dead : bool
    end

    (** Generate a sine waveform. *)
    val sine : int -> ?volume:float -> ?phase:float -> float -> t

    (** Generate a square waveform. *)
    val square : int -> ?volume:float -> ?phase:float -> float -> t

    (** Generate a saw waveform. *)
    val saw : int -> ?volume:float -> ?phase:float -> float -> t

    (** Apply an ADSR envlope on a generator. *)
    val adsr : Effect.ADSR.t -> t -> t
  end
end

(** An audio buffer. *)
type buffer = Mono.buffer array

(** [create chans len] creates a buffer with [chans] channels and [len] samples
    as duration. *)
val create : int -> int -> buffer

(** Create a buffer with the same number of channels and duration as the given
    buffer. *)
val create_same : buffer -> buffer

(** Clear the buffer (sets all the samples to zero). *)
val clear : buffer -> int -> int -> unit

(** Duration of a buffer in samples. *)
val duration : buffer -> int

(** Convert a buffer to a mono buffer by computing the mean of all channels. *)
val to_mono : buffer -> Mono.buffer

(** Convert a mono buffer into a buffer. Notice that the original mono buffer is
    not copied an might thus be modified afterwards. *)
val of_mono : Mono.buffer -> buffer

val resample : float -> buffer -> int -> int -> buffer

(** Same as [Array.blit] for audio data. *)
val blit : buffer -> int -> buffer -> int -> int -> unit

val clip : buffer -> int -> int -> unit

(** Amplify a portion of the buffer by a given coefficient. *)
val amplify : float -> buffer -> int -> int -> unit

(** Pan a stereo buffer from left to right (the buffer should have exactly two
    channels!). The coefficient should be between [-1.] and [1.]. *)
val pan : float -> buffer -> int -> int -> unit

val add : buffer -> int -> buffer -> int -> int -> unit

val add_coeff : buffer -> int -> float -> buffer -> int -> int -> unit

(** Buffers of variable size. These are particularly useful for temporary
    buffers. *)
module Extensible_buffer : sig
  type t

  val create : int -> int -> t

  val duration : t -> int

  val prepare : t -> int -> buffer
end

(** Circular ringbuffers. *)
module Ringbuffer : sig
  (** A ringbuffer. *)
  type t

  (** Create a ringbuffer of given number of channels and samplerate. *)
  val create : int -> int -> t

  val channels : t -> int

  val read_space : t -> int

  val write_space : t -> int

  val read_advance : t -> int -> unit

  val write_advance : t -> int -> unit

  val peek : t -> buffer -> int -> int -> unit

  val read : t -> buffer -> int -> int -> unit

  val write : t -> buffer -> int -> int -> unit

  val transmit : t -> (buffer -> int -> int -> int) -> int

  module Extensible : sig
    type t

    val create : int -> int -> t

    val channels : t -> int

    val read_space : t -> int

    val write_space : t -> int

    val read_advance : t -> int -> unit

    val write_advance : t -> int -> unit

    val peek : t -> buffer -> int -> int -> unit

    val read : t -> buffer -> int -> int -> unit

    val write : t -> buffer -> int -> int -> unit

    val transmit : t -> (buffer -> int -> int -> int) -> int
  end
end

(** Audio effects. *)
module Effect : sig
  (** A possibly stateful audio effect. *)
  class type t =
  object
    (** Apply the effect on a buffer. *)
    method process : buffer -> int -> int -> unit
  end

  val chain : t -> t -> t

  val of_mono : int -> (unit -> Mono.Effect.t) -> t

  (** [delay chans samplerate d once feedback] creates a delay operator for
      buffer with [chans] channels at [samplerate] samplerate with [d] as delay
      in seconds and [feedback] as feedback. If [once] is set to [true] only one
      echo will be heard (no feedback). *)
  val delay : int -> int -> float -> ?once:bool -> ?ping_pong:bool -> float -> t

  val biquad_filter : int -> int -> [ `Band_pass | `High_pass | `Low_pass ] -> float -> float -> t

  val auto_gain_control : int -> int -> ?rms_target:float -> ?rms_window:float -> ?kup:float -> ?kdown:float -> ?rms_threshold:float -> ?volume_init:float -> ?volume_min:float -> ?volume_max:float -> unit -> t
end

(** Sound generators. *)
module Generator : sig
  class type t =
  object
    method set_volume : float -> unit

    method set_frequency : float -> unit

    method fill : buffer -> int -> int -> unit

    method fill_add : buffer -> int -> int -> unit

    method release : unit

    method dead : bool
  end

  val of_mono : Mono.Generator.t -> t
end

(** Operation for reading and writing audio data from files, streams or
    devices. *)
module IO : sig
  (** The file is not valid. *)
  exception Invalid_file

  (** The operation is not valid on the file/device. *)
  exception Invalid_operation

  (** Trying to read past the end of the stream. *)
  exception End_of_stream

  class type reader =
  object
    (** Number of channels. *)
    method channels : int

    (** Sample rate in samples per second. *)
    method sample_rate : int

    (** Duration in samples. *)
    method duration : int

    (** Duration in seconds. *)
    method duration_time : float

    (** Seek to a given sample. *)
    method seek : int -> unit

    (** Close the file. This method should only be called once. The members of
	the object should not be accessed anymore after this method has been
	called. *)
    method close : unit

    method read : buffer -> int -> int -> int
  end

  (** Create a reader object from a wav file. *)
  val reader_of_wav_file : string -> reader

  class type writer =
  object
    method write : buffer -> int -> int -> unit

    method close : unit
  end

  val writer_to_wav_file : int -> int -> string -> writer

  (** Audio input and output using the OSS sound devices. *)
  module OSS : sig
    (** Create a writer on an OSS sound device. *)
    val writer : ?device:string -> int -> int -> writer

    val reader : ?device:string -> int -> int -> reader
  end
end
