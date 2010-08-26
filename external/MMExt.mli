module Audio : sig
  module IO : sig
    module Mad : sig
      val reader_of_file : string -> Audio.IO.reader
    end
  end
end
