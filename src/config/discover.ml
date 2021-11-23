module C = Configurator.V1

external is_big_endian : unit -> bool = "ocaml_mm_is_big_endian"

let () =
  C.main ~name:"mm" (fun c ->
      let has_aligned_alloc =
        C.c_test c
          {|
        #include <stdlib.h>

        int main() {
          char *data = aligned_alloc(16, 4096);
          return 0;
        }
      |}
      in

      let has_max_align_t =
        C.c_test c
          {|
        int main() {
          size_t x = sizeof(max_align_t);
          return 0;
        }
      |}
      in

      C.C_define.gen_header_file c ~fname:"config.h"
        [
          ("BIGENDIAN", Switch (is_big_endian ()));
          ("HAS_ALIGNED_ALLOC", Switch has_aligned_alloc);
          ("HAS_MAX_ALIGN_T", Switch has_max_align_t);
        ])
