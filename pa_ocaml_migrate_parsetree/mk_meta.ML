#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for the "pa_ocaml_migrate_parsetree" library:
requires = "camlp5,fmt,bos"
version = "%s"
description = "pa_ocaml_migrate_parsetree support"

# For linking
package "link" (
requires = "camlp5,fmt,bos"
archive(byte) = "pa_omp.cma"
archive(native) = "pa_omp.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_omp.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,bos"
  archive(syntax,preprocessor,-native) = "pa_omp.cma"
  archive(syntax,preprocessor,native) = "pa_omp.cmxa"
|} version)
