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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_migrate" preprocessor:
version = "%s"
description = "pa_ppx_migrate deriver"

  requires(toploop) = "camlp5,pa_ppx.deriving,pa_ppx.params_runtime"
  archive(toploop) = "pa_deriving_migrate.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving,pa_ppx.params_runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_migrate.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_migrate.cmx"

  package "link" (
  requires = "camlp5,pa_ppx.deriving.link"
  archive(byte) = "pa_deriving_migrate.cmo"
  archive(native) = "pa_deriving_migrate.cmx"
  )
|} version)
