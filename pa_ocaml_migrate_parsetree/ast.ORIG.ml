(**pp -syntax camlp5o *)
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: PREFIX Lexing.position] 
end

#if OCAML_VERSION >= (4,6,0)
module Warnings = struct
[%%import: PREFIX Warnings.loc] 
end
#endif

module Location = struct
#if OCAML_VERSION >= (4,6,0) && defined REDECLARE
type t = [%import: PREFIX Location.t
  [@synonym: Warnings.loc]
]
#else
type t = [%import: PREFIX Location.t]
#endif
[%%import: 'a PREFIX Location.loc]
end
module Longident = struct
[%%import: PREFIX Longident.t] 
end
module Asttypes = struct
#if defined REDECLARE
type 'a loc = [%import: 'a PREFIX Asttypes.loc [@synonym: 'a Location.loc]] 
#else
type 'a loc = [%import: 'a PREFIX Asttypes.loc] 
#endif
#if OCAML_VERSION >= (4, 3, 0)
[%%import: PREFIX Asttypes.arg_label] 
#endif
[%%import: PREFIX Asttypes.label] 
[%%import: PREFIX Asttypes.closed_flag] 
[%%import: PREFIX Asttypes.rec_flag] 
[%%import: PREFIX Asttypes.direction_flag] 
[%%import: PREFIX Asttypes.private_flag] 
[%%import: PREFIX Asttypes.mutable_flag] 
[%%import: PREFIX Asttypes.virtual_flag] 
[%%import: PREFIX Asttypes.override_flag] 
[%%import: PREFIX Asttypes.variance] 
#if OCAML_VERSION >= (4, 12, 0)
[%%import: PREFIX Asttypes.injectivity]
#endif
#if OCAML_VERSION < (4, 3,  0)
[%%import: PREFIX Asttypes.constant] 
#endif
end
module Parsetree = struct
open Asttypes
#if OCAML_VERSION >= (4,3,0)
[%%import: PREFIX Parsetree.constant] 
#endif
type location_stack = Location.t list 
[%%import: PREFIX Parsetree.attribute] 
end

#if OCAML_VERSION >= (4,10,0)
module Type_immediacy = struct
[%%import: PREFIX Type_immediacy.t] 
end
#endif

module Outcometree = struct
#if OCAML_VERSION > (4,7,0)
[%%import: PREFIX Outcometree.out_name] 
#endif
[%%import: PREFIX Outcometree.out_ident] 
#if OCAML_VERSION > (4,5,0)
[%%import: PREFIX Outcometree.out_string] 
#endif
#if OCAML_VERSION > (4,2,0)
[%%import: PREFIX Outcometree.out_attribute] 
#endif
[%%import: PREFIX Outcometree.out_value] 
#if OCAML_VERSION >= (4,12,0)
[%%import: PREFIX Outcometree.out_type_param]
#endif
[%%import: PREFIX Outcometree.out_type] 
[%%import: PREFIX Outcometree.out_class_type] 
[%%import: PREFIX Outcometree.out_module_type] 
[%%import: PREFIX Outcometree.out_phrase] 
end
