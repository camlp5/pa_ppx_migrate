# 1 "ast.ORIG.ml"
(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: 
# 5 "ast.ORIG.ml"
             
# 5 "ast.ORIG.ml"
                  Lexing.position] 
end

# 9 "ast.ORIG.ml"
module Warnings = struct
[%%import: 
# 10 "ast.ORIG.ml"
             
# 10 "ast.ORIG.ml"
                  Warnings.loc] 
end

# 14 "ast.ORIG.ml"
module Location = struct
[%%import: 
# 15 "ast.ORIG.ml"
             
# 15 "ast.ORIG.ml"
                  Location.t] 
[%%import: 'a 
# 16 "ast.ORIG.ml"
                
# 16 "ast.ORIG.ml"
                     Location.loc] 
end
module Longident = struct
[%%import: 
# 19 "ast.ORIG.ml"
             
# 19 "ast.ORIG.ml"
                  Longident.t] 
end
module Asttypes = struct
[%%import: 
# 22 "ast.ORIG.ml"
             
# 22 "ast.ORIG.ml"
                  Asttypes.loc] 
# 24 "ast.ORIG.ml"
[%%import: 
# 24 "ast.ORIG.ml"
             
# 24 "ast.ORIG.ml"
                  Asttypes.arg_label] 
# 26 "ast.ORIG.ml"
[%%import: 
# 26 "ast.ORIG.ml"
             
# 26 "ast.ORIG.ml"
                  Asttypes.label] 
[%%import: 
# 27 "ast.ORIG.ml"
             
# 27 "ast.ORIG.ml"
                  Asttypes.closed_flag] 
[%%import: 
# 28 "ast.ORIG.ml"
             
# 28 "ast.ORIG.ml"
                  Asttypes.rec_flag] 
[%%import: 
# 29 "ast.ORIG.ml"
             
# 29 "ast.ORIG.ml"
                  Asttypes.direction_flag] 
[%%import: 
# 30 "ast.ORIG.ml"
             
# 30 "ast.ORIG.ml"
                  Asttypes.private_flag] 
[%%import: 
# 31 "ast.ORIG.ml"
             
# 31 "ast.ORIG.ml"
                  Asttypes.mutable_flag] 
[%%import: 
# 32 "ast.ORIG.ml"
             
# 32 "ast.ORIG.ml"
                  Asttypes.virtual_flag] 
[%%import: 
# 33 "ast.ORIG.ml"
             
# 33 "ast.ORIG.ml"
                  Asttypes.override_flag] 
[%%import: 
# 34 "ast.ORIG.ml"
             
# 34 "ast.ORIG.ml"
                  Asttypes.variance] 
# 36 "ast.ORIG.ml"
[%%import: 
# 36 "ast.ORIG.ml"
             
# 36 "ast.ORIG.ml"
                  Asttypes.injectivity]
# 41 "ast.ORIG.ml"
end
module Parsetree = struct
open Asttypes
# 45 "ast.ORIG.ml"
[%%import: 
# 45 "ast.ORIG.ml"
             
# 45 "ast.ORIG.ml"
                  Parsetree.constant] 
# 47 "ast.ORIG.ml"
type location_stack = Location.t list 
[%%import: 
# 48 "ast.ORIG.ml"
             
# 48 "ast.ORIG.ml"
                  Parsetree.attribute] 
end

# 52 "ast.ORIG.ml"
module Type_immediacy = struct
[%%import: 
# 53 "ast.ORIG.ml"
             
# 53 "ast.ORIG.ml"
                  Type_immediacy.t] 
end

# 57 "ast.ORIG.ml"
module Outcometree = struct
# 59 "ast.ORIG.ml"
[%%import: 
# 59 "ast.ORIG.ml"
             
# 59 "ast.ORIG.ml"
                  Outcometree.out_name] 
# 61 "ast.ORIG.ml"
[%%import: 
# 61 "ast.ORIG.ml"
             
# 61 "ast.ORIG.ml"
                  Outcometree.out_ident] 
# 63 "ast.ORIG.ml"
[%%import: 
# 63 "ast.ORIG.ml"
             
# 63 "ast.ORIG.ml"
                  Outcometree.out_string] 
# 66 "ast.ORIG.ml"
[%%import: 
# 66 "ast.ORIG.ml"
             
# 66 "ast.ORIG.ml"
                  Outcometree.out_attribute] 
# 68 "ast.ORIG.ml"
[%%import: 
# 68 "ast.ORIG.ml"
             
# 68 "ast.ORIG.ml"
                  Outcometree.out_value] 
# 70 "ast.ORIG.ml"
[%%import: 
# 70 "ast.ORIG.ml"
             
# 70 "ast.ORIG.ml"
                  Outcometree.out_type_param]
# 72 "ast.ORIG.ml"
[%%import: 
# 72 "ast.ORIG.ml"
             
# 72 "ast.ORIG.ml"
                  Outcometree.out_type] 
[%%import: 
# 73 "ast.ORIG.ml"
             
# 73 "ast.ORIG.ml"
                  Outcometree.out_class_type] 
[%%import: 
# 74 "ast.ORIG.ml"
             
# 74 "ast.ORIG.ml"
                  Outcometree.out_module_type] 
[%%import: 
# 75 "ast.ORIG.ml"
             
# 75 "ast.ORIG.ml"
                  Outcometree.out_phrase] 
end
