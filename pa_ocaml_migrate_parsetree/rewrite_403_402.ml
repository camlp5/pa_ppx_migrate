
module SRC = All_ast.Ast_4_03
module DST = All_ast.Ast_4_02

let src_loc_none =
  let open SRC.Lexing in
  let open SRC.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dst_loc_none =
  let open DST.Lexing in
  let open DST.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

exception Migration_error of string * SRC.Location.t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _rewrite_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

let rewrite_arg_label_label :
  'a -> 'b -> SRC.Asttypes.arg_label -> DST.Asttypes.label
  =
  fun __dt__ __inh__ -> function
  | SRC.Asttypes.Nolabel  -> ""
  | SRC.Asttypes.Labelled x0 -> x0
  | SRC.Asttypes.Optional x0 -> "?" ^ x0

let rewrite_Parsetree_constant_Asttypes_constant :
  'a -> SRC.Location.t option -> SRC.Parsetree.constant -> DST.Asttypes.constant =
  fun __dt__ __inh__ -> function
  | SRC.Parsetree.Pconst_integer (x0,x1) ->
     begin match x1 with
     | None -> DST.Asttypes.Const_int (int_of_string x0)
     | Some 'l' ->
         DST.Asttypes.Const_int32 (Int32.of_string x0)
     | Some 'L' ->
         DST.Asttypes.Const_int64 (Int64.of_string x0)
     | Some 'n' ->
         DST.Asttypes.Const_nativeint (Nativeint.of_string x0)
     | Some _ -> migration_error __inh__ "Pconst_integer"
     end
  | SRC.Parsetree.Pconst_char x0 ->
      DST.Asttypes.Const_char x0
  | SRC.Parsetree.Pconst_string (x0,x1) ->
      DST.Asttypes.Const_string (x0,x1)
  | SRC.Parsetree.Pconst_float (x0,x1) ->
      begin match x1 with
      | None -> DST.Asttypes.Const_float x0
      | Some _ -> migration_error __inh__ "Pconst_float"
      end

type lexing_position = [%import: All_ast.Ast_4_03.Lexing.position]
and location_t = [%import: All_ast.Ast_4_03.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_03.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_03.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_03.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_03.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_03.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_03.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_03.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_03.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_03.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_03.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_03.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_03.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_03.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_03.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_03.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_03.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_03.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_03.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_03.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_03.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_03.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_03.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_03.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_03.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_03.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_03.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_03.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_03.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_03.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_03.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_03.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_03.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_03.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_03.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_03.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_03.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_03.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_03.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_signature = [%import: All_ast.Ast_4_03.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_03.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_03.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_03.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_03.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_03.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_03.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_03.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_structure = [%import: All_ast.Ast_4_03.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_03.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_03.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_03.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_03.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_03.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_03.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_03.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_03.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_03.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_03.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_03.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_03.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_03.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_03.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_03.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_03.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_03.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_03.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_03.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_03.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_03.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_03.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_03.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_03.Outcometree.out_ident]
and out_attribute = [%import: All_ast.Ast_4_03.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_03.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_03.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_03.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_03.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_03.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_03.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_03.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_03.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_03.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_03.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_03.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_03.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_03.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_03.Outcometree.out_phrase]


[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_03
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_03.Asttypes
      ; dstmod = DST.Asttypes
      ; types = [
          closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; variance
        ; virtual_flag
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_03.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_expr_desc
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; class_type_field_desc
        ; constructor_declaration
        ; core_type
        ; core_type_desc
        ; expression
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; include_infos
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; open_description
        ; package_type
        ; pattern
        ; pattern_desc
        ; row_field
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_infos = Some pci_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; constructor_declaration = Some pcd_loc
        ; core_type = Some ptyp_loc
        ; expression = Some pexp_loc
        ; extension_constructor = Some pext_loc
        ; include_infos = Some pincl_loc
        ; label_declaration = Some pld_loc
        ; module_binding = Some pmb_loc
        ; module_declaration = Some pmd_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; open_description = Some popen_loc
        ; pattern = Some ppat_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_binding = Some pvb_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
        srcmod = All_ast.Ast_4_03.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
          out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_type_extension
        ; out_value
        ; out_variant
        ]
      }
      ]
    ; dispatchers = {
        rewrite_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; rewrite_arg_label = {
          srctype = [%typ: arg_label]
        ; dsttype = [%typ: DST.Asttypes.label]
        ; code = rewrite_arg_label_label
        }
      ; rewrite_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.Asttypes.constant]
        ; code = rewrite_Parsetree_constant_Asttypes_constant
        }
      ; rewrite_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _rewrite_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_payload = {
          srctype = [%typ: payload]
        ; dsttype = [%typ: DST.Parsetree.payload]
        ; custom_branches_code = function
              PSig _x0 ->
              migration_error __inh__ "PSig"
        }
      ; rewrite_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
              Pexp_unreachable  ->
              migration_error __inh__ "Pexp_unreachable"
        }
      ; rewrite_constructor_arguments = {
          srctype = [%typ: constructor_arguments]
        ; dsttype = [%typ: DST.Parsetree.core_type list]
        ; custom_branches_code = function
              Pcstr_tuple pcd_args ->
              List.map (__dt__.rewrite_core_type __dt__ __inh__) pcd_args
            | Pcstr_record _ -> migration_error __inh__ "Pcstr_record"
        }
      ; rewrite_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        ; custom_branches_code = function
              Psig_type (Recursive, v_0) ->
              Psig_type (List.map (__dt__.rewrite_type_declaration __dt__ __inh__) v_0)
            | Psig_type (Nonrecursive, []) -> Psig_type []
            | Psig_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Psig_type (List.map (__dt__.rewrite_type_declaration __dt__ __inh__) (h::t))
        }
      ; rewrite_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type (Recursive, v_0) ->
              Pstr_type (List.map (__dt__.rewrite_type_declaration __dt__ __inh__) v_0)
            | Pstr_type (Nonrecursive, []) -> Pstr_type []
            | Pstr_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Pstr_type (List.map (__dt__.rewrite_type_declaration __dt__ __inh__) (h::t))
        }
      ; rewrite_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; rewrite_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; rewrite_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.Outcometree.out_type]
        ; custom_branches_code = function
              Otyp_attribute _ -> migration_error __inh__ "Otyp_attribute"
        }
      ; rewrite_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        ; custom_branches_code = function
              Osig_value ovd ->
              let open DST.Outcometree in
              Osig_value
                (ovd.oval_name,
                 __dt__.rewrite_out_type __dt__ __inh__ ovd.oval_type,
                 ovd.oval_prims)
            | Osig_ellipsis -> migration_error __inh__ "Osig_ellipsis"
        }
      ; rewrite_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; skip_fields = [ otype_immediate ]
        }
      }
    }
]
