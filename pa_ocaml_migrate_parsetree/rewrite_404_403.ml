
module SRC = All_ast.Ast_4_04
module DST = All_ast.Ast_4_03

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

type lexing_position = [%import: All_ast.Ast_4_04.Lexing.position]
and location_t = [%import: All_ast.Ast_4_04.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_04.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_04.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_04.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_04.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_04.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_04.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_04.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_04.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_04.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_04.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_04.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_04.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_04.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_04.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_04.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_04.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_04.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_04.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_04.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_04.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_04.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_04.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_04.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_04.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_04.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_04.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_04.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_04.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_04.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_04.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_04.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_04.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_04.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_04.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_04.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_04.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_04.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_04.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_signature = [%import: All_ast.Ast_4_04.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_04.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_04.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_04.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_04.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_04.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_04.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_04.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_structure = [%import: All_ast.Ast_4_04.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_04.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_04.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_04.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_04.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_04.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_04.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_04.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_04.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_04.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_04.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_04.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_04.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_04.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_04.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_04.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_04.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_04.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_04.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_04.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_04.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_04.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_04.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_04.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_04.Outcometree.out_ident]
and out_attribute = [%import: All_ast.Ast_4_04.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_04.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_04.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_04.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_04.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_04.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_04.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_04.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_04.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_04.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_04.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_04.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_04.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_04.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_04.Outcometree.out_phrase]


[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_04 ;
          dstmod = DST ;
          types = [
            lexing_position
          ; location_t
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_04.Asttypes ;
        dstmod = DST.Asttypes ;
        types = [
          arg_label
        ; closed_flag
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
        srcmod = All_ast.Ast_4_04.Parsetree ;
        dstmod = DST.Parsetree ;
        types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr_desc
        ; class_field_desc
        ; class_field_kind
        ; class_signature
        ; class_structure
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field_desc
        ; constant
        ; constructor_arguments
        ; core_type_desc
        ; extension
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; location_stack
        ; module_expr_desc
        ; module_type_desc
        ; package_type
        ; payload
        ; row_field
        ; signature
        ; signature_item_desc
        ; structure
        ; structure_item_desc
        ; type_extension
        ; type_kind
        ; with_constraint
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_04.Outcometree ;
        dstmod = DST.Outcometree ;
        types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_sig_item
        ; out_type
        ; out_type_extension
        ; out_value
        ; out_val_decl
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
      ; rewrite_Location_loc = {
          srctype = [%typ: 'a location_loc]
        ; dsttype = [%typ: 'b DST.Location.loc]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _rewrite_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_core_type = {
          srctype = [%typ: core_type]
        ; dsttype = [%typ: DST.Parsetree.core_type]
        ; inherit_code = Some ptyp_loc
        }
      ; rewrite_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.Parsetree.pattern]
        ; inherit_code = Some ppat_loc
        }
      ; rewrite_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.Parsetree.pattern_desc]
        ; custom_branches_code = function
              Ppat_open _ -> migration_error __inh__ "Ppat_open"
        }
      ; rewrite_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.Parsetree.expression]
        ; inherit_code = Some pexp_loc
        }
      ; rewrite_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
              Pexp_letexception _ -> migration_error __inh__ "Pexp_letexception"
        }
      ; rewrite_value_description = {
          srctype = [%typ: value_description]
        ; dsttype = [%typ: DST.Parsetree.value_description]
        ; inherit_code = Some pval_loc
        }
      ; rewrite_type_declaration = {
          srctype = [%typ: type_declaration]
        ; dsttype = [%typ: DST.Parsetree.type_declaration]
        ; inherit_code = Some ptype_loc
        }
      ; rewrite_label_declaration = {
          srctype = [%typ: label_declaration]
        ; dsttype = [%typ: DST.Parsetree.label_declaration]
        ; inherit_code = Some pld_loc
        }
      ; rewrite_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        ; inherit_code = Some pcd_loc
        }
      ; rewrite_extension_constructor = {
          srctype = [%typ: extension_constructor]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor]
        ; inherit_code = Some pext_loc
        }
      ; rewrite_class_type = {
          srctype = [%typ: class_type]
        ; dsttype = [%typ: DST.Parsetree.class_type]
        ; inherit_code = Some pcty_loc
        }
      ; rewrite_class_type_field = {
          srctype = [%typ: class_type_field]
        ; dsttype = [%typ: DST.Parsetree.class_type_field]
        ; inherit_code = Some pctf_loc
        }
      ; rewrite_class_infos = {
          srctype = [%typ: 'a class_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.class_infos]
        ; inherit_code = Some pci_loc
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_class_expr = {
          srctype = [%typ: class_expr]
        ; dsttype = [%typ: DST.Parsetree.class_expr]
        ; inherit_code = Some pcl_loc
        }
      ; rewrite_class_field = {
          srctype = [%typ: class_field]
        ; dsttype = [%typ: DST.Parsetree.class_field]
        ; inherit_code = Some pcf_loc
        }
      ; rewrite_module_type = {
          srctype = [%typ: module_type]
        ; dsttype = [%typ: DST.Parsetree.module_type]
        ; inherit_code = Some pmty_loc
        }
      ; rewrite_signature_item = {
          srctype = [%typ: signature_item]
        ; dsttype = [%typ: DST.Parsetree.signature_item]
        ; inherit_code = Some psig_loc
        }
      ; rewrite_module_declaration = {
          srctype = [%typ: module_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_declaration]
        ; inherit_code = Some pmd_loc
        }
      ; rewrite_module_type_declaration = {
          srctype = [%typ: module_type_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_type_declaration]
        ; inherit_code = Some pmtd_loc
        }
      ; rewrite_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.Parsetree.open_description]
        ; inherit_code = Some popen_loc
        }
      ; rewrite_include_infos = {
          srctype = [%typ: 'a include_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.include_infos]
        ; inherit_code = Some pincl_loc
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_module_expr = {
          srctype = [%typ: module_expr]
        ; dsttype = [%typ: DST.Parsetree.module_expr]
        ; inherit_code = Some pmod_loc
        }
      ; rewrite_structure_item = {
          srctype = [%typ: structure_item]
        ; dsttype = [%typ: DST.Parsetree.structure_item]
        ; inherit_code = Some pstr_loc
        }
      ; rewrite_value_binding = {
          srctype = [%typ: value_binding]
        ; dsttype = [%typ: DST.Parsetree.value_binding]
        ; inherit_code = Some pvb_loc
        }
      ; rewrite_module_binding = {
          srctype = [%typ: module_binding]
        ; dsttype = [%typ: DST.Parsetree.module_binding]
        ; inherit_code = Some pmb_loc
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
      ; rewrite_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; skip_fields = [ otype_unboxed ]
        }
      }
    }
]
