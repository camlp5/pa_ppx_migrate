(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)

module Ast_4_02 = struct
type lexing_position = [%import: All_ast.Ast_4_02.Lexing.position]
and location_t = [%import: All_ast.Ast_4_02.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_02.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_02.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_02.Asttypes.label
]

and closed_flag =  [%import: All_ast.Ast_4_02.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_02.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_02.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_02.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_02.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_02.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_02.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_02.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_02.Asttypes.constant]
and location_stack = [%import: All_ast.Ast_4_02.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_02.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_02.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_02.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_02.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_02.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_02.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_02.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_02.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_02.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_02.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.constant := constant ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_02.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_02.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
      Asttypes.constant := constant
    ]
]
and case = [%import: All_ast.Ast_4_02.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_02.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_02.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_02.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_02.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_02.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_extension = [%import: All_ast.Ast_4_02.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_02.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
    ]
]
and class_signature = [%import: All_ast.Ast_4_02.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_02.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_02.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_02.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
    ]
]
and class_structure = [%import: All_ast.Ast_4_02.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_02.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_02.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_02.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_02.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_02.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_02.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_02.Parsetree.signature_item_desc]
and module_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_02.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_02.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_02.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_02.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_02.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_02.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_02.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_02.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_02.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_02.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_02.Outcometree.out_ident]
and out_value = [%import: All_ast.Ast_4_02.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_02.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_02.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_02.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_02.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_02.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_02.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_02.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_02.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_02.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_rec_status = [%import: All_ast.Ast_4_02.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_02.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_02.Outcometree.out_phrase]
end

module Ast_4_03 = struct
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

end

module Ast_4_04 = struct
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


end

module Ast_4_05 = struct
type lexing_position = [%import: All_ast.Ast_4_05.Lexing.position]
and location_t = [%import: All_ast.Ast_4_05.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_05.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_05.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_05.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_05.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_05.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_05.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_05.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_05.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_05.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_05.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_05.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_05.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_05.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_05.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_05.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_05.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_05.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_05.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_05.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_05.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_05.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_05.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
and pattern = [%import: All_ast.Ast_4_05.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_05.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_05.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_05.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_05.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_05.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_05.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_05.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_05.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_05.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_05.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_05.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_05.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_05.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_05.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_05.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_signature = [%import: All_ast.Ast_4_05.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_05.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_05.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_05.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_05.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_05.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_05.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_05.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
    ]
]
and class_structure = [%import: All_ast.Ast_4_05.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_05.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_05.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_05.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_05.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_05.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_05.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_05.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_05.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_05.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_05.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_05.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_05.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_05.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_05.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_05.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_05.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_05.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_05.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_05.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_05.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_05.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_05.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_05.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_05.Outcometree.out_ident]
and out_attribute = [%import: All_ast.Ast_4_05.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_05.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_05.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_05.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_05.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_05.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_05.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_05.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_05.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_05.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_05.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_05.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_05.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_05.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_05.Outcometree.out_phrase]


end

module Ast_4_06 = struct
type lexing_position = [%import: All_ast.Ast_4_06.Lexing.position]
and location_t = [%import: All_ast.Ast_4_06.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_06.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_06.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_06.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_06.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_06.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_06.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_06.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_06.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_06.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_06.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_06.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_06.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_06.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_06.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_06.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_06.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_06.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_06.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_06.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_06.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_06.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_06.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_06.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_06.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_06.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_06.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_06.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_06.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_06.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_06.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_06.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_06.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_06.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_06.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_06.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_06.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_06.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_06.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_06.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_06.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_06.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_06.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_06.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_06.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_06.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_06.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_06.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_06.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_06.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_06.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_06.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_06.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_06.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_06.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_06.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_06.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_06.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_06.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_06.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_06.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_06.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_06.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_06.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_06.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_06.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_06.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_06.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_06.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_06.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_06.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_06.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_06.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_06.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_06.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_06.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_06.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_06.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_06.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_06.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_06.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_06.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_06.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_06.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_06.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_06.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_06.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_06.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_06.Outcometree.out_phrase]


end

module Ast_4_07 = struct
type lexing_position = [%import: All_ast.Ast_4_07.Lexing.position]
and location_t = [%import: All_ast.Ast_4_07.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_07.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_07.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_07.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_07.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_07.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_07.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_07.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_07.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_07.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_07.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_07.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_07.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_07.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_07.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_07.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_07.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_07.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_07.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_07.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_07.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_07.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_07.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_07.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_07.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_07.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_07.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_07.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_07.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_07.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_07.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_07.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_07.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_07.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_07.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_07.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_07.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_07.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_07.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_07.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_07.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_07.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_07.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_07.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_07.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_07.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_07.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_07.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_07.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_07.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_07.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_07.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_07.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_07.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_07.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_07.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_07.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_07.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_07.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_07.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_07.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_07.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_07.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_07.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_07.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_07.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_07.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_07.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_07.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_07.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_07.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_07.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_07.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_07.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_07.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_07.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_07.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_07.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_07.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_07.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_07.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_07.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_07.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_07.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_07.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_07.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_07.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_07.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_07.Outcometree.out_phrase]


end

module Ast_4_08 = struct
type lexing_position = [%import: All_ast.Ast_4_08.Lexing.position]
and location_t = [%import: All_ast.Ast_4_08.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_08.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_08.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_08.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_08.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_08.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_08.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_08.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_08.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_08.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_08.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_08.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_08.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_08.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_08.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_08.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_08.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_08.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_08.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_08.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_08.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_08.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_08.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_08.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_08.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_08.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_08.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_08.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_08.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_08.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_08.Parsetree.case]
and letop = [%import: All_ast.Ast_4_08.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_08.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_08.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_08.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_08.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_08.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_08.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_08.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_08.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_08.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_08.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_08.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_08.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_08.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_08.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_08.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_08.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_08.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_08.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_08.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_08.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_08.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_08.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_08.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_08.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_08.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_08.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_08.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_08.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_08.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_08.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_08.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_08.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_08.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_08.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_08.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_08.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_08.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_08.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_08.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_08.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_08.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_08.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_08.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_08.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_08.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_08.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_name = [%import: All_ast.Ast_4_08.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_08.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_08.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_08.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_08.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_08.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_08.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_08.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_08.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_08.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_08.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_08.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_08.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_08.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_08.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_08.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_08.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_08.Outcometree.out_phrase]

end

module Ast_4_09 = struct
type lexing_position = [%import: All_ast.Ast_4_09.Lexing.position]
and location_t = [%import: All_ast.Ast_4_09.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_09.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_09.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_09.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_09.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_09.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_09.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_09.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_09.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_09.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_09.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_09.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_09.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_09.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_09.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_09.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_09.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_09.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_09.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_09.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_09.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_09.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_09.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_09.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_09.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_09.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_09.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_09.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_09.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_09.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_09.Parsetree.case]
and letop = [%import: All_ast.Ast_4_09.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_09.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_09.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_09.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_09.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_09.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_09.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_09.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_09.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_09.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_09.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_09.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_09.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_09.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_09.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_09.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_09.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_09.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_09.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_09.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_09.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_09.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_09.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_09.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_09.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_09.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_09.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_09.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_09.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_09.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_09.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_09.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_09.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_09.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_09.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_09.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_09.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_09.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_09.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_09.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_09.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_09.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_09.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_09.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_09.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_09.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_09.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_09.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_09.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_name = [%import: All_ast.Ast_4_09.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_09.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_09.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_09.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_09.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_09.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_09.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_09.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_09.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_09.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_09.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_09.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_09.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_09.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_09.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_09.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_09.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_09.Outcometree.out_phrase]


end

module Ast_4_10 = struct
type lexing_position = [%import: All_ast.Ast_4_10.Lexing.position]
and location_t = [%import: All_ast.Ast_4_10.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_10.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_10.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_10.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_10.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_10.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_10.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_10.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_10.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_10.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_10.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_10.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_10.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_10.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_10.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_10.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_10.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_10.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_10.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_10.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_10.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_10.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_10.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_10.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_10.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_10.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_10.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_10.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_10.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_10.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_10.Parsetree.case]
and letop = [%import: All_ast.Ast_4_10.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_10.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_10.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_10.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_10.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_10.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_10.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_10.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_10.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_10.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_10.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_10.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_10.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_10.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_10.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_10.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_10.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_10.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_10.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_10.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_10.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_10.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_10.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_10.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_10.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_10.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_10.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_10.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_10.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_4_10.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_4_10.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_10.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_10.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_10.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_10.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_10.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_10.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_10.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_10.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_10.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_10.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_10.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_10.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_10.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_10.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_10.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_10.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_10.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_10.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_10.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_4_10.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_4_10.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_10.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_10.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_10.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_10.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_10.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_10.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_10.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_10.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_10.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_10.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_10.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_4_10.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_10.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_10.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_10.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_10.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_10.Outcometree.out_phrase]

end

module Ast_4_11 = struct
type lexing_position = [%import: All_ast.Ast_4_11.Lexing.position]
and location_t = [%import: All_ast.Ast_4_11.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_11.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_11.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_11.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_11.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_11.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_11.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_11.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_11.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_11.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_11.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_11.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_11.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_11.Parsetree.constant
    [@with Location.t := location_t]
]
and location_stack = [%import: All_ast.Ast_4_11.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_11.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_11.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_11.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_11.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_11.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_11.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_11.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_11.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_11.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_11.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_11.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_11.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_11.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_11.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_11.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_11.Parsetree.case]
and letop = [%import: All_ast.Ast_4_11.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_11.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_11.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_11.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_11.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_11.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_11.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_11.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_11.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_11.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_11.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_11.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_11.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_11.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_11.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_11.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_11.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_11.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_11.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_11.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_11.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_11.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_11.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_11.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_11.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_11.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_11.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_11.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_11.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_4_11.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_4_11.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_11.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_11.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_11.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_11.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_11.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_11.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_11.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_11.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_11.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_11.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_11.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_11.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_11.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_11.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_11.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_11.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_11.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_11.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_11.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_4_11.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_4_11.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_11.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_11.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_11.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_11.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_11.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_11.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_11.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_11.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_11.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_11.Outcometree.out_sig_item
  [@with
    [%typ: string * (bool * bool)] := out_type_param
  ]
]
and out_type_param = string * (bool * bool)
and out_type_decl = [%import: All_ast.Ast_4_11.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
        ; [%typ: string * (bool * bool)] := out_type_param
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_4_11.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_11.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_11.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_11.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_11.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_11.Outcometree.out_phrase]


end

module Ast_4_12 = struct
type lexing_position = [%import: All_ast.Ast_4_12.Lexing.position]
and location_t = [%import: All_ast.Ast_4_12.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_12.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_12.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_12.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_12.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_12.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_12.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_12.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_12.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_12.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_12.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_12.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_12.Asttypes.variance]
and injectivity =  [%import: All_ast.Ast_4_12.Asttypes.injectivity]
and constant =  [%import: All_ast.Ast_4_12.Parsetree.constant
    [@with Location.t := location_t]
]
and location_stack = [%import: All_ast.Ast_4_12.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_12.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_12.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_12.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_12.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_12.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_12.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_12.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_12.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_12.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_12.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_12.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_12.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_12.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_12.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_12.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_12.Parsetree.case]
and letop = [%import: All_ast.Ast_4_12.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_12.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_12.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_12.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
          ; [%typ: Asttypes.variance * Asttypes.injectivity] := variance_injectivity
    ]
]
and variance_injectivity = All_ast.Ast_4_12.Asttypes.variance * All_ast.Ast_4_12.Asttypes.injectivity
and type_kind = [%import: All_ast.Ast_4_12.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_12.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_12.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_12.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_12.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
         ; [%typ: Asttypes.variance * Asttypes.injectivity] := variance_injectivity
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_12.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_12.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_12.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_12.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_12.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_12.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_12.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_12.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_12.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
         ; [%typ: Asttypes.variance * Asttypes.injectivity] := variance_injectivity
    ]
]
and class_description = [%import: All_ast.Ast_4_12.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_12.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_12.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_12.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_12.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_12.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_12.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_12.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_12.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_12.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_12.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_4_12.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_4_12.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_12.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_12.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_12.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_12.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_12.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_12.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_12.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_12.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_12.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_12.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_12.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_12.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_12.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_12.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_12.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_12.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_12.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_12.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_12.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_4_12.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_4_12.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_12.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_12.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_12.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_12.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_12.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_12.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_12.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_12.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_12.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_12.Outcometree.out_sig_item]
and out_type_param = [%import: All_ast.Ast_4_12.Outcometree.out_type_param
    [@with
       Asttypes.variance := variance
     ; Asttypes.injectivity := injectivity
    ]
]
and out_type_decl = [%import: All_ast.Ast_4_12.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_4_12.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_12.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_12.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_12.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_12.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_12.Outcometree.out_phrase]

end

module Ast_4_13 = struct
type lexing_position = [%import: All_ast.Ast_4_13.Lexing.position]
and location_t = [%import: All_ast.Ast_4_13.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_13.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_13.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_13.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_13.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_13.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_13.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_13.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_13.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_13.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_13.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_13.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_13.Asttypes.variance]
and injectivity =  [%import: All_ast.Ast_4_13.Asttypes.injectivity]
and constant =  [%import: All_ast.Ast_4_13.Parsetree.constant
    [@with Location.t := location_t]
]
and location_stack = [%import: All_ast.Ast_4_13.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_13.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_13.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_13.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_13.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_13.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_13.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_13.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_13.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_13.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_13.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_13.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_13.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_13.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_13.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_13.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_13.Parsetree.case]
and letop = [%import: All_ast.Ast_4_13.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_13.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_13.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_13.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.injectivity := injectivity
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_13.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_13.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_13.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_13.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_13.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_13.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_13.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_13.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_13.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_13.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_13.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_13.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_13.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_13.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_13.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_13.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_13.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_13.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_13.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_13.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_13.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_13.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_13.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_13.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_13.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_4_13.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_4_13.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_13.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_13.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_13.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_13.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_13.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_13.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_13.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_13.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_13.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_13.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_13.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_13.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_13.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_13.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_13.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_13.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_13.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_13.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_13.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_4_13.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_4_13.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_13.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_13.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_13.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_13.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_13.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_13.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_13.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_13.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_13.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_13.Outcometree.out_sig_item]
and out_type_param = [%import: All_ast.Ast_4_13.Outcometree.out_type_param
    [@with
       Asttypes.variance := variance
     ; Asttypes.injectivity := injectivity
    ]
]
and out_type_decl = [%import: All_ast.Ast_4_13.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_4_13.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_13.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_13.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_13.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_13.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_13.Outcometree.out_phrase]

end

module Ast_4_14 = struct
type lexing_position = [%import: All_ast.Ast_4_14.Lexing.position]
and location_t = [%import: All_ast.Ast_4_14.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_14.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_14.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_14.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_14.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_14.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_14.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_14.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_14.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_14.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_14.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_14.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_14.Asttypes.variance]
and injectivity =  [%import: All_ast.Ast_4_14.Asttypes.injectivity]
and constant =  [%import: All_ast.Ast_4_14.Parsetree.constant
    [@with Location.t := location_t]
]
and location_stack = [%import: All_ast.Ast_4_14.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_14.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_4_14.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_14.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_14.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_14.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_14.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_14.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_14.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_4_14.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_14.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_4_14.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_14.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_14.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_14.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_14.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_14.Parsetree.case]
and letop = [%import: All_ast.Ast_4_14.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_4_14.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_4_14.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_14.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.injectivity := injectivity
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_14.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_14.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_14.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_14.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_14.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_14.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_4_14.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_14.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_14.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_14.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_14.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_14.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_14.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_14.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_14.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_14.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_14.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_14.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_14.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_14.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_14.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_14.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_14.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_14.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_14.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_4_14.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_4_14.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_14.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_14.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_14.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_4_14.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_14.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_4_14.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_4_14.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_4_14.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_4_14.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_14.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_14.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_14.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_14.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_14.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_14.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_14.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_14.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_14.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_14.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_4_14.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_4_14.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_4_14.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_14.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_14.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_14.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_14.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_14.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_14.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_14.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_14.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_14.Outcometree.out_sig_item]
and out_type_param = [%import: All_ast.Ast_4_14.Outcometree.out_type_param
    [@with
       Asttypes.variance := variance
     ; Asttypes.injectivity := injectivity
    ]
]
and out_type_decl = [%import: All_ast.Ast_4_14.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_4_14.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_constructor = [%import: All_ast.Ast_4_14.Outcometree.out_constructor]
and out_type_extension = [%import: All_ast.Ast_4_14.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_14.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_14.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_14.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_14.Outcometree.out_phrase]


end

module Ast_5_0 = struct
type lexing_position = [%import: All_ast.Ast_5_0.Lexing.position]
and location_t = [%import: All_ast.Ast_5_0.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_5_0.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_5_0.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_5_0.Asttypes.label]
and arg_label = [%import: All_ast.Ast_5_0.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_5_0.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_5_0.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_5_0.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_5_0.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_5_0.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_5_0.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_5_0.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_5_0.Asttypes.variance]
and injectivity =  [%import: All_ast.Ast_5_0.Asttypes.injectivity]
and constant =  [%import: All_ast.Ast_5_0.Parsetree.constant
    [@with Location.t := location_t]
]
and location_stack = [%import: All_ast.Ast_5_0.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_5_0.Parsetree.attribute
    [@with Asttypes.loc := location_loc
         ; Location.t := location_t
    ]
]
and extension = [%import: All_ast.Ast_5_0.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_5_0.Parsetree.attributes]
and payload = [%import: All_ast.Ast_5_0.Parsetree.payload]
and core_type = [%import: All_ast.Ast_5_0.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_5_0.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_5_0.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_5_0.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and row_field_desc = [%import: All_ast.Ast_5_0.Parsetree.row_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_5_0.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ; Location.t := location_t
    ]
]
and object_field_desc = [%import: All_ast.Ast_5_0.Parsetree.object_field_desc
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_5_0.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_5_0.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_5_0.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_5_0.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_5_0.Parsetree.case]
and letop = [%import: All_ast.Ast_5_0.Parsetree.letop]
and binding_op = [%import: All_ast.Ast_5_0.Parsetree.binding_op
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and value_description = [%import: All_ast.Ast_5_0.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_5_0.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.injectivity := injectivity
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_5_0.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_5_0.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_5_0.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_5_0.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_5_0.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.private_flag := private_flag
         ; Location.t := location_t
    ]
]
and extension_constructor = [%import: All_ast.Ast_5_0.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_exception = [%import: All_ast.Ast_5_0.Parsetree.type_exception
    [@with Location.t := location_t ;]
]
and extension_constructor_kind = [%import: All_ast.Ast_5_0.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_5_0.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_5_0.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_5_0.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_5_0.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_5_0.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_5_0.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.injectivity := injectivity
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_5_0.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_5_0.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_5_0.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_5_0.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_5_0.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_5_0.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_5_0.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_5_0.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_5_0.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_5_0.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_5_0.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and functor_parameter = [%import: All_ast.Ast_5_0.Parsetree.functor_parameter
    [@with Asttypes.loc := location_loc]
]
and signature = [%import: All_ast.Ast_5_0.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_5_0.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_5_0.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_5_0.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_substitution = [%import: All_ast.Ast_5_0.Parsetree.module_substitution
    [@with Longident.t := longident_t
         ; Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_5_0.Parsetree.module_type_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and 'a open_infos = [%import: 'a All_ast.Ast_5_0.Parsetree.open_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
    ]
]
and open_description = [%import: All_ast.Ast_5_0.Parsetree.open_description
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and open_declaration = [%import: All_ast.Ast_5_0.Parsetree.open_declaration]
and 'a include_infos = [%import: 'a All_ast.Ast_5_0.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_5_0.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_5_0.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_5_0.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_5_0.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_5_0.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_5_0.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_5_0.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_5_0.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_5_0.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_5_0.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and type_immediacy_t = [%import: All_ast.Ast_5_0.Type_immediacy.t]
and out_name = [%import: All_ast.Ast_5_0.Outcometree.out_name]
and out_ident = [%import: All_ast.Ast_5_0.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_5_0.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_5_0.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_5_0.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_5_0.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_5_0.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_5_0.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_5_0.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_5_0.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_5_0.Outcometree.out_sig_item]
and out_type_param = [%import: All_ast.Ast_5_0.Outcometree.out_type_param
    [@with
       Asttypes.variance := variance
     ; Asttypes.injectivity := injectivity
    ]
]
and out_type_decl = [%import: All_ast.Ast_5_0.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag
        ; Type_immediacy.t := type_immediacy_t
    ]
]
and out_extension_constructor = [%import: All_ast.Ast_5_0.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_constructor = [%import: All_ast.Ast_5_0.Outcometree.out_constructor]
and out_type_extension = [%import: All_ast.Ast_5_0.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_5_0.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_5_0.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_5_0.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_5_0.Outcometree.out_phrase]
end
