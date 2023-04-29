(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = All_ast.Ast_4_12
module DST = All_ast.Ast_4_13

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_12.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_12
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_12.Asttypes
      ; dstmod = DST.Asttypes
      ; types = [
          arg_label
        ; closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; virtual_flag
        ; variance
        ; injectivity
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_12.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; binding_op
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_expr_desc
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; class_type_field_desc
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
        ; core_type_desc
        ; expression
        ; expression_desc
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; functor_parameter
        ; include_declaration
        ; include_description
        ; label_declaration
        ; letop
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_substitution
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; object_field
        ; object_field_desc
        ; open_declaration
        ; open_description
        ; package_type
        ; pattern
        ; payload
        ; row_field
        ; row_field_desc
        ; signature
        ; signature_item
        ; signature_item_desc
        ; structure
        ; structure_item
        ; structure_item_desc
        ; type_declaration
        ; type_exception
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; constructor_declaration = Some pcd_loc
        ; extension_constructor = Some pext_loc
        ; label_declaration = Some pld_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_binding = Some pvb_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
        srcmod = All_ast.Ast_4_12.Outcometree
            ; dstmod = DST.Outcometree
                  ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_name
        ; out_phrase
        ; out_rec_status
        ; out_sig_item
        ; out_string
        ; out_type_decl
        ; out_type_extension
        ; out_type_param
        ; out_val_decl
        ; out_value
        ; out_variant
        ]
      }
      ]
    ; dispatchers = {
        migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; migrate_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.Parsetree.pattern_desc]
        ; custom_branches_code = function
        | Ppat_construct (v_0, v_1) ->
           let open DST.Parsetree in
           Ppat_construct
             (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_0,
              __dt__.migrate_option (fun __dt__ __inh__ p -> ([], __dt__.migrate_pattern __dt__ __inh__ p))
                __dt__ __inh__ v_1)
        }
      ; migrate_type_immediacy_t = {
          srctype = [%typ: type_immediacy_t]
        ; dsttype = [%typ: DST.Type_immediacy.t]
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.Outcometree.out_type]
        ; custom_branches_code = function
    | Otyp_module (v_0, v_1, v_2) ->
        let open DST.Outcometree in
        if List.length v_1 <> List.length v_2 then
          migration_error None "Otyp_module args of differing length" ;
        Otyp_module
          (__dt__.migrate_out_ident __dt__ __inh__ v_0,
           List.map2 (fun s oty -> (s, __dt__.migrate_out_type __dt__ __inh__ oty)) v_1 v_2)
        }
      }
    }
]
