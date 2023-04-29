(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = All_ast.Ast_4_09
module DST = All_ast.Ast_4_10

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_09.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_09
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_09.Asttypes
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
        ; variance
        ; virtual_flag
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_09.Parsetree
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
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; label_declaration
        ; letop
        ; location_stack
        ; module_expr
        ; module_substitution
        ; module_type
        ; module_type_declaration
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
        srcmod = All_ast.Ast_4_09.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_name
        ; out_phrase
        ; out_rec_status
        ; out_string
        ; out_type
        ; out_type_extension
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
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.Parsetree.pattern_desc]
        ; custom_branches_code = function
            | Ppat_unpack v_0 ->
              let v_0 = map_loc (fun x -> Some x) v_0 in
              let open DST.Parsetree in
              Ppat_unpack
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
            | Pexp_letmodule (v_0, v_1, v_2) ->
              let v_0 = map_loc (fun x -> Some x) v_0 in
              let open DST.Parsetree in
              Pexp_letmodule
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                 __dt__.migrate_module_expr __dt__ __inh__ v_1,
                 __dt__.migrate_expression __dt__ __inh__ v_2)
        }
      ; migrate_module_type_desc = {
          srctype = [%typ: module_type_desc]
        ; dsttype = [%typ: DST.Parsetree.module_type_desc]
        ; custom_branches_code = function
            | Pmty_functor (v_0, v_1, v_2) ->
              let v_0 = map_loc (fun x -> Some x) v_0 in
              let open DST.Parsetree in
              Pmty_functor
                ((match v_1 with
                   None -> Unit
                 | Some mty ->
                   Named(__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                         __dt__.migrate_module_type __dt__ __inh__ mty)),
                 __dt__.migrate_module_type __dt__ __inh__ v_2)
        }
      ; migrate_module_declaration = {
          srctype = [%typ: module_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_declaration]
        ; inherit_code = Some pmd_loc
        ; skip_fields = [ pmd_name ]
        ; custom_fields_code = {
            pmd_name =
              let pmd_name = map_loc (fun x -> Some x) pmd_name in
              __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ pmd_name
          }
        }
      ; migrate_module_expr_desc = {
          srctype = [%typ: module_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.module_expr_desc]
        ; custom_branches_code = function
            | Pmod_functor (v_0, v_1, v_2) ->
              let v_0 = map_loc (fun x -> Some x) v_0 in
              let open DST.Parsetree in
              Pmod_functor
                ((match v_1 with
                      None -> Unit
                    | Some mty ->
                      Named(__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                            __dt__.migrate_module_type __dt__ __inh__ mty)),
                 __dt__.migrate_module_expr __dt__ __inh__ v_2)
        }
      ; migrate_module_binding = {
          srctype = [%typ: module_binding]
        ; dsttype = [%typ: DST.Parsetree.module_binding]
        ; inherit_code = Some pmb_loc
        ; skip_fields = [ pmb_name ]
        ; custom_fields_code = {
            pmb_name =
              let pmb_name = map_loc (fun x -> Some x) pmb_name in
              __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ pmb_name
          }
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
      ; migrate_out_module_type = {
          srctype = [%typ: out_module_type]
        ; dsttype = [%typ: DST.Outcometree.out_module_type]
        ; custom_branches_code = function
            | Omty_functor (v_0, v_1, v_2) ->
              let v_0 = Some v_0 in
              let open DST.Outcometree in
              Omty_functor
                (Option.map (fun mty ->
                     (v_0, __dt__.migrate_out_module_type __dt__ __inh__ mty)) v_1,
                 __dt__.migrate_out_module_type __dt__ __inh__ v_2)
        }
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; skip_fields = [ otype_immediate ]
        ; custom_fields_code = {
            otype_immediate =
              if otype_immediate then Always else Unknown
          }
        }
      }
    }
]
