(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = All_ast.Ast_4_05
module DST = All_ast.Ast_4_04

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_05.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_05
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_05.Asttypes
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
        srcmod = All_ast.Ast_4_05.Parsetree
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
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_desc
        ; class_type_field
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
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
        ; payload
        ; row_field
        ; signature
        ; signature_item
        ; signature_item_desc
        ; structure
        ; structure_item
        ; structure_item_desc
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
        srcmod = All_ast.Ast_4_05.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
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
        ; out_type_decl
        ; out_type_extension
        ; out_val_decl
        ; out_value
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
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.Parsetree.core_type_desc]
        ; custom_branches_code = function
            | Ptyp_object (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_object
                ((fun __dt__ __inh__ ->
                    __dt__.migrate_list
                      (fun __dt__ __inh__ (v_0, v_1, v_2) ->
                         unwrap_loc v_0,
                         __dt__.migrate_attributes __dt__ __inh__ v_1,
                         __dt__.migrate_core_type __dt__ __inh__ v_2)
                      __dt__ __inh__)
                   __dt__ __inh__ v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
            | Ptyp_poly (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_poly
                (List.map unwrap_loc v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
            | Pexp_send (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_send
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 unwrap_loc v_1)
            | Pexp_newtype (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_newtype
                (unwrap_loc v_0,
                 __dt__.migrate_expression __dt__ __inh__ v_1)
        }
      ; migrate_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_field_desc]
        ; custom_branches_code = function
            | Pctf_val v_0 ->
              let open DST.Parsetree in
              Pctf_val
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
            | Pctf_method v_0 ->
              let open DST.Parsetree in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_private_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
        }
      ; migrate_class_field_desc = {
          srctype = [%typ: class_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_field_desc]
        ; custom_branches_code = function
              Pcf_inherit (v_0, v_1, v_2) ->
              let open DST.Parsetree in
              Pcf_inherit
                (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                 __dt__.migrate_class_expr __dt__ __inh__ v_1,
                 Option.map unwrap_loc v_2)
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
      ; migrate_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.Outcometree.out_variant]
        ; custom_branches_code = function
            | Ovar_typ (Otyp_constr (id,tyl)) ->
              Ovar_name (__dt__.migrate_out_ident __dt__ __inh__ id,
                         List.map (__dt__.migrate_out_type __dt__ __inh__) tyl)
            | Ovar_typ x0 ->
              Ovar_name
                (Oide_ident "", [__dt__.migrate_out_type __dt__ __inh__ x0])
        }
      }
    }
]
