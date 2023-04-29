(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = All_ast.Ast_4_02
module DST = All_ast.Ast_4_04

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_02.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_02
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_02.Asttypes
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
        srcmod = All_ast.Ast_4_02.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; class_type_field_desc
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
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
        srcmod = All_ast.Ast_4_02.Outcometree
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
        ; out_type
        ; out_type_extension
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
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.Parsetree.constant]
        ; code = migrate_Asttypes_constant_Parsetree_constant
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
            Ptyp_arrow (v_0, v_1, v_2) ->
            let open DST.Parsetree in
            Ptyp_arrow
              (migrate_label_arg_label __dt__ __inh__ v_0,
               __dt__.migrate_core_type __dt__ __inh__ v_1,
               __dt__.migrate_core_type __dt__ __inh__ v_2)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = function
              Pexp_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pexp_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 __dt__.migrate_option __dt__.migrate_expression __dt__ __inh__ v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_expression __dt__ __inh__ v_3)
            | Pexp_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_apply
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1) v_1)
        }
      ; migrate_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        ; inherit_code = Some pcd_loc
        ; skip_fields = [ pcd_args ]
        ; custom_fields_code = {
            pcd_args =
              DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args)
          }
        }
      ; migrate_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor_kind]
        ; custom_branches_code = function
    Pext_decl (v_0, v_1) ->
      let open DST.Parsetree in
      Pext_decl
        (DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) v_0),
         Option.map (__dt__.migrate_core_type __dt__ __inh__) v_1)
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        ; custom_branches_code = function
Pcty_arrow (v_0, v_1, v_2) ->
      let open DST.Parsetree in
      Pcty_arrow
        (migrate_label_arg_label __dt__ __inh__ v_0,
         __dt__.migrate_core_type __dt__ __inh__ v_1,
         __dt__.migrate_class_type __dt__ __inh__ v_2)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        ; custom_branches_code = function
              Pcl_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pcl_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 Option.map (__dt__.migrate_expression __dt__ __inh__)  v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_class_expr __dt__ __inh__ v_3)
            | Pcl_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pcl_apply
                (__dt__.migrate_class_expr __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1)
                   v_1)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        ; custom_branches_code = function
              Psig_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Psig_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Pstr_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
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
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        ; custom_branches_code = function
              Osig_value (v_0, v_1, v_2) ->
              let open DST.Outcometree in
              Osig_value
                {oval_name = v_0
                ; oval_type = __dt__.migrate_out_type __dt__ __inh__ v_1
                ; oval_prims = v_2
                ; oval_attributes = []}
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; custom_fields_code = {
            otype_immediate = false
          ; otype_unboxed = false
          }
        }
      }
    }
]
