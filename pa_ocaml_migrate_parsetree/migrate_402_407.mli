(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = Reorg_ast.Ast_4_02
module DST = Reorg_ast.Ast_4_07

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_02.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_02
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_02
      ; dstmod = DST
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
        srcmod = Reorg_ast.Ast_4_02
      ; dstmod = DST
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
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
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
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
        srcmod = Reorg_ast.Ast_4_02
      ; dstmod = DST
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
        ; dsttype = [%typ: DST.constant]
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
        ; dsttype = [%typ: DST.core_type_desc]
        ; custom_branches_code = function
            Ptyp_arrow (v_0, v_1, v_2) ->
            let open DST in
            Ptyp_arrow
              (migrate_label_arg_label __dt__ __inh__ v_0,
               __dt__.migrate_core_type __dt__ __inh__ v_1,
               __dt__.migrate_core_type __dt__ __inh__ v_2)
            | Ptyp_object (v_0, v_1) ->
              let open DST in
              Ptyp_object
                (List.map (fun (v_0, v_1, v_2) ->
                     Otag(__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                          __dt__.migrate_attributes __dt__ __inh__ v_1,
                          __dt__.migrate_core_type __dt__ __inh__ v_2)) v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
                
            | Ptyp_poly (v_0, v_1) ->
              let open DST in
              Ptyp_poly
                (List.map (fun v_0 ->
                  __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0)) v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1)
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.row_field]
        ; custom_branches_code = function
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST in
              Rtag
                (__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ (wrap_loc __inh__ v_0),
                 __dt__.migrate_attributes __dt__ __inh__ v_1,
                 v_2,
                 List.map (__dt__.migrate_core_type __dt__ __inh__) v_3)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
              Pexp_fun (v_0, v_1, v_2, v_3) ->
              let open DST in
              Pexp_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 __dt__.migrate_option __dt__.migrate_expression __dt__ __inh__ v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_expression __dt__ __inh__ v_3)
            | Pexp_apply (v_0, v_1) ->
              let open DST in
              Pexp_apply
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1) v_1)
            | Pexp_send (v_0, v_1) ->
              let open DST in
              Pexp_send
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_1))
            | Pexp_newtype (v_0, v_1) ->
              let open DST in
              Pexp_newtype
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                 __dt__.migrate_expression __dt__ __inh__ v_1)
        }
      ; migrate_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.constructor_declaration]
        ; inherit_code = Some pcd_loc
        ; skip_fields = [ pcd_args ]
        ; custom_fields_code = {
            pcd_args =
              DST.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args)
          }
        }
      ; migrate_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.extension_constructor_kind]
        ; custom_branches_code = function
    Pext_decl (v_0, v_1) ->
      let open DST in
      Pext_decl
        (DST.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) v_0),
         Option.map (__dt__.migrate_core_type __dt__ __inh__) v_1)
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.class_type_desc]
        ; custom_branches_code = function
Pcty_arrow (v_0, v_1, v_2) ->
      let open DST in
      Pcty_arrow
        (migrate_label_arg_label __dt__ __inh__ v_0,
         __dt__.migrate_core_type __dt__ __inh__ v_1,
         __dt__.migrate_class_type __dt__ __inh__ v_2)
        }
      ; migrate_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.class_type_field_desc]
        ; custom_branches_code = function
            | Pctf_val v_0 ->
              let open DST in
              Pctf_val
                ((fun (v_0, v_1, v_2, v_3) ->
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3)
                   v_0)

            | Pctf_method v_0 ->
              let open DST in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                    __dt__.migrate_private_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)

        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.class_expr_desc]
        ; custom_branches_code = function
              Pcl_fun (v_0, v_1, v_2, v_3) ->
              let open DST in
              Pcl_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 Option.map (__dt__.migrate_expression __dt__ __inh__)  v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_class_expr __dt__ __inh__ v_3)
            | Pcl_apply (v_0, v_1) ->
              let open DST in
              Pcl_apply
                (__dt__.migrate_class_expr __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1)
                   v_1)
        }
      ; migrate_class_field_desc = {
          srctype = [%typ: class_field_desc]
        ; dsttype = [%typ: DST.class_field_desc]
        ; custom_branches_code = function
              Pcf_inherit (v_0, v_1, v_2) ->
              let open DST in
              Pcf_inherit
                (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                 __dt__.migrate_class_expr __dt__ __inh__ v_1,
                 Option.map (fun v -> __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v)) v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.signature_item_desc]
        ; custom_branches_code = function
              Psig_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Nonrecursive
                else DST.Recursive in
              let open DST in
              Psig_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
        }
      ; migrate_with_constraint = {
          srctype = [%typ: with_constraint]
        ; dsttype = [%typ: DST.with_constraint]
        ; custom_branches_code = function
            | Pwith_typesubst x0 ->
              let lid_loc = map_loc (fun x -> Lident x) x0.ptype_name in 
              let open DST in
              Pwith_typesubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_type_declaration __dt__ __inh__ x0)

            | Pwith_modsubst (v_0, v_1) ->
              let lid_loc = map_loc (fun x -> Lident x) v_0 in 
              let open DST in
              Pwith_modsubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1)

        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Nonrecursive
                else DST.Recursive in
              let open DST in
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
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.out_value]
        ; custom_branches_code = function
            | Oval_string v_0 ->
              let open DST in
              Oval_string (v_0, max_int, Ostr_string)
        }
      ; migrate_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.out_variant]
        ; custom_branches_code = function
            | Ovar_name (v_0, v_1) ->
              let open DST in
              Ovar_typ
                (Otyp_constr
                   (__dt__.migrate_out_ident __dt__ __inh__ v_0,
                    List.map (__dt__.migrate_out_type __dt__ __inh__) v_1))
        }
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.out_sig_item]
        ; custom_branches_code = function
              Osig_value (v_0, v_1, v_2) ->
              let open DST in
              Osig_value
                {oval_name = v_0
                ; oval_type = __dt__.migrate_out_type __dt__ __inh__ v_1
                ; oval_prims = v_2
                ; oval_attributes = []}
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.out_type_decl]
        ; custom_fields_code = {
            otype_immediate = false
          ; otype_unboxed = false
          }
        }
      }
    }
]
