(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = Reorg_ast.Ast_4_06
module DST = Reorg_ast.Ast_4_02

include (sig open Reorg_ast end)

[%%import: Reorg_ast.Ast_4_06.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_06 ;
          dstmod = DST ;
          types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_06 ;
        dstmod = DST ;
        types = [
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
        srcmod = Reorg_ast.Ast_4_06 ;
        dstmod = DST ;
        types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; constructor_declaration
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; include_infos
        ; location_stack
        ; label_declaration
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
          class_infos = Some pci_loc
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
        srcmod = Reorg_ast.Ast_4_06 ;
        dstmod = DST ;
        types = [
          out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
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
      ; migrate_arg_label = {
          srctype = [%typ: arg_label]
        ; dsttype = [%typ: DST.label]
        ; code = migrate_arg_label_label
        }
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.constant]
        ; code = migrate_Parsetree_constant_Asttypes_constant
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_payload = {
          srctype = [%typ: payload]
        ; dsttype = [%typ: DST.payload]
        ; custom_branches_code = function
              PSig _x0 ->
              migration_error __inh__ "PSig"
        }
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.core_type_desc]
        ; custom_branches_code = function
            | Ptyp_object (v_0, v_1) ->
              let unpack_object_field = function
            Otag (ll, al, ct) ->
              let open DST in
              (ll,
                al,
                ct)
            | Oinherit _ -> migration_error __inh__ "Oinherit" in
              let open DST in
              Ptyp_object
                ((fun __dt__ __inh__ ->
                    __dt__.migrate_list
                      (fun __dt__ __inh__ (v_0, v_1, v_2) ->
                         unwrap_loc v_0,
                         __dt__.migrate_attributes __dt__ __inh__ v_1,
                         __dt__.migrate_core_type __dt__ __inh__ v_2)
                      __dt__ __inh__)
                   __dt__ __inh__ (List.map unpack_object_field v_0),
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
            | Ptyp_poly (v_0, v_1) ->
              let open DST in
              Ptyp_poly
                (List.map unwrap_loc v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1)
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.row_field]
        ; custom_branches_code = function
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST in
              Rtag
                (__dt__.migrate_label __dt__ __inh__ (unwrap_loc v_0),
                 __dt__.migrate_attributes __dt__ __inh__ v_1,
                 v_2,
                 List.map (__dt__.migrate_core_type __dt__ __inh__) v_3)
        }
      ; migrate_object_field = {
          srctype = [%typ: object_field]
        ; dsttype = [%typ: (string DST.location_loc * DST.attributes * DST.core_type)]
        ; dstmodule = DST
        ; code = fun __dt__ __inh__ -> function
            Otag (ll, al, ct) ->
              let open DST in
              (__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ ll,
               __dt__.migrate_attributes __dt__ __inh__ al,
               __dt__.migrate_core_type __dt__ __inh__ ct)
            | Oinherit _ -> migration_error __inh__ "Oinherit"
        }
      ; migrate_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.pattern_desc]
        ; custom_branches_code = function
              Ppat_open _ -> migration_error __inh__ "Ppat_open"
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
            | Pexp_letexception _ -> migration_error __inh__ "Pexp_letexception"
            | Pexp_unreachable  ->
              migration_error __inh__ "Pexp_unreachable"
            | Pexp_send (v_0, v_1) ->
              let open DST in
              Pexp_send
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 unwrap_loc v_1)
            | Pexp_newtype (v_0, v_1) ->
              let open DST in
              Pexp_newtype
                (unwrap_loc v_0,
                 __dt__.migrate_expression __dt__ __inh__ v_1)
        }
      ; migrate_constructor_arguments = {
          srctype = [%typ: constructor_arguments]
        ; dsttype = [%typ: DST.core_type list]
        ; custom_branches_code = function
              Pcstr_tuple pcd_args ->
              List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args
            | Pcstr_record _ -> migration_error __inh__ "Pcstr_record"
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open _ ->migration_error __inh__ "Pcty_open"
        }
      ; migrate_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.class_type_field_desc]
        ; custom_branches_code = function
            | Pctf_val v_0 ->
              let open DST in
              Pctf_val
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
            | Pctf_method v_0 ->
              let open DST in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    unwrap_loc v_0,
                    __dt__.migrate_private_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.class_expr_desc]
        ; custom_branches_code = function
            | Pcl_open _ -> migration_error __inh__ "Pcl_open"
        }
      ; migrate_class_field = {
          srctype = [%typ: class_field]
        ; dsttype = [%typ: DST.class_field]
        ; inherit_code = Some pcf_loc
        ; custom_branches_code = function
              Pcf_inherit (v_0, v_1, v_2) ->
              let open DST in
              Pcf_inherit
                (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                 __dt__.migrate_class_expr __dt__ __inh__ v_1,
                 Option.map unwrap_loc v_2)
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
                 Option.map unwrap_loc v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.signature_item_desc]
        ; custom_branches_code = function
              Psig_type (Recursive, v_0) ->
              Psig_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
            | Psig_type (Nonrecursive, []) -> Psig_type []
            | Psig_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Psig_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) (h::t))
        }
      ; migrate_with_constraint = {
          srctype = [%typ: with_constraint]
        ; dsttype = [%typ: DST.with_constraint]
        ; custom_branches_code = function
            | Pwith_typesubst ({txt=Lident _;}, v_1) ->
              let open DST in
              Pwith_typesubst (__dt__.migrate_type_declaration __dt__ __inh__ v_1)
            | Pwith_typesubst _ -> migration_error __inh__ "Pwith_typesubst:longident"
            | Pwith_modsubst (v_0, v_1) ->
              let v_0 = map_loc (function
                    Lident s -> s
                  | _ -> migration_error __inh__ "Pwith_modsubst:longident") v_0 in
              let open DST in
              Pwith_modsubst
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                 __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1)
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.structure_item_desc]
        ; custom_branches_code = function
              Pstr_type (Recursive, v_0) ->
              Pstr_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0)
            | Pstr_type (Nonrecursive, []) -> Pstr_type []
            | Pstr_type (Nonrecursive, h::t) ->
              let h = { h with ptype_attributes = ({txt="nonrec"; loc=src_loc_none}, PStr[]) :: h.ptype_attributes } in
              Pstr_type (List.map (__dt__.migrate_type_declaration __dt__ __inh__) (h::t))
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
            | Oval_string (s, _, _) -> 
              let open DST in
              Oval_string s
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
              Otyp_attribute _ -> migration_error __inh__ "Otyp_attribute"
        }
      ; migrate_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.out_variant]
        ; custom_branches_code = function
            | Ovar_typ (Otyp_constr (id,tyl)) ->
              Ovar_name (__dt__.migrate_out_ident __dt__ __inh__ id,
                         List.map (__dt__.migrate_out_type __dt__ __inh__) tyl)
            | Ovar_typ x0 ->
              Ovar_name
                (Oide_ident "", [__dt__.migrate_out_type __dt__ __inh__ x0])
        }
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.out_sig_item]
        ; custom_branches_code = function
              Osig_value ovd ->
              let open DST in
              Osig_value
                (ovd.oval_name,
                 __dt__.migrate_out_type __dt__ __inh__ ovd.oval_type,
                 ovd.oval_prims)
            | Osig_ellipsis -> migration_error __inh__ "Osig_ellipsis"
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.out_type_decl]
        ; skip_fields = [ otype_unboxed; otype_immediate ]
        }
      }
    }
]
