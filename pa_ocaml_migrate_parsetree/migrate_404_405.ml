(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = All_ast.Ast_4_04
module DST = All_ast.Ast_4_05

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

let wrap_loc inh v =
  let loc = match inh with
      None -> src_loc_none
    | Some loc -> loc in
  let open SRC.Location in
  { txt = v ; loc = loc }

exception Migration_error of string * SRC.Location.t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

[%%import: Migrate_404_405.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_04
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_04.Asttypes
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
        srcmod = All_ast.Ast_4_04.Parsetree
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
        srcmod = All_ast.Ast_4_04.Outcometree
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
        ; out_value
        ; out_val_decl
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
                (List.map (fun (v_0, v_1, v_2) ->
                     (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                      __dt__.migrate_attributes __dt__ __inh__ v_1,
                      __dt__.migrate_core_type __dt__ __inh__ v_2)) v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
                
            | Ptyp_poly (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_poly
                (List.map (fun v_0 ->
                  __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0)) v_0,
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
                 __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_1))
            | Pexp_newtype (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_newtype
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
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
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3)
                   v_0)

            | Pctf_method v_0 ->
              let open DST.Parsetree in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
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
                 Option.map (fun v -> __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v)) v_2)
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
            | Ovar_name (v_0, v_1) ->
              let open DST.Outcometree in
              Ovar_typ
                (Otyp_constr
                   (__dt__.migrate_out_ident __dt__ __inh__ v_0,
                    List.map (__dt__.migrate_out_type __dt__ __inh__) v_1))
        }
      }
    }
]
