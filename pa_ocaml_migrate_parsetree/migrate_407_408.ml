(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_4_07
module DST = Reorg_ast.Ast_4_08

let src_loc_none =
  let open SRC in
  let open SRC in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dst_loc_none =
  let open DST in
  let open DST in
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
  let open SRC in
  { txt = v ; loc = loc }

let map_loc f v =
  let open SRC in
  { txt = f v.txt ; loc = v.loc }

let unwrap_loc v = v.SRC.txt

exception Migration_error of string * SRC.location_t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _migrate_list subrw0 __dt__ __inh__ l =
  List.map (subrw0 __dt__ __inh__) l

[%%import: Migrate_407_408.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_07
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_07
      ; dstmod = DST
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
        srcmod = Reorg_ast.Ast_4_07
      ; dstmod = DST
      ; types = [
          attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_desc
        ; class_field_kind
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; class_type_field_desc
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type_desc
        ; extension
        ; extension_constructor
        ; extension_constructor_kind
        ; include_declaration
        ; include_description
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; package_type
        ; payload
        ; pattern_desc
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
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
        ; module_binding = Some pmb_loc
        ; module_declaration = Some pmd_loc
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
        srcmod = Reorg_ast.Ast_4_07
      ; dstmod = DST
      ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_sig_item
        ; out_string
        ; out_type_decl
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
      ; migrate_attribute = {
          srctype = [%typ: attribute]
        ; dsttype = [%typ: DST.attribute]
        ; code = fun __dt__ __inh__ (v_0, v_1) ->
            let open DST in
            let name = __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0 in
            let pay = __dt__.migrate_payload __dt__ __inh__ v_1 in
            { attr_name = name;
              attr_payload = pay;
              attr_loc = name.loc }
        }
      ; migrate_core_type = {
          srctype = [%typ: core_type]
        ; dsttype = [%typ: DST.core_type]
        ; inherit_code = Some ptyp_loc
        ; custom_fields_code = {
            ptyp_loc_stack = []
          }
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.row_field]
        ; custom_branches_code = function
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST in
              let ll = __dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ v_0 in
              { prf_desc = Rtag
                    (ll,
                     v_2,
                     List.map (__dt__.migrate_core_type __dt__ __inh__)  v_3);
                prf_loc = ll.loc;
                prf_attributes = __dt__.migrate_attributes __dt__ __inh__ v_1
              }
            | Rinherit v_0 ->
              let open DST in
              { prf_desc = Rinherit (__dt__.migrate_core_type __dt__ __inh__ v_0);
                prf_loc = dst_loc_none;
                prf_attributes = []
              }
        }
      ; migrate_object_field = {
          srctype = [%typ: object_field]
        ; dsttype = [%typ: DST.object_field]
        ; custom_branches_code = function
              Otag (v_0, v_1, v_2) ->
              let open DST in
              let ll = __dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ v_0 in
              { pof_desc = Otag (ll, __dt__.migrate_core_type __dt__ __inh__ v_2);
                pof_loc = ll.loc;
                pof_attributes = __dt__.migrate_attributes __dt__ __inh__ v_1 }
            | Oinherit v_0 ->
              let open DST in
              { pof_desc = Oinherit (__dt__.migrate_core_type __dt__ __inh__ v_0);
                pof_loc = dst_loc_none;
                pof_attributes = [] }
        }
      ; migrate_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.pattern]
        ; inherit_code = Some ppat_loc
        ; custom_fields_code = {
            ppat_loc_stack = []
          }
        }
      ; migrate_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.expression]
        ; inherit_code = Some pexp_loc
        ; custom_fields_code = {
            pexp_loc_stack = []
          }
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
            | Pexp_open (v_0, v_1, v_2) ->
              let open DST in
              let ll = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1 in
              Pexp_open
                ({ popen_expr = { pmod_desc = Pmod_ident ll;
                                  pmod_loc = ll.loc ;
                                  pmod_attributes = [] };
                   popen_override = __dt__.migrate_override_flag __dt__ __inh__ v_0;
                   popen_loc = ll.loc ;
                   popen_attributes = [] },
                 __dt__.migrate_expression __dt__ __inh__ v_2)
        }
      ; migrate_type_extension = {
          srctype = [%typ: type_extension]
        ; dsttype = [%typ: DST.type_extension]
        ; custom_fields_code = {
            ptyext_loc = __dt__.migrate_location_t __dt__ __inh__ ptyext_path.SRC.loc
          }
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open (v_0, v_1, v_2) ->
              let open DST in
              let ll = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1 in
              Pcty_open
                ({ popen_expr = ll;
                   popen_override = __dt__.migrate_override_flag __dt__ __inh__ v_0;
                   popen_loc = ll.loc;
                   popen_attributes = [] },
                 __dt__.migrate_class_type __dt__ __inh__ v_2)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.class_expr_desc]
        ; custom_branches_code = function
            | Pcl_open (v_0, v_1, v_2) ->
              let open DST in
              let ll = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1 in
              Pcl_open
                ({ popen_expr = ll;
                   popen_override = __dt__.migrate_override_flag __dt__ __inh__ v_0;
                   popen_loc = ll.loc;
                   popen_attributes = [] },
                 __dt__.migrate_class_expr __dt__ __inh__ v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.signature_item_desc]
        ; custom_branches_code = function
            | Psig_exception v_0 ->
              let open DST in
              Psig_exception
                { ptyexn_constructor = __dt__.migrate_extension_constructor __dt__ __inh__ v_0 ;
                  ptyexn_loc = dst_loc_none ;
                  ptyexn_attributes = [] }
        }
      ; migrate_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.open_description]
        ; inherit_code = Some popen_loc
        ; skip_fields = [ popen_lid ]
        ; custom_fields_code = {
            popen_expr = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ popen_lid
          }
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.structure_item_desc]
        ; custom_branches_code = function
            | Pstr_exception v_0 ->
              let open DST in
              Pstr_exception
                { ptyexn_constructor = __dt__.migrate_extension_constructor __dt__ __inh__ v_0 ;
                  ptyexn_loc = dst_loc_none ;
                  ptyexn_attributes = [] }
            | Pstr_open v_0 ->
              let open DST in
              let odesc = __dt__.migrate_open_description __dt__ __inh__ v_0 in
              let ll = odesc.popen_expr in
              Pstr_open
                { popen_expr = { pmod_desc = Pmod_ident ll;
                                 pmod_loc = ll.loc ;
                                 pmod_attributes = [] };
                  popen_override = odesc.popen_override;
                  popen_loc = odesc.popen_loc ;
                  popen_attributes = odesc.popen_attributes }
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
      ; migrate_out_ident = {
          srctype = [%typ: out_ident]
        ; dsttype = [%typ: DST.out_ident]
        ; custom_branches_code = function
            | Oide_ident v_0 ->
              let open DST in
              Oide_ident { printed_name = v_0 }
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
            | Otyp_module (v_0, v_1, v_2) ->
              let open DST in
              Otyp_module
                (Oide_ident { printed_name = v_0 },
                 v_1,
                 List.map (__dt__.migrate_out_type __dt__ __inh__) v_2)
        }
      }
    }
]
