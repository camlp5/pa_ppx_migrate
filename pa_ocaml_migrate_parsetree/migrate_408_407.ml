(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = Reorg_ast.Ast_4_08
module DST = Reorg_ast.Ast_4_07

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

[%%import: Migrate_408_407.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_08
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_08
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
        srcmod = Reorg_ast.Ast_4_08
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
      }
      ; {
        srcmod = Reorg_ast.Ast_4_08
      ; dstmod = DST
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
        ; code = fun __dt__ __inh__ { attr_name = attr_name;
                                      attr_payload = attr_payload;
                                      attr_loc = _ } ->
            (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ attr_name,
             __dt__.migrate_payload __dt__ __inh__ attr_payload)
        }
      ; migrate_core_type = {
          srctype = [%typ: core_type]
        ; dsttype = [%typ: DST.core_type]
        ; inherit_code = Some ptyp_loc
        ; skip_fields = [ ptyp_loc_stack ]
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.row_field]
        ; code = fun __dt__ __inh__ -> function
              { prf_desc = Rtag (ll, b, ctl);
                prf_loc = prf_loc ;
                prf_attributes = prf_attributes } ->
              let open DST in
              Rtag(__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ ll,
                   __dt__.migrate_attributes __dt__ __inh__ prf_attributes,
                   b,
                   List.map (__dt__.migrate_core_type __dt__ __inh__) ctl)

            | { prf_desc = Rinherit ct;
                prf_loc = prf_loc ;
                prf_attributes = prf_attributes } ->
              let open DST in
              Rinherit (__dt__.migrate_core_type __dt__ __inh__ ct)
        }
      ; migrate_object_field = {
          srctype = [%typ: object_field]
        ; dsttype = [%typ: DST.object_field]
        ; code = fun __dt__ __inh__ -> function
              { pof_desc = Otag(ll, ct);
                pof_loc = pof_loc;
                pof_attributes = pof_attributes } ->
              let open DST in
              Otag(__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ ll,
                   __dt__.migrate_attributes __dt__ __inh__ pof_attributes,
                   __dt__.migrate_core_type __dt__ __inh__ ct)

            | { pof_desc = Oinherit ct;
                pof_loc = pof_loc;
                pof_attributes = pof_attributes } ->
              let open DST in
              Oinherit (__dt__.migrate_core_type __dt__ __inh__ ct)
        }
      ; migrate_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.pattern]
        ; inherit_code = Some ppat_loc
        ; skip_fields = [ ppat_loc_stack ]
        }
      ; migrate_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.expression]
        ; inherit_code = Some pexp_loc
        ; skip_fields = [ pexp_loc_stack ]
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
            | Pexp_open ({ popen_expr = { pmod_desc = Pmod_ident ll;
                                          pmod_loc = pmod_loc ;
                                          pmod_attributes = pmod_attributes } ;
                           popen_override = popen_override;
                           popen_loc = popen_loc ;
                           popen_attributes = attributes }, e) ->
              let open DST in
              Pexp_open(__dt__.migrate_override_flag __dt__ __inh__ popen_override,
                        __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll,
                        __dt__.migrate_expression __dt__ __inh__ e)
            | Pexp_open ({ popen_expr = _ ; }, _) ->
              migration_error __inh__ "Pexp_open:longident"
            | Pexp_letop _ ->
              migration_error __inh__ "Pexp_letop"
        }
      ; migrate_type_extension = {
          srctype = [%typ: type_extension]
        ; dsttype = [%typ: DST.type_extension]
        ; skip_fields = [ ptyext_loc ]
        }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open ({ popen_expr = ll;
                   popen_override = popen_override;
                   popen_loc = popen_loc;
                   popen_attributes = popen_attributes }, ct) ->
              let open DST in
              Pcty_open (__dt__.migrate_override_flag __dt__ __inh__ popen_override,
                         __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll,
                        __dt__.migrate_class_type __dt__ __inh__ ct)
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.class_expr_desc]
        ; custom_branches_code = function
            Pcl_open
                ({ popen_expr = v_1;
                   popen_override = v_0 ;
                   popen_loc = popen_loc ;
                   popen_attributes = popen_attributes },
                 v_2) ->
              let open DST in
              Pcl_open (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                        __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1,
                        __dt__.migrate_class_expr __dt__ __inh__ v_2)
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.signature_item_desc]
        ; custom_branches_code = function
            | Psig_exception {ptyexn_constructor = ptyexn_constructor} ->
              let open DST in
              Psig_exception (__dt__.migrate_extension_constructor __dt__ __inh__ ptyexn_constructor)
            | Psig_modsubst _ ->
              migration_error __inh__ "Psig_modsubst"
            | Psig_typesubst _ -> migration_error __inh__ "Psig_typesubst"
        }
      ; migrate_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.open_description]
        ; inherit_code = Some popen_loc
        ; skip_fields = [ popen_expr ]
        ; custom_fields_code = {
            popen_lid = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ popen_expr
          }
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.structure_item_desc]
        ; custom_branches_code = function
            | Pstr_exception {ptyexn_constructor = ptyexn_constructor} ->
              let open DST in
              Pstr_exception (__dt__.migrate_extension_constructor __dt__ __inh__ ptyexn_constructor)
            | Pstr_open { popen_expr = { pmod_desc = Pmod_ident ll;
                                 pmod_loc = pmod_loc ;
                                 pmod_attributes = pmod_attributes } ;
                          popen_override = popen_override;
                          popen_loc = popen_loc;
                          popen_attributes = popen_attributes } ->
              let open DST in
              Pstr_open { popen_lid = __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ ll ;
                          popen_override = __dt__.migrate_override_flag __dt__ __inh__ popen_override ;
                          popen_loc = __dt__.migrate_location_t __dt__ __inh__ popen_loc ;
                          popen_attributes  = __dt__.migrate_attributes __dt__ __inh__ popen_attributes }
            | Pstr_open { popen_expr = _ ; } ->
              migration_error __inh__ "Pstr_open:longident"
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
      ; migrate_out_name = {
          srctype = [%typ: out_name]
        ; dsttype = [%typ: string]
        ; code = fun _ _ { printed_name = printed_name } -> printed_name
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
              Otyp_module
                (Oide_ident { printed_name = v_0 },
                 v_1,
                 v_2) ->
              let open DST in
              Otyp_module (v_0, v_1,
                           List.map (__dt__.migrate_out_type __dt__ __inh__) v_2)
            | Otyp_module _ -> migration_error __inh__ "Otyp_module:out_ident"
        }
      }
    }
]
