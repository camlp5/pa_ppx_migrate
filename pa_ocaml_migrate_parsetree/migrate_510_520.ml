(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_5_1
module DST = Reorg_ast.Ast_5_2

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

[%%import: Migrate_510_520.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_5_2
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_5_2
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
        ; virtual_flag
        ; variance
        ; injectivity
        ]
      }
      ; {
        srcmod = Reorg_ast.Ast_5_2
      ; dstmod = DST
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
        ; expression
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
        ; pattern_desc
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
        ; value_constraint
        ; value_description
        ; with_constraint
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; extension_constructor = Some pext_loc
        ; label_declaration = Some pld_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
          srcmod = Reorg_ast.Ast_5_2
        ; dstmod = DST
        ; types = [
          out_attribute
        ; out_class_sig_item
        ; out_constructor
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
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.core_type_desc]
        ; custom_branches_code = function
          | Ptyp_alias (v_0, v_1) ->
             let open DST in
             Ptyp_alias
               ((fun __dt__ __inh__ -> __dt__.migrate_core_type __dt__ __inh__)
                  __dt__ __inh__ v_0,
                __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_1))
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
          | Pexp_function v_0 ->
             let open DST in
             let loc = match __inh__ with
                 None -> src_loc_none
               | Some loc -> loc in
             Pexp_function
               ([], None,
                Pfunction_cases (
                    __dt__.migrate_list __dt__.migrate_case __dt__ __inh__ v_0,
                    __dt__.migrate_location_t __dt__ __inh__ loc,
                    []
                  )
               )
          | Pexp_fun (v_0, v_1, v_2, v_3) ->
             let open DST in
             let loc = match __inh__ with
                 None -> src_loc_none
               | Some loc -> loc in
             Pexp_function
               ([{ pparam_loc = __dt__.migrate_location_t __dt__ __inh__ loc
                 ; pparam_desc = Pparam_val (__dt__.migrate_arg_label __dt__ __inh__ v_0,
                                             __dt__.migrate_option __dt__.migrate_expression __dt__ __inh__ v_1,
                                             __dt__.migrate_pattern __dt__ __inh__ v_2)}],
                None,
                Pfunction_body (__dt__.migrate_expression __dt__ __inh__ v_3)
               )
        }
      ; migrate_type_immediacy = {
          srctype = [%typ: type_immediacy_t]
        ; dsttype = [%typ: DST.type_immediacy_t]
        }
      ; migrate_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.out_type]
        ; custom_branches_code = function
          | Otyp_arrow (v_0, v_1, v_2) ->
             let open DST in
             Otyp_arrow
               ((match v_0 with "" -> Nolabel | s -> Labelled s),
                (fun __dt__ __inh__ -> __dt__.migrate_out_type __dt__ __inh__)
                  __dt__ __inh__ v_1,
                (fun __dt__ __inh__ -> __dt__.migrate_out_type __dt__ __inh__)
                  __dt__ __inh__ v_2)
        }
      ; migrate_out_type_param = {
          srctype = [%typ: out_type_param]
        ; dsttype = [%typ: DST.out_type_param]
        ; code = fun __dt__ __inh__ (v_0, (v_1, v_2)) ->
            let open DST in
            { ot_non_gen = false
            ; ot_name = v_0
            ; ot_variance = (__dt__.migrate_variance __dt__ __inh__ v_1,
                             __dt__.migrate_injectivity __dt__ __inh__ v_2)
            }
        }
      ; migrate_out_class_type = {
          srctype = [%typ: out_class_type]
        ; dsttype = [%typ: DST.out_class_type]
        ; custom_branches_code = function
          | Octy_arrow (v_0, v_1, v_2) ->
             let open DST in
             Octy_arrow
               ((match v_0 with "" -> Nolabel | s -> Labelled s),
                (fun __dt__ __inh__ -> __dt__.migrate_out_type __dt__ __inh__)
                  __dt__ __inh__ v_1,
                (fun __dt__ __inh__ ->
                  __dt__.migrate_out_class_type __dt__ __inh__)
                  __dt__ __inh__ v_2)
        }
      }
    }
]
