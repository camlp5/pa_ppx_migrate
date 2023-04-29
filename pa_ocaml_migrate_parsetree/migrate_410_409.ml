(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
module SRC = Reorg_ast.Ast_4_10
module DST = Reorg_ast.Ast_4_09

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

[%%import: Migrate_410_409.attribute]
[@@deriving migrate
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Reorg_ast.Ast_4_10
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = Reorg_ast.Ast_4_10
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
        srcmod = Reorg_ast.Ast_4_10
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
      }
      ; {
        srcmod = Reorg_ast.Ast_4_10
      ; dstmod = DST
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
        ; out_sig_item
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
        ; dsttype = [%typ: DST.pattern_desc]
        ; custom_branches_code = function
            | Ppat_unpack v_0 ->
              let v_0 = map_loc (function Some x -> x
                                        | None -> migration_error __inh__ "Ppat_unpack") v_0 in
              let open DST in
              Ppat_unpack
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0)
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.expression_desc]
        ; custom_branches_code = function
            | Pexp_letmodule (v_0, v_1, v_2) ->
              let v_0 = map_loc (function Some x -> x
                                        | None -> migration_error __inh__ "Pexp_letmodule") v_0 in
              let open DST in
              Pexp_letmodule
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                 __dt__.migrate_module_expr __dt__ __inh__ v_1,
                 __dt__.migrate_expression __dt__ __inh__ v_2)
        }
      ; migrate_module_type_desc = {
          srctype = [%typ: module_type_desc]
        ; dsttype = [%typ: DST.module_type_desc]
        ; custom_branches_code = function
            | Pmty_functor (Unit, v_2) ->
              let open DST in
              Pmty_functor
              (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ "*"),
               None,
               __dt__.migrate_module_type __dt__ __inh__ v_2)
            | Pmty_functor (Named(v_0, v_1), v_2) ->
              let v_0 = map_loc (function Some x -> x
                                        | None -> migration_error __inh__ "Pmty_functor") v_0 in
              let open DST in
              Pmty_functor
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                 Some (__dt__.migrate_module_type __dt__ __inh__ v_1),
                 __dt__.migrate_module_type __dt__ __inh__ v_2)
        }
      ; migrate_module_declaration = {
          srctype = [%typ: module_declaration]
        ; dsttype = [%typ: DST.module_declaration]
        ; inherit_code = Some pmd_loc
        ; skip_fields = [ pmd_name ]
        ; custom_fields_code = {
            pmd_name =
              let pmd_name = map_loc (function Some x -> x
                                             | None -> migration_error __inh__ "pmd_name") pmd_name in
              __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ pmd_name
          }
        }
      ; migrate_module_expr_desc = {
          srctype = [%typ: module_expr_desc]
        ; dsttype = [%typ: DST.module_expr_desc]
        ; custom_branches_code = function
            | Pmod_functor (Unit, v_2) ->
              let open DST in
              Pmod_functor
              (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ "*"),
               None,
               __dt__.migrate_module_expr __dt__ __inh__ v_2)
            | Pmod_functor (Named(v_0, v_1), v_2) ->
              let v_0 = map_loc (function Some x -> x
                                        | None -> migration_error __inh__ "Pmod_functor") v_0 in
              let open DST in
              Pmod_functor
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ v_0,
                 Some (__dt__.migrate_module_type __dt__ __inh__ v_1),
                 __dt__.migrate_module_expr __dt__ __inh__ v_2)
        }
      ; migrate_module_binding = {
          srctype = [%typ: module_binding]
        ; dsttype = [%typ: DST.module_binding]
        ; skip_fields = [ pmb_name ]
        ; custom_fields_code = {
            pmb_name =
              let pmb_name = map_loc (function Some x -> x
                                             | None -> migration_error __inh__ "pmb_name") pmb_name in
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
        ; dsttype = [%typ: DST.out_module_type]
        ; custom_branches_code = function
            | Omty_functor (farg, v_2) ->
              let (v_0, v_1) = match farg with
                  None -> ("*", None)
                | Some (None, mt) -> ("_", Some mt)
                | Some (Some s, mt) -> (s, Some mt) in
              let open DST in
              Omty_functor
                (v_0,
                 Option.map (__dt__.migrate_out_module_type __dt__ __inh__) v_1,
                 __dt__.migrate_out_module_type __dt__ __inh__ v_2)
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.out_type_decl]
        ; skip_fields = [ otype_immediate ]
        ; custom_fields_code = {
            otype_immediate = match otype_immediate with
                Always -> true
              | Unknown -> false
              | _ -> migration_error __inh__ "otype_immediate"
          }
        }
      }
    }
]
