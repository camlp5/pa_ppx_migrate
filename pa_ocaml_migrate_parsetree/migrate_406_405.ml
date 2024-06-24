(**pp -syntax camlp5o $(IMPORT_OCAMLCFLAGS) *)
[@@@ocaml.warning "@partial-match"]
module SRC = Reorg_ast.Ast_4_06
module DST = Reorg_ast.Ast_4_05

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

[%%import: Migrate_406_405.attribute]
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
        srcmod = Reorg_ast.Ast_4_06 ;
        dstmod = DST ;
        types = [
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
        ; constant
        ; constructor_arguments
        ; constructor_declaration
        ; core_type
        ; core_type_desc
        ; expression
        ; expression_desc
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
        ; pattern_desc
        ; payload
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
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open _ ->migration_error __inh__ "Pcty_open"
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.class_expr_desc]
        ; custom_branches_code = function
            | Pcl_open _ -> migration_error __inh__ "Pcl_open"
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
      }
    }
]
