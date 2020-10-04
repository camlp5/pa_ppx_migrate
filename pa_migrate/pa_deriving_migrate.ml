(* camlp5r *)
(* pa_deriving_migrate.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;
open Pa_ppx_utils ;
open Ppxutil ;

value debug = Pa_passthru.debug ;
open Pa_ppx_params.Runtime ;

module Params = struct

module Dispatch1 = struct

value extract_case_branches = fun [
  None -> []
| Some <:expr< fun [ $list:l$ ] >> ->
  List.map (fun (p,wheno,e) ->
      match Patt.unapplist p with [
        (<:patt< $uid:uid$ >>, _) -> (uid, (p, wheno, e))
      | _ -> Ploc.raise (loc_of_patt p) (Failure "extract_case_branches: case-branches must start with a UIDENT")
      ]) l
]
;

type tyarg_t = {
  srctype : ctyp
; dsttype : ctyp
; dstmodule : option longid
; inherit_code : option expr
; code : option expr
; custom_branches_code : option expr 
; custom_branches : (alist lident case_branch) [@computed extract_case_branches custom_branches_code;]
; custom_fields_code : (alist lident expr) [@default [];]
; skip_fields : list lident [@default [];]
; subs : list (ctyp * ctyp) [@default [];]
} [@@deriving params;] ;
type t = (alist lident tyarg_t) [@@deriving params;] ;
end
;
module Migrate = struct

type default_dispatcher_t = {
  srcmod : longid
; dstmod : longid
; types : list lident
; inherit_code : (alist lident expr) [@default [];]
} [@@deriving params;]
;

type t = {
  inherit_type : option ctyp
; dispatch_type : lident
; dispatch_table_constructor : lident
; dispatchers : Dispatch1.t [@default [];]
; default_dispatchers : list default_dispatcher_t [@default [];]
} [@@deriving params {
    formal_args = {
      t = [ type_decls ]
    }
  };]
;

end
;

end
;

module Dispatch1 = struct
type tyarg_t = {
  name: string
; srctype : ctyp
; dsttype : ctyp
; dstmodule : option longid
; inherit_code : option expr
; code : option expr
; custom_branches_code : list (string * MLast.case_branch)
; custom_fields_code : list (string * MLast.expr)
; skip_fields : list string
; type_vars : list string
; subs : list (ctyp * ctyp)
; subs_types : list ctyp
} ;
type t = (string * tyarg_t) ;

value tyvars t =
  let rec trec acc = fun [
    <:ctyp< $t1$ $t2$ >> -> trec (trec acc t1) t2
  | <:ctyp< ' $id$ >> -> [ id :: acc ]
  | _ -> acc
  ] in
  Std.uniquize(trec [] t)
;

value to_type (_, t) =
  let loc = loc_of_ctyp t.srctype in
  let rhs = <:ctyp< migrater_t 'aux $t.srctype$ $t.dsttype$ >> in
  let rhs = List.fold_right (fun subty rhs -> <:ctyp< $subty$ -> $rhs$ >>) t.subs_types rhs in
  if t.type_vars = [] then rhs else
  <:ctyp< ! $list:t.type_vars$ . $rhs$ >>
;

value convert_subs loc e =
  let rec crec acc = fun [
    <:expr< [] >> -> List.rev acc
  | <:expr< [ ( [%typ: $type:t1$], [%typ: $type:t2$] ) :: $tl$ ] >> ->
    crec [ (t1, t2) :: acc ] tl
  ] in
  crec [] e
;

value convert_field_name_list loc e =
  convert_down_list_expr
    (fun [ <:expr< $lid:f$ >> -> f
         | _ -> Ploc.raise (loc_of_expr e) (Failure Fmt.(str "convert_field_name_list: malformed list %a"
                                                           Pp_MLast.pp_expr e))
         ])
    e
;

value convert_tyarg loc type_decls name targ =
  let srctype = targ.Params.Dispatch1.srctype in
  let dsttype = targ.Params.Dispatch1.dsttype in
  let dstmodule = match targ.Params.Dispatch1.dstmodule with [
    Some _ as z -> z
  | None ->
    match Ctyp.unapplist dsttype with [
      (<:ctyp:< $longid:li$ . $lid:_$ >>, _) -> Some li
    | _ -> None
    ]
  ] in
  let code = targ.Params.Dispatch1.code in
  let inherit_code = targ.Params.Dispatch1.inherit_code in
  let custom_branches = targ.Params.Dispatch1.custom_branches in
  let custom_fields_code = targ.Params.Dispatch1.custom_fields_code in
  let skip_fields = targ.Params.Dispatch1.skip_fields in
  let subs = targ.Params.Dispatch1.subs in
  let type_vars = Std.uniquize((tyvars srctype)@(tyvars dsttype)@
                          List.concat (List.map (fun (a,b) -> (tyvars a)@(tyvars b)) subs)) in
  let subs_types = List.map (fun (a,b) -> <:ctyp< (migrater_t 'aux $a$ $b$) >>) subs in
  { name = name
  ; srctype = srctype
  ; dsttype = dsttype
  ; dstmodule = dstmodule
  ; code = code
  ; inherit_code = inherit_code
  ; custom_branches_code = custom_branches
  ; custom_fields_code = custom_fields_code
  ; skip_fields = skip_fields
  ; subs = subs
  ; type_vars = type_vars
  ; subs_types = subs_types
  }
;

value convert loc type_decls (fname, tyargs) =
  (fname, convert_tyarg loc type_decls fname tyargs)
;

value expr_wrap_dsttype_module d e =
  match d.dstmodule with [
    None -> e
  | Some li ->
    let loc = loc_of_expr e in
    <:expr< let open $module_expr_of_longident li$ in $e$ >>
  ]
;

value patt_wrap_dsttype_module d p =
  match d.dstmodule with [
    None -> p
  | Some li ->
    let loc = loc_of_patt p in
    <:patt< $longid:li$ . $p$ >>
  ]
;

end
;

value pmatch pat ty =
  let rec pmrec acc = fun [
    (t1, t2) when Reloc.eq_ctyp t1 t2 -> acc
  | (<:ctyp< $lid:id$ >>, <:ctyp< $lid:id2$ >>) when id = id2 -> acc
  | (<:ctyp< $p1$ $p2$ >>, <:ctyp< $t1$ $t2$ >>) ->
    pmrec (pmrec acc (p1, t1)) (p2, t2)
  | (<:ctyp< ' $id$ >>, ty) ->
    if List.mem_assoc id acc then
      Ploc.raise (loc_of_ctyp pat) (Failure "polymorphic type-variables in patterns must not be repeated")
    else
      [ (id, ty) :: acc ]
  | _ -> failwith "caught"
  ]
  in
  if Reloc.eq_ctyp pat ty then Some []
  else
    match pmrec [] (pat, ty) with [
      rho -> Some rho
    | exception Failure "caught" -> None
    ]
;

module Prettify = struct

type t = {
  lhs : MLast.ctyp
; rhs : MLast.ctyp
}
;

value mk1 (_, td) =
    let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let vars = List.map (fun [
      (<:vala< Some v >>, _) -> v
    | _ -> Ploc.raise loc (Failure Fmt.(str "Prettify.mk1: cannot make prettify rule from type_decl %s: unnamed polymorphic type variables" name))
    ]) (uv td.tdPrm) in
  let lhs =
    Ctyp.applist <:ctyp< $lid:name$ >> (List.map (fun s -> <:ctyp< ' $s$ >>) vars) in
  match td.tdDef with [
    <:ctyp:< $rhs$ == $_$ >> ->
    (name, { lhs = lhs ; rhs = rhs })
  | rhs when not (is_generative_type rhs) ->
    (name, { lhs = lhs ; rhs = rhs })
  | _ -> Ploc.raise loc (Failure Fmt.(str "Prettify.mk1: cannot make prettify rule from type_decl %s: not a manifest type_decl" name))
  ]
;

value mk_from_type_decls tdl =
  List.fold_right (fun td acc ->
      match mk1 td with [
        p -> [p::acc]
      | exception Ploc.Exc _ _ -> acc
      ]) tdl []
;

value prettify rules t =
  let rec prec t =
    match (t, List.find_map (fun (_, r) -> t |> pmatch r.lhs |> Std.map_option (fun rho -> (r, rho))) rules) with [
      (_, Some (r, rho)) ->
      let rho = List.map (fun (v, subt) -> (v, prec subt)) rho in
      prec (Ctyp.subst rho r.rhs)
    | (<:ctyp:< $t1$ $t2$ >>, None) ->
      <:ctyp< $prec t1$ $prec t2$ >>
    | (<:ctyp:< $t1$ -> $t2$ >>, None) ->
      <:ctyp< $prec t1$ -> $prec t2$ >>
    | (<:ctyp:< ( $list:l$ ) >>, None) ->
      <:ctyp:< ( $list:List.map prec l$ ) >>
    | (t, _) -> t
    ]
  in prec t
;
end
;
module Migrate = struct

type t = {
  inherit_type : option MLast.ctyp
; dispatch_type_name : string
; dispatch_table_constructor : string
; dispatchers : list Dispatch1.t
; type_decls : list (string * MLast.type_decl)
; pretty_rewrites : list (string * Prettify.t)
} ;

value dispatch_table_type_decls loc t =
  let ltl = List.map (fun (dispatcher_name, d) ->
      let ty = Dispatch1.to_type (dispatcher_name, d) in
      let ty = Prettify.prettify t.pretty_rewrites ty in
      (loc_of_ctyp ty, dispatcher_name, False, ty, <:vala< [] >>)
    ) t.dispatchers in
  let aux = (loc, "aux", False, <:ctyp< 'aux >>, <:vala< [] >>) in
  let dispatch_table_type = <:ctyp< { $list:[aux :: ltl]$ } >> in
  let migrater_type = match t.inherit_type with [
    None -> <:ctyp< $lid:t.dispatch_type_name$ 'aux -> 'a -> 'b >>
  | Some inhty -> <:ctyp< $lid:t.dispatch_type_name$ 'aux -> $inhty$ -> 'a -> 'b >>
  ] in
  [ <:type_decl< $lid:t.dispatch_type_name$ 'aux = $dispatch_table_type$ >> ;
    <:type_decl< migrater_t 'aux 'a 'b = $migrater_type$ >> ]
;

value dispatch_table_expr loc t =
  let lel = List.map (fun (dispatcher_name, t) ->
      (<:patt< $lid:dispatcher_name$ >>, <:expr< $lid:dispatcher_name$ >>)
    ) t.dispatchers in
  let aux = (<:patt< aux >>, <:expr< aux >>) in
  <:expr< fun aux -> { $list:[aux :: lel]$ } >>
;

value must_subst_lid (srclid, dstlid) li =
  let rec srec li =
    if Reloc.eq_longid srclid li then
      dstlid
    else match li with [
      <:extended_longident:< $longid:li$ . $uid:uid$ >> -> <:longident< $longid:srec li$ . $uid:uid$ >>
    | <:extended_longident:< $longid:_$ ( $longid:_$ ) >> ->
        Ploc.raise loc (Failure Fmt.(str "must_subst_lid: unexpected -extended- longid seen: %a"
                                       Pp_MLast.pp_longid li))
    | _ -> li
    ]
  in srec li
;

value must_subst_lid_in_ctyp (srclid, dstlid) ty =
  match Ctyp.unapplist ty with [
    (<:ctyp:< $longid:li$ . $lid:lid$ >>, args) ->
    let li = must_subst_lid (srclid, dstlid) li in
    Ctyp.applist <:ctyp< $longid:li$ . $lid:lid$ >> args
  | _ ->
    Ploc.raise (loc_of_ctyp ty)
      (Failure Fmt.(str "must_subst_lid_in_ctyp: the manifest type must be module-qualified:@ %a"
                      Pp_MLast.pp_ctyp ty))
  ]
;

value fresh_tyv_args suffix ty =
  let (ty0, args) = Ctyp.unapplist ty in
  let args = List.map (fun [
      <:ctyp:< ' $id$ >> ->
      let id = id ^ suffix in
      <:ctyp:< ' $id$ >>
    | _ -> Ploc.raise (loc_of_ctyp ty)
        (Failure Fmt.(str "fresh_tyv_args: can only apply to args that are type-variables:@ %a"
                        Pp_MLast.pp_ctyp ty))
    ]) args in
  Ctyp.applist ty0 args
;

value generate_dsttype loc (srclid, dstlid) td =
  let ty = match td.tdDef with [
    <:ctyp< $t1$ == $_$ >> -> t1
  | _ ->
    let tname = td.tdNam |> uv |> snd |> uv in
    let loc = loc_of_ctyp td.tdDef in
    <:ctyp< $longid:srclid$ . $lid:tname$ >>
  ] in
  must_subst_lid_in_ctyp (srclid, dstlid) ty
;

value generate_srctype loc dsttype tyid =
  let (_, args) = Ctyp.unapplist dsttype in
  Ctyp.applist <:ctyp:< $lid:tyid$ >> args
;

value generate_default_dispatcher loc type_decls (tyid,dd) td =
  let srcmod = dd.Params.Migrate.srcmod in
  let dstmod = dd.Params.Migrate.dstmod in
  let dsttype = generate_dsttype (loc_of_type_decl td) (srcmod, dstmod) td in
  let srctype = generate_srctype (loc_of_type_decl td) dsttype tyid in
  let dsttype = fresh_tyv_args "1" dsttype in
  let srctype = fresh_tyv_args "0" srctype in
  let subs = List.map2 (fun t1 t2 -> (t1, t2))
      (snd (Ctyp.unapplist srctype)) (snd (Ctyp.unapplist dsttype)) in
  let rwname = Printf.sprintf "migrate_%s" tyid in
  let inherit_code = match List.assoc tyid dd.Params.Migrate.inherit_code with [
    e -> Some e
  | exception Not_found -> None
  ] in
  Dispatch1.convert loc type_decls
    (rwname,
     let open Params.Dispatch1 in {
       srctype = srctype
     ; dsttype = dsttype
     ; dstmodule = None
     ; inherit_code = inherit_code
     ; code = None
     ; custom_branches_code = None
     ; custom_branches = []
     ; custom_fields_code = []
     ; skip_fields = []
     ; subs = subs
     })
;

value build_default_dispatchers loc type_decls dd =
 let types = dd.Params.Migrate.types in
 let inherit_code = dd.Params.Migrate.inherit_code in
  if not (Std.subset (List.map fst inherit_code) types) then
    let extras = Std.subtract (List.map fst inherit_code) types in
    Ploc.raise loc (Failure Fmt.(str "build_default_dispatchers: extra members of inherit_code: %a"
                                   (list ~{sep=sp} string) extras))
  else
  List.map (fun tyid ->
    match List.assoc tyid type_decls with [
      td ->
        generate_default_dispatcher (loc_of_type_decl td) type_decls (tyid, dd) td
      | exception Not_found -> Ploc.raise loc (Failure Fmt.(str "build_default_dispatchers: type %s not declared" tyid))
    ]) types
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let optarg =
    let l = List.map (fun (k, e) -> (<:patt< $lid:k$ >>, e)) (Ctxt.options ctxt) in
    <:expr< { $list:l$ } >> in
  let rc0 = Params.Migrate.params type_decls optarg in
  let dispatchers = List.map (Dispatch1.convert loc type_decls) rc0.Params.Migrate.dispatchers in
  let more_dispatchers = List.concat (List.map (build_default_dispatchers loc type_decls)
                                        rc0.Params.Migrate.default_dispatchers) in
  let dispatchers = List.sort
      (fun (n1,_) (n2,_) -> Stdlib.compare n1 n2)
      (dispatchers@more_dispatchers) in
  let repeated_dispatcher_names = Std2.hash_list_repeats (List.map fst dispatchers) in
  let sorted_repeated_dispatcher_names = List.sort Stdlib.compare repeated_dispatcher_names in
  if [] <> repeated_dispatcher_names then
    Ploc.raise loc (Failure Fmt.(str "pa_deriving.migrate: dispatchers defined more than once: %a"
                                   (list ~{sep=sp} string) sorted_repeated_dispatcher_names))
  else
  let pretty_rewrites = Prettify.mk_from_type_decls type_decls in
  {
    inherit_type = rc0.Params.Migrate.inherit_type ;
    dispatch_type_name = rc0.Params.Migrate.dispatch_type;
    dispatch_table_constructor = rc0.Params.Migrate.dispatch_table_constructor;
    dispatchers = dispatchers ;
    type_decls = type_decls ;
    pretty_rewrites = pretty_rewrites
  }
;


value reduce1 (id, tyargs) td = do {
  if List.length tyargs <> List.length (uv td.tdPrm) then
    Ploc.raise (loc_of_type_decl td) (Failure "actual/formal length mismatch")
  else () ;
  let rho = List.map2 (fun formal actual ->
      match formal with [
        ( <:vala< Some tyv >>, _ ) -> (tyv, actual)
      | _ -> Ploc.raise (loc_of_type_decl td) (Failure "pa_deriving.migrate: blank formal type-variables are not supported")
      ]
    ) (uv td.tdPrm) tyargs in
  let rho = Std.filter (fun [
      (id, <:ctyp< ' $id2$ >>) when id = id2 -> False
    | _ -> True
    ]) rho in
  let rhs = match td.tdDef with [
    <:ctyp< $t1$ == $t2$ >> -> t2
  | t -> t
  ] in
  if rho = [] then rhs else
    Ctyp.subst rho rhs
}
;

value head_reduce1 t ty =
  match Ctyp.unapplist ty with [
    (<:ctyp< $lid:id$ >>, tyargs) when List.mem_assoc id t.type_decls ->
    let td = List.assoc id t.type_decls in
    reduce1 (id, tyargs) td
  | _ -> ty
  ]
;

value match_migrate_rule ~{except} t ctyp =
  List.find_map (fun (dname, t) ->
    if (Some dname) = except then None else
     ctyp
     |> (fun r -> pmatch t.Dispatch1.srctype r)
     |> Std.map_option (fun rho -> (t, rho))
  ) t.dispatchers
;

(** strategy for generating a migrater.

(1) start with srctype

(2) reduce it; if you get no change, it's a failure

(3) if it matches anything other than the current migrate-dispatcher rule, apply that.

(4) otherwise, keep reducing until you get TySum or TyRec

(5) Take the dsttype's module-prefix and use it

(6) And generate a copy-expression

*)

value rec match_or_head_reduce loc ~{except} t ty =
  match (except, match_migrate_rule ~{except=except} t ty) with [
    (_, Some (d, rho)) -> Left (d, rho)
  | (Some dname, None) ->
    let ty' = head_reduce1 t ty in
    if Reloc.eq_ctyp ty ty' then
      match ty with [
        (<:ctyp< [ $list:_$ ] >> | <:ctyp< { $list:_$ } >> | <:ctyp< ( $list:_$ ) >> | <:ctyp< ' $_$ >> | <:ctyp< $lid:_$ >>) -> Right (dname, ty)

      | _ -> Ploc.raise loc (Failure Fmt.(str "migrate rule %s: cannot migrate srctype %a" dname Pp_MLast.pp_ctyp ty))
      ]
    else
      match_or_head_reduce loc ~{except=except} t ty'
  | (None, None) ->
    Ploc.raise loc (Failure Fmt.(str "match_or_head_reduce: cannot head-reduce except at toplevel of a dispatcher's srctype: %a" Pp_MLast.pp_ctyp ty))
  ]
;

value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;
value builtin_copy_types =
  let loc = Ploc.dummy in
  List.map canon_ctyp [
    <:ctyp< string >>
  ; <:ctyp< int >>
  ; <:ctyp< int32 >>
  ; <:ctyp< int64 >>
  ; <:ctyp< nativeint >>
  ; <:ctyp< float >>
  ; <:ctyp< bool >>
  ; <:ctyp< char >>
  ]
;
value id_expr t =
  let loc = Ploc.dummy in
  match t.inherit_type with [
    None -> <:expr< (fun __dt__ x -> x) >>
  | Some _ -> <:expr< (fun __dt__ __inh__ x -> x) >>
  ]
;

value app_dt t e =
  let loc = loc_of_expr e in
  match t.inherit_type with [
    None -> <:expr< $e$ __dt__ >>
  | Some _ -> <:expr< $e$ __dt__ __inh__ >>
  ]
;

value abs_dt t e =
  let loc = loc_of_expr e in
  match t.inherit_type with [
    None -> <:expr< fun __dt__ -> $e$ >>
  | Some _ -> <:expr< fun __dt__ __inh__ -> $e$ >>
  ]
;

value rec generate_leaf_dispatcher_expression t d subs_rho = fun [
  <:ctyp:< [ $list:branches$ ] >> ->
  let ll = List.map (fun [
      <:constructor< $uid:uid$ of $list:tyl$ >> ->
      let custom_branches = Std.filter (fun (n, _) -> uid = n) d.Dispatch1.custom_branches_code in
      if custom_branches <> [] then
        List.map snd custom_branches
      else
      let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
      let patt = List.fold_left (fun p (v,_) -> <:patt< $p$ $lid:v$ >>) <:patt< $uid:uid$ >> argvars in
      let expr = List.fold_left (fun e (v,ty) ->
          let sub_rw = generate_dispatcher_expression ~{except=None} t subs_rho ty in
          <:expr< $e$ ($app_dt t (fst sub_rw)$ $lid:v$) >>
        ) <:expr< $uid:uid$ >> argvars in
      [(patt, <:vala< None >>, Dispatch1.expr_wrap_dsttype_module d expr)]
    ]) branches in
  let l = List.concat ll in
  <:expr< fun [ $list:l$ ] >>
| <:ctyp:< { $list:ltl$ } >> ->
    let patt =
      let lpl = List.map (fun (_, lid, _, _, _) ->
          (<:patt< $lid:lid$ >>, <:patt< $lid:lid$ >>)
        ) ltl in
      <:patt< { $list:lpl$ } >> in
    let expr =
      let trimmed_ltl = Std.filter (fun (_, lid, _, _, _) -> not (List.mem lid d.Dispatch1.skip_fields)) ltl in 
      let trimmed_lel = List.map (fun  (_, lid, _, ty, _) ->
          let sub_rw = generate_dispatcher_expression ~{except=None} t subs_rho ty in
          (Dispatch1.patt_wrap_dsttype_module d <:patt< $lid:lid$ >>, <:expr< $app_dt t (fst sub_rw)$ $lid:lid$ >>)
        ) trimmed_ltl in
      let full_lel = trimmed_lel @ (
          List.map (fun (lid, e) ->
              (Dispatch1.patt_wrap_dsttype_module d <:patt< $lid:lid$ >>, e)) d.Dispatch1.custom_fields_code) in 
      <:expr< { $list:full_lel$ } >> in
    let expr = match d.Dispatch1.inherit_code with [
      None -> expr
    | Some inhexp -> <:expr< let __inh__ = $inhexp$ in $expr$ >>
    ] in
    <:expr< fun [ $patt$ -> $expr$ ] >>
| <:ctyp:< ( $list:tyl$ ) >> ->
    let patt =
      let pl = List.mapi (fun i ty ->
          let lid = Printf.sprintf "v_%d" i in
          <:patt< $lid:lid$ >>) tyl in
      <:patt< ( $list:pl$ ) >> in
    let expr =
      let el = List.mapi (fun i ty ->
          let lid = Printf.sprintf "v_%d" i in
          let sub_rw = generate_dispatcher_expression ~{except=None} t subs_rho ty in
          <:expr< $app_dt t (fst sub_rw)$ $lid:lid$ >>
        ) tyl in
      <:expr< ( $list:el$ ) >> in
    <:expr< fun [ $patt$ -> $expr$ ] >>
]

and generate_dispatcher_expression ~{except} t subs_rho ty = 
  if AList.mem ~{cmp=Reloc.eq_ctyp} ty subs_rho then
    let (f_sub, f_result_ty) = AList.assoc ~{cmp=Reloc.eq_ctyp} ty subs_rho in
    let loc = loc_of_ctyp ty in
    (<:expr< $lid:f_sub$ >>, f_result_ty)
  else if List.mem (canon_ctyp ty) builtin_copy_types then
    (id_expr t, ty)
  else match ty with [
    <:ctyp:< ( $list:tyl$ ) >> ->
      let patt =
        let pl = List.mapi (fun i ty ->
            let lid = Printf.sprintf "v_%d" i in
            <:patt< $lid:lid$ >>) tyl in
        <:patt< ( $list:pl$ ) >> in
      let exprs_types = List.mapi (fun i ty ->
            let lid = Printf.sprintf "v_%d" i in
            let sub_rw = generate_dispatcher_expression ~{except=None} t subs_rho ty in
            (<:expr< $app_dt t (fst sub_rw)$ $lid:lid$ >>, snd sub_rw)
          ) tyl in
      let expr =
        let el = List.map fst exprs_types in
        <:expr< ( $list:el$ ) >> in
      let rhsty =
        let tyl = List.map snd exprs_types in
        <:ctyp< ( $list:tyl$ ) >> in
      (abs_dt t <:expr< fun [ $patt$ -> $expr$ ] >>, rhsty)
    | _ ->
      generate_tycon_dispatcher_expression ~{except=except} t subs_rho ty
  ]

and generate_tycon_dispatcher_expression ~{except} t subs_rho ty = 
  let loc = loc_of_ctyp ty in
  match match_or_head_reduce loc ~{except=except} t ty with [
    Left (rwd, lrho) ->
    (** [rwd] is the migrate dispatcher that matched,
        and [lrho] is the substitution generated by the match. *)
    let (revsubs, rrho) = List.fold_left (fun (revsubs, rrho) (lhsty, rhsty) ->
        let conc_lhsty = Ctyp.subst lrho lhsty in
        let (e, conc_rhsty) = generate_dispatcher_expression ~{except=None} t subs_rho conc_lhsty in
        let add_rrho = match pmatch rhsty conc_rhsty with [
          None -> Ploc.raise (loc_of_ctyp ty) (Failure "generate_dispatcher_expression: subterm dispatch returned non-matching type")
        | Some rho -> rho
        ] in
        ([ e :: revsubs ], Env.append (loc_of_ctyp ty) rrho add_rrho)
      ) ([], []) rwd.Dispatch1.subs in
    let dname = rwd.Dispatch1.name in
    let loc = loc_of_ctyp ty in
    let e = Expr.applist <:expr< __dt__ . $lid:dname$ >> (List.rev revsubs) in
    let e = abs_dt t (app_dt t e) in
    (e, Ctyp.subst rrho rwd.Dispatch1.dsttype)

  | Right (dname, headredty) ->
    let d = List.assoc dname t.dispatchers in

    if List.mem (canon_ctyp headredty) builtin_copy_types then
      (id_expr t, d.Dispatch1.dsttype)
    else
      let e = generate_leaf_dispatcher_expression t d subs_rho headredty in
      let e = abs_dt t e in
      (e, d.Dispatch1.dsttype)
  ]
;

value toplevel_generate_dispatcher t (dname,d) = do {
  if debug.val then
    Fmt.(pf stderr "[toplevel_generate_dispatcher: %s]\n%!" dname)
  else () ;
  match d.Dispatch1.code with [
    Some e -> e
  | None ->
    let srctype = d.Dispatch1.srctype in
    let loc = loc_of_ctyp srctype in
    let subs_rho = List.mapi (fun i (lhsty, rhsty) -> (lhsty, (Printf.sprintf "__subrw_%d" i, rhsty))) d.Dispatch1.subs in
    let subs_binders = List.map2 (fun (_,(v, _)) ty -> <:patt< ( $lid:v$ : $ty$ ) >>) subs_rho d.Dispatch1.subs_types in
    let (e, t) = generate_dispatcher_expression ~{except=Some dname} t subs_rho srctype in
    let loc = loc_of_expr e in
    List.fold_right (fun p rhs -> <:expr< fun $p$ -> $rhs$ >>) subs_binders e
  ]
}
;
end ;

value str_item_gen_migrate name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = Migrate.build_context loc arg tdl in
    let dispatch_type_decls = Migrate.dispatch_table_type_decls loc rc in
    let dispatch_table_constructor_expression = Migrate.dispatch_table_expr loc rc in
    let migrate_dispatcher_decls = List.map (fun (dname,d) ->
        let e = Migrate.toplevel_generate_dispatcher rc (dname, d) in
        (<:patt< $lid:dname$ >>, e, <:vala< [] >>)
      ) rc.Migrate.dispatchers in
    let si0 = <:str_item< value $list:migrate_dispatcher_decls$ >> in
    let si1 = <:str_item< value $lid:rc.Migrate.dispatch_table_constructor$ = $dispatch_table_constructor_expression$ >> in
  <:str_item< declare type $list:dispatch_type_decls$ ; $si0$ ; $si1$ ; end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "migrate"
; alternates = []
; options = ["optional"; "default_dispatchers"; "dispatchers"; "dispatch_type"; "dispatch_table_constructor"; "inherit_type"]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>) 
  ; ("default_dispatchers", <:expr< [] >>) 
  ]
; alg_attributes = ["nobuiltin"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_migrate
; sig_item = (fun arg e -> assert False)
})
;

