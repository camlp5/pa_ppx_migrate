(**pp -syntax camlp5r -package camlp5.parser_quotations *)
(* pa_deriving_migrate.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;
open Pa_ppx_utils ;
open Ppxutil ;

value debug = Pa_passthru.debug ;
open Pa_ppx_params_runtime.Runtime ;

open MLPrinters.OP.Pretty ;

value pmatch pat ty =
  let rec pmrec acc = fun [
    (t1, t2) when Reloc.eq_ctyp t1 t2 -> acc
  | (<:ctyp< $lid:id$ >>, <:ctyp< $lid:id2$ >>) when id = id2 -> acc
  | (<:ctyp< $p1$ $p2$ >>, <:ctyp< $t1$ $t2$ >>) ->
    pmrec (pmrec acc (p1, t1)) (p2, t2)
  | (<:ctyp:< ' $s$ >>, ty) ->
    if List.mem_assoc s acc then
      Fmt.(raise_failwithf loc "polymorphic type-variables in a pattern must not be repeated: %s" s)
    else
      [ (s, ty) :: acc ]
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

value type_var_to_type loc = fun [
      (<:vala< Some v >>, _) -> <:ctyp< ' $v$ >>
    | _ -> raise_failwith loc "type_var_to_type: cannot accept unnamed polymorphic type variable"
]
;

module Prettify = struct

type t = {
  lhs : MLast.ctyp
; rhs : MLast.ctyp
}
;
value pp pps {lhs=lhs;rhs=rhs} =
    Fmt.(pf pps "{lhs=%a; rhs=%a}" pp_ctyp lhs pp_ctyp rhs) ;

value mk1 (_, td) =
    let loc = loc_of_type_decl td in
  let name = td.tdNam |> uv |> snd |> uv in
  let lhs =
    Ctyp.applist <:ctyp< $lid:name$ >> (List.map (type_var_to_type loc) (uv td.tdPrm)) in
  match td.tdDef with [
    <:ctyp:< $rhs$ == $_$ >> ->
    Some (name, { lhs = lhs ; rhs = rhs })
  | rhs when not (is_generative_type rhs) ->
     Some (name, { lhs = lhs ; rhs = rhs })
  | _ -> None
  ]
;

value mk_from_type_decls tdl =
  List.filter_map mk1 tdl
;

value find_prettify_match rules t =
  List.find_map (fun (_, r) -> t |> pmatch r.lhs |> Std.map_option (fun rho -> (r, rho))) rules ;

value prettify rules t =
  let rec prec t =
    match (t, find_prettify_match rules t) with [
      (_, Some (r, rho)) ->
      let rho = List.map (fun (v, subt) -> (v, prec subt)) rho in
      prec (Ctyp.subst rho r.rhs)
    | (<:ctyp:< $t1$ $t2$ >>, None) ->
      <:ctyp< $prec t1$ $prec t2$ >>
    | (<:ctyp:< $t1$ -> $t2$ >>, None) ->
      <:ctyp< $prec t1$ -> $prec t2$ >>
    | (<:ctyp:< ( $list:l$ ) >>, None) ->
      <:ctyp:< ( $list:List.map prec_tuple l$ ) >>
    | (t, _) -> t
    ]
  and prec_tuple (lab, ct) = (lab, prec ct)
  in prec t
;
end
;

module Dispatch1 = struct

value extract_case_branches = fun [
  None -> []
| Some <:expr< fun [ $list:l$ ] >> ->
  List.map (fun (p,wheno,e) ->
      match Patt.unapplist p with [
        (<:patt< $uid:uid$ >>, _) -> (uid, (p, wheno, e))
      | (<:patt< ` $cid$ >>, _) -> (cid, (p, wheno, e))
      | _ -> Fmt.(raise_failwithf (loc_of_patt p) "extract_case_branches: 'custom_branches_code' case-branches must start with a UIDENT: %a" pp_patt p)
      ]) l
]
;

value tyvars t =
  let rec trec acc = fun [
    <:ctyp< $t1$ $t2$ >> -> trec (trec acc t1) t2
  | <:ctyp< ' $id$ >> -> [ id :: acc ]
  | _ -> acc
  ] in
  Std.uniquize(trec [] t)
;

value compute_type_vars srctype dsttype subs =
  Std.uniquize((tyvars srctype)@(tyvars dsttype)@
               List.concat (List.map (fun (a,b) -> (tyvars a)@(tyvars b)) subs))
;

value compute_subs_types loc subs =
  List.map (fun (a,b) -> <:ctyp< (migrater_t 'aux $a$ $b$) >>) subs ;

value longid_of_dstmodule dsttype = fun [
  Some _ as z -> z
| None ->
  match Ctyp.unapplist dsttype with [
    (<:ctyp:< $longid:li$ . $lid:_$ >>, _) -> Some li
  | _ -> None
  ]
]
;

type tyarg_t = {
  loc : (Ploc.t[@printer fun fmt _ -> Format.fprintf fmt "<loc>";]) [@computed (MLast.loc_of_expr __arg__);]
; srctype : ctyp
; dsttype : ctyp
; manual : bool [@default False;]
; raw_dstmodule : option longid [@name dstmodule;]
; dstmodule : option longid [@computed longid_of_dstmodule dsttype raw_dstmodule;]
; inherit_code : option expr
; code : option expr
; custom_branches_code : option expr 
; custom_branches : (alist lident case_branch) [@computed extract_case_branches custom_branches_code;]
; custom_fields_code : (alist lident expr) [@default [];]
; skip_fields : list lident [@default [];]
; subs : list (ctyp * ctyp) [@default [];]
; type_vars : list string [@computed compute_type_vars srctype dsttype subs;]
; subs_types : list ctyp [@computed compute_subs_types loc subs;]
} [@@deriving (show, params);] ;

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

module Explanation = struct
  type t = [
    Toplevel of string
  | In_tuple of ctyp
  | Tycon of ctyp
  | Rewrite of string
  | Reduced of string and ctyp
  | Constructor of string and ctyp
  | Field of string and ctyp
  ][@@deriving show;];

  value pp1_hum pp_ctyp pps = fun [
    Toplevel s -> Fmt.(pf pps "[toplevel %s]" s)
  | In_tuple ty -> Fmt.(pf pps "[in tuple %a]" pp_ctyp ty)
  | Tycon ty -> Fmt.(pf pps "[tycon %a]" pp_ctyp ty)
  | Rewrite s ->  Fmt.(pf pps "[rewrite %s]" s)
  | Reduced s ty -> Fmt.(pf pps "[reduced %s %a]" s pp_ctyp ty)
  | Constructor s ty -> Fmt.(pf pps "[constructor %s %a]" s pp_ctyp ty)
  | Field s ty -> Fmt.(pf pps "[field %s %a]" s pp_ctyp ty)
  ] ;

  value pp_hum_list pp_ctyp pps l =
    Fmt.(pf pps "%a" (list ~{sep=const string "\n"} (pp1_hum pp_ctyp)) l) ;

  value pp_hum pps l =
    Fmt.(pf pps "#<|%a|>" (pp_hum_list pp_ctyp) l) ;
end
;
(*
exception Migration_exception of (list Explanation.t) and exn ;
 *)
type Pa_ppx_runtime.Exceptions.t +=
       [ Migration_exception of Pa_ppx_runtime.Exceptions.t and (list Explanation.t) ] [@@deriving show;] ;

value raise_migration_exception loc ex exn =
  let ex = [] in
  match exn with [
      Failure msg ->
      let msg = Fmt.(str "%s\n%a" msg Explanation.pp_hum ex) in
      Ploc.raise loc (Failure msg)
    | exn ->
       Ploc.raise loc (Migration_exception exn ex)
    ]
(*
  Ploc.raise loc (Migration_exception exn ex)
 *)
;

module Migrate = struct

open Pa_ppx_params_runtime.Runtime ;

type default_dispatcher_t = {
  loc : (Ploc.t[@printer fun fmt _ -> Format.fprintf fmt "<loc>";]) [@computed (MLast.loc_of_expr __arg__);]
; srcmod : longid
; dstmod : longid
; types : list lident
; inherit_code : (alist lident expr) [@default [];]
} [@@deriving (show,params {
    validators = {
      default_dispatcher_t =
        (fun dd ->
          let extras = Std.subtract (List.map fst dd.inherit_code) dd.types in
          if extras = [] then Result.Ok True
          else Result.Error (dd.loc, Fmt.(str "build_default_dispatchers: extra members of inherit_code: %a"
                                            (list ~{sep=sp} string) extras)))
    }
       });]
;

value must_subst_lid (srclid, dstlid) li =
  let rec srec li =
    if Reloc.eq_longid srclid li then
      Some dstlid
    else match li with [
      <:extended_longident:< $longid:li$ . $uid:uid$ >> ->
        match srec li with [
            None -> None
          | Some li -> Some <:longident< $longid:li$ . $uid:uid$ >>
          ]
    | <:extended_longident:< $longid:_$ ( $longid:_$ ) >> ->
        Fmt.(raise_failwithf loc "must_subst_lid: unexpected -extended- longid seen: %a"
               pp_longident li)
    | _ -> None
    ]
  in srec li
;

value must_subst_lid_in_ctyp (srclid, dstlid) ty =
  match Ctyp.unapplist ty with [
    (<:ctyp:< $longid:li$ . $lid:lid$ >>, args) ->
    match must_subst_lid (srclid, dstlid) li with [
          None -> None
        | Some li -> Some (Ctyp.applist <:ctyp< $longid:li$ . $lid:lid$ >> args)
        ]
  | _ ->
    Fmt.(raise_failwithf (loc_of_ctyp ty)
           "must_subst_lid_in_ctyp: the manifest type must be module-qualified:@ %a"
           pp_ctyp ty)
  ]
;

value fresh_tyv_args suffix ty =
  let (ty0, args) = Ctyp.unapplist ty in
  let args = List.map (fun [
      <:ctyp:< ' $id$ >> ->
      let id = id ^ suffix in
      <:ctyp:< ' $id$ >>
    | _ -> Fmt.(raise_failwithf (loc_of_ctyp ty)
                  "fresh_tyv_args: can only apply to args that are type-variables:@ %a"
                  pp_ctyp ty)
    ]) args in
  Ctyp.applist ty0 args
;

value generate_dsttype loc (srclid, dstlid) td =
  let tname = td.tdNam |> uv |> snd |> uv in
  let loc = loc_of_ctyp td.tdDef in
  let ty = match td.tdDef with [
    <:ctyp< $t1$ == $_$ >> -> t1
  | _ -> 
    Ctyp.applist <:ctyp< $longid:srclid$ . $lid:tname$ >> (List.map (type_var_to_type loc) (uv td.tdPrm))
  ] in
  match must_subst_lid_in_ctyp (srclid, dstlid) ty with [
      Some ty -> ty
    | None -> Ctyp.applist <:ctyp< $longid:dstlid$ . $lid:tname$ >> (List.map (type_var_to_type loc) (uv td.tdPrm))
    ]
;

value generate_srctype loc dsttype tyid =
  let (_, args) = Ctyp.unapplist dsttype in
  Ctyp.applist <:ctyp:< $lid:tyid$ >> args
;

value generate_default_dispatcher loc type_decls (tyid,dd) td =
  let srcmod = dd.srcmod in
  let dstmod = dd.dstmod in
  let dsttype = generate_dsttype (loc_of_type_decl td) (srcmod, dstmod) td in
  let srctype = generate_srctype (loc_of_type_decl td) dsttype tyid in
  let dsttype = fresh_tyv_args "1" dsttype in
  let srctype = fresh_tyv_args "0" srctype in
  let subs = List.map2 (fun t1 t2 -> (t1, t2))
      (snd (Ctyp.unapplist srctype)) (snd (Ctyp.unapplist dsttype)) in
  let rwname = Printf.sprintf "migrate_%s" tyid in
  let inherit_code = match List.assoc tyid dd.inherit_code with [
    e -> Some e
  | exception Not_found -> None
  ] in
  (rwname,
   let open Dispatch1 in {
     loc = dd.loc
   ; srctype = srctype
   ; dsttype = dsttype
   ; manual = False
   ; raw_dstmodule = None
   ; dstmodule = longid_of_dstmodule dsttype None
   ; inherit_code = inherit_code
   ; code = None
   ; custom_branches_code = None
   ; custom_branches = []
   ; custom_fields_code = []
   ; skip_fields = []
   ; subs = subs
   ; type_vars = compute_type_vars srctype dsttype subs
   ; subs_types = compute_subs_types loc subs
   })
;

value build_default_dispatchers loc type_decls dd =
 let types = dd.types in
 let inherit_code = dd.inherit_code in
  if not (Std.subset (List.map fst inherit_code) types) then
    let extras = Std.subtract (List.map fst inherit_code) types in
    Fmt.(raise_failwithf loc "Internal error (please report): build_default_dispatchers: extra members of inherit_code: %a"
           (list ~{sep=sp} string) extras)
  else
  List.map (fun tyid ->
    match List.assoc tyid type_decls with [
      td ->
        generate_default_dispatcher (loc_of_type_decl td) type_decls (tyid, dd) td
      | exception Not_found ->
         Fmt.(raise_failwithf dd.loc "build_default_dispatchers: type %s not declared" tyid)
    ]) types
;

value compute_dispatchers loc type_decls declared_dispatchers default_dispatchers =
  let more_dispatchers = List.concat (List.map (build_default_dispatchers loc type_decls)
                                        default_dispatchers) in
  List.sort
    (fun (n1,_) (n2,_) -> Stdlib.compare n1 n2)
    (declared_dispatchers@more_dispatchers)
;

type t = {
  loc : (Ploc.t[@printer fun fmt _ -> Format.fprintf fmt "<loc>";]) [@computed (MLast.loc_of_expr __arg__);]
; optional : bool
; plugin_name : string
; inherit_type : option ctyp
; default_open_recursion : bool [@default True;]
; open_recursion_dispatchers : list lident [@default [];]
; closed_recursion_dispatchers : list lident [@default [];]
; dispatch_type_name : lident [@name dispatch_type;]
; dispatch_table_constructor : lident
; declared_dispatchers : (alist lident Dispatch1.tyarg_t) [@default [];][@name dispatchers;]
; default_dispatchers : list default_dispatcher_t [@default [];]
; dispatchers : (alist lident Dispatch1.tyarg_t) [@computed compute_dispatchers loc type_decls declared_dispatchers default_dispatchers;]
; type_decls : list (string * (MLast.type_decl[@printer fun fmt _ -> Format.fprintf fmt "<typedecl>";])) [@computed type_decls;]
; pretty_rewrites : list (string * Prettify.t) [@computed Prettify.mk_from_type_decls type_decls;]
} [@@deriving (show,params {
    formal_args = {
      t = [ type_decls ]
    }
  ; validators = { t = fun rc ->
      if rc.default_open_recursion then
        if rc.open_recursion_dispatchers = [] then Result.Ok True
        else Result.Error (rc.loc, "when default_open_recursion is true, open_recursion_dispatchers should be []")
      else if rc.closed_recursion_dispatchers = [] then Result.Ok True
      else Result.Error (rc.loc, "when default_open_recursion is false, closed_recursion_dispatchers should be []")
    }
  });]
;

value dispatcher_open_recursion rc dname =
  rc.default_open_recursion || List.mem dname rc.open_recursion_dispatchers
;

value dispatcher_invocation_expression rc loc dname =
  if dispatcher_open_recursion rc dname then
    <:expr< __dt__ . $lid:dname$ >>
  else <:expr< $lid:dname$ >>
;

value dispatcher_type loc t (dname, d) =
  let ty = Dispatch1.to_type (dname, d) in
  Prettify.prettify t.pretty_rewrites ty
;

value dispatch_table_type_decls loc t =
  let ltl = t.dispatchers |> List.filter_map (fun (dispatcher_name, d) ->
    if dispatcher_open_recursion t dispatcher_name then
      let ty = dispatcher_type loc t (dispatcher_name, d) in
      Some (loc_of_ctyp ty, dispatcher_name, False, ty, <:vala< [] >>)
    else None
  ) in
  let aux = (loc, "aux", False, <:ctyp< 'aux >>, <:vala< [] >>) in
  let dispatch_table_type = <:ctyp< { $list:[aux :: ltl]$ } >> in
  let migrater_type = match t.inherit_type with [
    None -> <:ctyp< $lid:t.dispatch_type_name$ 'aux -> 'a -> 'b >>
  | Some inhty -> <:ctyp< $lid:t.dispatch_type_name$ 'aux -> $inhty$ -> 'a -> 'b >>
  ] in
  [ <:type_decl< $lid:t.dispatch_type_name$ 'aux = $dispatch_table_type$ >> ;
    <:type_decl< migrater_t 'aux 'a 'b = $migrater_type$ >> ]
;

value dispatch_table_expr loc rc =
  let lel = List.concat (List.map (fun (dispatcher_name, t) ->
    if dispatcher_open_recursion rc dispatcher_name then
      [(<:patt< $lid:dispatcher_name$ >>, <:expr< $lid:dispatcher_name$ >>)]
    else []
    ) rc.dispatchers) in
  let aux = (<:patt< aux >>, <:expr< aux >>) in
  <:expr< fun aux -> { $list:[aux :: lel]$ } >>
;

value build_context loc ctxt tdl =
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  let optarg =
    let l = List.map (fun (k, e) -> (<:patt< $lid:k$ >>, e)) (Ctxt.options ctxt) in
    <:expr< { $list:l$ } >> in
  let rc = params type_decls optarg in
  let dispatchers = rc.dispatchers in

  let repeated_dispatcher_names = Std2.hash_list_repeats (List.map fst dispatchers) in
  let sorted_repeated_dispatcher_names = List.sort Stdlib.compare repeated_dispatcher_names in
  if [] <> repeated_dispatcher_names then
    Fmt.(raise_failwithf rc.loc "pa_deriving.migrate: dispatchers defined more than once: %a"
           (list ~{sep=sp} string) sorted_repeated_dispatcher_names)
  else
   rc
;


value reduce1 ((id: string), tyargs) td = do {
  if List.length tyargs <> List.length (uv td.tdPrm) then
    Fmt.(raise_failwith (loc_of_type_decl td) "actual/formal length mismatch")
  else () ;
  let rho = List.map2 (fun formal actual ->
      match formal with [
        ( <:vala< Some tyv >>, _ ) -> (tyv, actual)
      | _ -> Fmt.(raise_failwith (loc_of_type_decl td) "pa_deriving.migrate: blank formal type-variables are not supported")
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
    if (Some dname) = except || t.Dispatch1.manual then None else
     ctyp
     |> (fun r -> pmatch t.Dispatch1.srctype r)
     |> Std.map_option (fun rho -> ((dname, t), rho))
  ) t.dispatchers
;

(**pp strategy for generating a migrater.

(1) start with srctype

(2) reduce it; if you get no change, it's a failure

(3) if it matches anything other than the current migrate-dispatcher rule, apply that.

(4) otherwise, keep reducing until you get TySum or TyRec

(5) Take the dsttype's module-prefix and use it

(6) And generate a copy-expression

*)

value rec match_or_head_reduce loc ~{explain} ~{except} t ty =
  match (except, match_migrate_rule ~{except=except} t ty) with [
    (_, Some (d, rho)) -> Left (d, rho)
  | (Some dname, None) ->
    let ty' = head_reduce1 t ty in
    if Reloc.eq_ctyp ty ty' then
      match ty with [
        (<:ctyp< [ $list:_$ ] >> | <:ctyp< [= $list:_$ ] >> | <:ctyp< { $list:_$ } >> | <:ctyp< ( $list:_$ ) >> | <:ctyp< ' $_$ >> | <:ctyp< $lid:_$ >>) -> Right (dname, ty)

      | _ -> raise_migration_exception loc explain (Failure Fmt.(str "migrate rule %s: cannot migrate srctype %a.%a" dname pp_ctyp ty
                                                                   Explanation.pp_hum explain
               ))
      ]
    else
      match_or_head_reduce ~{explain} loc ~{except=except} t ty'
  | (None, None) ->
    raise_migration_exception loc explain (Failure Fmt.(str "match_or_head_reduce: No matching type-pattern in a migration rule for this type: %a@.%a"
                                   pp_ctyp ty
                                   Explanation.pp_hum explain
      ))
  ]
;

value canon_ctyp ty = Reloc.ctyp (fun _ -> Ploc.dummy) 0 ty ;
value builtin_copy_types =
  let loc = Ploc.dummy in
  List.map canon_ctyp [
    <:ctyp< bool >>
  ; <:ctyp< char >>
  ; <:ctyp< float >>
  ; <:ctyp< int >>
  ; <:ctyp< int32 >>
  ; <:ctyp< int64 >>
  ; <:ctyp< nativeint >>
  ; <:ctyp< string >>
  ; <:ctyp< unit >>
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

value rec generate_leaf_dispatcher_expression ~{explain} t d subs_rho ty =
  if AList.mem ~{cmp=Reloc.eq_ctyp} ty subs_rho then
    let (f_sub, f_result_ty) = AList.assoc ~{cmp=Reloc.eq_ctyp} ty subs_rho in
    let loc = loc_of_ctyp ty in
    app_dt t <:expr< $lid:f_sub$ >>
  else match ty with [
  <:ctyp:< [ $list:branches$ ] >> ->
  let ll = List.map (fun [
      <:constructor:< $uid:uid$ of { $list:ltl$ } >> ->
      let custom_branches = Std.filter (fun (n, _) -> uid = n) d.Dispatch1.custom_branches in
      if custom_branches <> [] then
        List.map snd custom_branches
      else
      let e =
        let d = { (d) with Dispatch1.dstmodule = None } in
        let explain = [Explanation.Constructor uid <:ctyp< { $list:ltl$ } >> :: explain] in
        generate_leaf_dispatcher_expression ~{explain} t d subs_rho <:ctyp< { $list:ltl$ } >> in
      match e with [
          <:expr:< fun [ $patt$ -> $expr$ ] >> ->
              let expr = <:expr< $uid:uid$ $expr$ >> in
              let expr = Dispatch1.expr_wrap_dsttype_module d expr in
              [(<:patt< $uid:uid$ $patt$ >>, <:vala< None >>, expr)]
        ]

    | <:constructor< $uid:uid$ of $list:tyl$ >> ->
      let custom_branches = Std.filter (fun (n, _) -> uid = n) d.Dispatch1.custom_branches in
      if custom_branches <> [] then
        List.map snd custom_branches
      else
      let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
      let patt = List.fold_left (fun p (v,_) -> <:patt< $p$ $lid:v$ >>) <:patt< $uid:uid$ >> argvars in
      let expr = List.fold_left (fun e (v,ty) ->
          let explain = [Explanation.Constructor uid ty :: explain] in
          let sub_rw = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho ty in
          <:expr< $e$ ($app_dt t (fst sub_rw)$ $lid:v$) >>
        ) <:expr< $uid:uid$ >> argvars in
      [(patt, <:vala< None >>, Dispatch1.expr_wrap_dsttype_module d expr)]

    ]) branches in
  let l = List.concat ll in
  <:expr< fun [ $list:l$ ] >>

| <:ctyp:< [= $list:l$ ] >> as z ->
  let branches = List.concat_map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
      let custom_branches = Std.filter (fun (n, _) -> cid = n) d.Dispatch1.custom_branches in
      if custom_branches <> [] then
        List.map snd custom_branches
      else
      let tyl = uv tyl in
      let argvars = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i,ty)) tyl in
      let patt = List.fold_left (fun p (v,_) -> <:patt< $p$ $lid:v$ >>) <:patt< ` $cid$ >> argvars in
      let expr = List.fold_left (fun e (v,ty) ->
          let sub_rw = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho ty in
          <:expr< $e$ ($app_dt t (fst sub_rw)$ $lid:v$) >>
        ) <:expr< ` $cid$ >> argvars in
      [(patt, <:vala< None >>, Dispatch1.expr_wrap_dsttype_module d expr)]

  | PvInh loc ty ->
     Fmt.(raise_failwithf loc "pa_deriving_migrate: we don't handle poly-variant inheritance: the OCaml compiler will expand the inheritance, so just use pa_ppx_import to import the type and you wont' have any inheritance.")
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< { $list:ltl$ } >> ->
    let patt =
      let lpl = List.map (fun (_, lid, _, _, _) ->
          (<:patt< $lid:lid$ >>, <:patt< $lid:lid$ >>)
        ) ltl in
      <:patt< { $list:lpl$ } >> in
    let expr =
      let trimmed_ltl = Std.filter (fun (_, lid, _, _, _) -> not (List.mem lid d.Dispatch1.skip_fields)) ltl in 
      let trimmed_lel = List.map (fun  (_, lid, _, ty, _) ->
          let explain = [Explanation.Field lid ty :: explain] in
          let sub_rw = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho ty in
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
      let pl = List.mapi (fun i (lab, ty) ->
          let lid = Printf.sprintf "v_%d" i in
          match uv lab with [
              None -> <:patt< $lid:lid$ >>
            | Some <:vala< lab >> -> <:patt< ~{$lid:lab$ = $lid:lid$} >>
            ]
                 ) tyl in
      <:patt< ( $list:pl$ ) >> in
    let expr =
      let el = List.mapi (fun i (lab, ty) ->
          let explain = [Explanation.In_tuple ty :: explain] in
          let lid = Printf.sprintf "v_%d" i in
          let sub_rw = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho ty in
          match uv lab with [
              None -> <:expr< $app_dt t (fst sub_rw)$ $lid:lid$ >>
            | Some <:vala< lab >> -> <:expr< ~{$lid:lab$ = $app_dt t (fst sub_rw)$ $lid:lid$} >>
            ]
        ) tyl in
      <:expr< ( $list:el$ ) >> in
    <:expr< fun [ $patt$ -> $expr$ ] >>
| ty -> Fmt.(raise_failwithf (loc_of_ctyp ty)
               "generate_leaf_dispatcher_expression: unsupported type:@ %a"
               pp_ctyp ty)
]

and generate_dispatcher_expression ~{explain} ~{except} t subs_rho ty = 
  if AList.mem ~{cmp=Reloc.eq_ctyp} ty subs_rho then
    let (f_sub, f_result_ty) = AList.assoc ~{cmp=Reloc.eq_ctyp} ty subs_rho in
    let loc = loc_of_ctyp ty in
    (<:expr< $lid:f_sub$ >>, f_result_ty)
  else if List.mem (canon_ctyp ty) builtin_copy_types then
    (id_expr t, ty)
  else match ty with [
    <:ctyp:< ( $list:tyl$ ) >> ->
      let explain = [Explanation.In_tuple ty ::  explain] in
      let patt =
        let pl = List.mapi (fun i (lab, ty) ->
            let lid = Printf.sprintf "v_%d" i in
            match uv lab with [
                None -> <:patt< $lid:lid$ >>
              | Some <:vala< lab >> -> <:patt< ~{$lid:lab$ = $lid:lid$} >>
              ]
                   ) tyl in
        <:patt< ( $list:pl$ ) >> in
      let exprs_types = List.mapi (fun i (lab, ty) ->
            let lid = Printf.sprintf "v_%d" i in
            let sub_rw = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho ty in
            let e = match uv lab with [
                  None -> <:expr< $app_dt t (fst sub_rw)$ $lid:lid$ >>
                | Some <:vala< lab >> -> <:expr< ~{$lid:lab$ = $app_dt t (fst sub_rw)$ $lid:lid$} >>
                ] in
            (e, (lab, snd sub_rw))
          ) tyl in
      let expr =
        let el = List.map fst exprs_types in
        <:expr< ( $list:el$ ) >> in
      let rhsty =
        let tyl = List.map snd exprs_types in
        <:ctyp< ( $list:tyl$ ) >> in
      (abs_dt t <:expr< fun [ $patt$ -> $expr$ ] >>, rhsty)
    | _ ->
      let explain = [Explanation.Tycon ty ::  explain] in
      generate_tycon_dispatcher_expression ~{explain} ~{except=except} t subs_rho ty
  ]

and generate_tycon_dispatcher_expression ~{explain} ~{except} t subs_rho ty = 
  let loc = loc_of_ctyp ty in
  match match_or_head_reduce ~{explain} loc ~{except=except} t ty with [
    Left ((rwdname, rwd), lrho) ->
    let explain = [Explanation.Rewrite rwdname ::  explain] in
    (** [rwd] is the migrate dispatcher that matched,
        and [lrho] is the substitution generated by the match. *)
    let (revsubs, rrho) = List.fold_left (fun (revsubs, rrho) (lhsty, rhsty) ->
        let conc_lhsty = Ctyp.subst lrho lhsty in
        let (e, conc_rhsty) = generate_dispatcher_expression ~{explain} ~{except=None} t subs_rho conc_lhsty in
        let add_rrho = match pmatch rhsty conc_rhsty with [
          None ->
          Fmt.(raise_failwith (loc_of_ctyp rhsty) "generate_dispatcher_expression: subterm dispatch returned non-matching type")
        | Some rho -> rho
        ] in
        if [] <> Std.intersect (List.map fst rrho) (List.map fst add_rrho) then
          Fmt.(raise_failwithf (loc_of_ctyp rhsty)
                 "generate_tycon_dispatcher_expression (ty=%a): overlapping type-variables on rhs of rewrite rule %s"
                 pp_ctyp ty rwdname)
        else
          ([ e :: revsubs ], Env.append (loc_of_ctyp rhsty) rrho add_rrho)
      ) ([], []) rwd.Dispatch1.subs in
    let loc = loc_of_ctyp ty in
    let dexp = dispatcher_invocation_expression t loc rwdname in
    let e = Expr.applist dexp (List.rev revsubs) in
    let e = abs_dt t (app_dt t e) in
    (e, Ctyp.subst rrho rwd.Dispatch1.dsttype)

  | Right (dname, headredty) ->
     let explain = [Explanation.Reduced dname headredty :: explain] in
    let d = List.assoc dname t.dispatchers in

    if List.mem (canon_ctyp headredty) builtin_copy_types then
      (id_expr t, d.Dispatch1.dsttype)
    else
      let e = generate_leaf_dispatcher_expression ~{explain} t d subs_rho headredty in
      let e = abs_dt t e in
      (e, d.Dispatch1.dsttype)
  ]
;

value toplevel_generate_dispatcher t (dname,d) = do {
  let explain = [Explanation.Toplevel dname] in
  if debug.val then
    Fmt.(pf stderr "[toplevel_generate_dispatcher: %s]\n%!" dname)
  else () ;
  match d.Dispatch1.code with [
    Some <:expr:< $lid:f$ >> ->
    <:expr< (fun __dt__ __inh__ -> $lid:f$ __dt__ __inh__) >>
  | Some e -> e
  | None ->
    let srctype = d.Dispatch1.srctype in
    let loc = loc_of_ctyp srctype in
    let subs_rho = List.mapi (fun i (lhsty, rhsty) -> (lhsty, (Printf.sprintf "__subrw_%d" i, rhsty))) d.Dispatch1.subs in
    let subs_binders = List.map2 (fun (_,(v, _)) ty -> <:patt< $lid:v$ >>) subs_rho d.Dispatch1.subs_types in
    let (e, t) = generate_dispatcher_expression ~{explain} ~{except=Some dname} t subs_rho srctype in
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
        let ety = Migrate.dispatcher_type loc rc (dname, d) in
        let (l, ety) = match ety with [
            <:ctyp< ! $list:l$ . $rhs$ >> ->
            let l = ["aux" :: l] in
            (l, <:ctyp< ! $list:l$ . $rhs$ >>)
          | rhs -> 
            let l = ["aux"] in
            (l, <:ctyp< ! $list:l$ . $rhs$ >>)
            ] in
        let e =
          List.fold_right (fun tv e -> <:expr< fun (type $lid:tv$) -> $e$ >>) l e in
        (<:patt< ($lid:dname$ : $ety$ ) >>, e, <:vala< [] >>)
      ) rc.Migrate.dispatchers in
    let si0 = <:str_item< value rec $list:migrate_dispatcher_decls$ >> in
    let si1 = <:str_item< value $lid:rc.Migrate.dispatch_table_constructor$ = $dispatch_table_constructor_expression$ >> in
  <:str_item< declare type $list:dispatch_type_decls$ ; $si0$ ; $si1$ ; end >>
| _ -> assert False ]
;
(*
value str_item_gen_migrate name arg si =
  try
    _str_item_gen_migrate name arg si
  with Ploc.Exc loc (Migration_exception e explain) as exn ->
    let bt = Printexc.get_raw_backtrace() in do {
      Fmt.(pf stderr "Pa_deriving_migrate.str_item_gen_migrate: Migration error:@.%a@.%a"
             exn e
             Explanation.pp_hum explain
      ) ;
      Printexc.raise_with_backtrace exn bt
    }
;
 *)
value sig_item_gen_migrate name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = Migrate.build_context loc arg tdl in
    let dispatch_type_decls = Migrate.dispatch_table_type_decls loc rc in
    let migrate_dispatcher_decls = List.map (fun (dname,d) ->
        let ety = Migrate.dispatcher_type loc rc (dname, d) in
        let ety = match ety with [
            <:ctyp< ! $list:l$ . $rhs$ >> -> rhs
          | rhs -> rhs
            ] in
        <:sig_item< value $dname$ : $ety$ >>
      ) rc.Migrate.dispatchers in
    <:sig_item<
      declare type $list:dispatch_type_decls$ ;
      declare $list:migrate_dispatcher_decls$ end  ;
      value $lid:rc.dispatch_table_constructor$ : 'a -> dispatch_table_t 'a ;
  end >>
| _ -> assert False ]
;
(*
value sig_item_gen_migrate name arg si =
  try
    _sig_item_gen_migrate name arg si
  with Ploc.Exc loc (Migration_exception e explain) as exn ->
    let bt = Printexc.get_raw_backtrace() in do {
      Fmt.(pf stderr "Pa_deriving_migrate.sig_item_gen_migrate: Migration error:@.%a@.%a"
             exn e
             Explanation.pp_hum explain
      ) ;
      Printexc.raise_with_backtrace exn bt
    }
;
 *)
Pa_deriving.(Registry.add PI.{
  name = "migrate"
; alternates = []
; options = ["optional"
            ; "default_dispatchers"
            ; "default_open_recursion"
            ; "closed_recursion_dispatchers"
            ; "open_recursion_dispatchers"
            ; "dispatchers"
            ; "dispatch_type"
            ; "dispatch_table_constructor"
            ; "inherit_type"]
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
; sig_item = sig_item_gen_migrate
})
;

