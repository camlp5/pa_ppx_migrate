(**pp -syntax camlp5o -package pa_ppx_migrate  *)

module OK1 = struct
module DST = Ex_ast.AST2

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

type t0 = string
and t1 = Ex_ast(N).AST1.t1 = A of Ploc.t * t0 * int list * z1
and z1 = int * bool
[@@deriving
     migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = false
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST1
        ; dstmod = Ex_ast.AST2
        ; types = [
            t1
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ s ->
            match int_of_string s with
              n -> n
            | exception Failure _ -> migration_error "t0"
        }
      ; migrate_z1 = {
          srctype = [%typ: z1]
        ; dsttype = [%typ: int * int * bool]
        ; code = fun __dt__ (n,b) -> (n,n,b)
        }
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]
end
