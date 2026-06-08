(**pp -syntax camlp5o -package pa_ppx_migrate  *)

module OK1 = struct
module DST = Ex_ast.AST2

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

type t1 = [ `A | `B | t ]
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
      }
    }
]
end
