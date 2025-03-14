(**pp -syntax camlp5o *)

module Ploc = Ex_ast.Ploc
module Migrate_AST1_AST2 :
  sig
    module SRC = Ex_ast.AST1
    module DST = Ex_ast.AST2
    exception Migration_error of string
    val migration_error : string -> 'a
    val _migrate_list : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list
type t0 = [%import: Ex_ast.AST1.t0]
and pvt2 = Ex_ast.AST1.pvt2
and t1 = [%import: Ex_ast.AST1.t1
  [@with [%typ: int * bool] := z1]
]
and z1 = int * bool
and 'a pt2 = [%import: 'a Ex_ast.AST1.pt2]
and t2 = [%import: Ex_ast.AST1.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST1.pt3]
and pvt = Ex_ast.pvt
and t4 = [%import: Ex_ast.AST1.t4]
and t4' = [%import: Ex_ast.AST1.t4']
and t5 = [%import: Ex_ast.AST1.t5]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = false
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST1
        ; dstmod = Ex_ast.AST2
        ; types = [
            t1
          ; pt2
          ; t4'
          ; t5
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
      ; migrate_pvt = {
          srctype = [%typ: pvt]
        ; dsttype = [%typ: pvt]
        ; code = fun __dt__ x -> x
        }
      ; migrate_pvt2 = {
          srctype = [%typ: pvt2]
        ; dsttype = [%typ: pvt2]
        ; code = fun __dt__ x -> x
        }
      ; migrate_z1 = {
          srctype = [%typ: z1]
        ; dsttype = [%typ: int * int * bool]
        ; code = fun __dt__ (n,b) -> (n,n,b)
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ s ->
            match int_of_string s with
              n -> n
            | exception Failure _ -> migration_error "t0"
        }
      ; migrate_t2 = {
          srctype = [%typ: t2]
        ; dsttype = [%typ: DST.t2]
        ; custom_branches_code = function
              C true -> C 1
            | C false -> C 0
            | D -> migration_error "t2:D"
        }
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ dropped_field ]
        ; custom_fields_code = {
            new_field = extra
          }
        }
      ; migrate_t4 = {
          srctype = [%typ: t4]
        ; dsttype = [%typ: DST.t4]
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

module Migrate_AST2_AST1 :
  sig
    module SRC = Ex_ast.AST2
    module DST = Ex_ast.AST1
    exception Migration_error of string
    val migration_error : string -> 'a
    val _migrate_list : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list
type t0 = [%import: Ex_ast.AST2.t0]
and pvt2 = Ex_ast.AST2.pvt2
and t1 = [%import: Ex_ast.AST2.t1
  [@with [%typ: int * int * bool] := z1]
]
and z1 = int * int * bool
and 'a pt2 = [%import: 'a Ex_ast.AST2.pt2]
and t2 = [%import: Ex_ast.AST2.t2]
and 'a pt3 = [%import: 'a Ex_ast.AST2.pt3]
and pvt = Ex_ast.pvt
and t4 = [%import: Ex_ast.AST2.t4]
and t4' = [%import: Ex_ast.AST2.t4']
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; closed_recursion_dispatchers = [ migrate_t1 ]
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST2
        ; dstmod = Ex_ast.AST1
        ; types = [
            t1
          ; pt2
          ; t4'
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
      ; migrate_pvt = {
          srctype = [%typ: pvt]
        ; dsttype = [%typ: pvt]
        ; code = fun __dt__ x -> x
        }
      ; migrate_pvt2 = {
          srctype = [%typ: pvt2]
        ; dsttype = [%typ: pvt2]
        ; code = fun __dt__ x -> x
        }
      ; migrate_z1 = {
          srctype = [%typ: z1]
        ; dsttype = [%typ: int * bool]
        ; code = fun __dt__ (n,_,b) -> (n,b)
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ n -> string_of_int n
        }
      ; migrate_t2 = {
          srctype = [%typ: t2]
        ; dsttype = [%typ: DST.t2]
        ; custom_branches_code = function
              C 1 -> C true
            | C 0 -> C false
            | C _ -> migration_error "t2:C"
            | E -> migration_error "t2:E"
        }
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ new_field ]
        ; custom_fields_code = {
            dropped_field = string_of_int extra
          }
        }
      ; migrate_t4 = {
          srctype = [%typ: t4]
        ; dsttype = [%typ: DST.t4]
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
module Migrate_AST3_AST4 :
  sig
    module SRC = Ex_ast.AST3
    module DST = Ex_ast.AST4
    exception Migration_error of string
    val migration_error : string -> 'a
    val _migrate_list : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list
type t1 = [%import: Ex_ast.AST3.t1]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = false
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST3
        ; dstmod = Ex_ast.AST4
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
module Migrate_AST4_AST3 :
  sig
    module SRC = Ex_ast.AST4
    module DST = Ex_ast.AST3
    exception Migration_error of string
    val migration_error : string -> 'a
    val _migrate_list : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list
type t1 = [%import: Ex_ast.AST4.t1]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST4
        ; dstmod = Ex_ast.AST3
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

module Migrate_AST5 : sig

exception Migration_error of string

type t = [%import:Ex_ast.AST5.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST5
        ; dstmod = Ex_ast.AST5
        ; types = [
            t
          ]
        }
      ]
    ; dispatchers = {
        manual_migrate_t = {
          srctype = [%typ: t]
        ; dsttype = [%typ: t]
        ; manual = true
        ; code = fun _ t -> migration_error "should not be raising this"
        }
      }
    }
]
end
module Migrate_AST6 : sig

exception Migration_error of string

type t = [%import:Ex_ast.AST6.t]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST6
        ; dstmod = Ex_ast.AST6
        ; types = [
            t
          ]
        }
      ]
    ; dispatchers = {
        manual_migrate_t = {
          srctype = [%typ: t]
        ; dsttype = [%typ: t]
        ; manual = true
        ; code = fun _ t -> migration_error "should not be raising this"
        }
      }
    }
]
end
module Migrate_AST7 : sig

exception Migration_error of string

[%%import:Ex_ast.AST7.t']
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST7
        ; dstmod = Ex_ast.AST7
        ; types = [
            t'
          ]
        }
      ]
    ; dispatchers = {
        manual_migrate_t' = {
          srctype = [%typ: t']
        ; dsttype = [%typ: t']
        ; manual = true
        ; code = fun _ t -> migration_error "should not be raising this"
        }
      }
    }
]
end
