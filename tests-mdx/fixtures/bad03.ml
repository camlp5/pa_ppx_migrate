(**pp -syntax camlp5o -package pa_ppx_migrate  *)

type t1 = M.t1 = A of (int, string) choice
[@@deriving
     migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_open_recursion = false
    ; default_dispatchers = [
        {
          srcmod = M
        ; dstmod = N
        ; types = [
            t1
          ]
        }
      ]
    ; dispatchers = {
        migrate_choice = {
          srctype = [%typ: ('a1, 'a1) choice]
        ; dsttype = [%typ: ('b1, 'b2) choice]
        ; code = _migrate_choice
        ; subs = [ ([%typ: 'a1], [%typ: 'b1])
                 ; ([%typ: 'a2], [%typ: 'b2]) ]
        }
      }
    }
]
