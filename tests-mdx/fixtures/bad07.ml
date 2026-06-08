(**pp -syntax camlp5o -package pa_ppx_migrate  *)

type _ t1 = int
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
    }
]
