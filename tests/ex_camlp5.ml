(**pp -syntax camlp5o *)
module Ploc= struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())
end

[%%typedecls
   [%%import: 'a Ploc.vala]
   [%%import: MLast.expr
                [@add [%%import: MLast.loc]]
                [@add [%%import: MLast.type_var]]
                [@with Ploc.vala := vala]
   ]
]
