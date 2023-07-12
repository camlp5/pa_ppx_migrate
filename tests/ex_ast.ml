
module Ploc = struct
  include Ploc
let preeq_t x y = x = y
let prehash_t x = Hashtbl.hash x
let hash_t = prehash_t
end

open Ploc

type pvt = [ `A | `B ]

module AST1 = struct
type pvt2 = [ `C | `D ]
type t0 = string
type t1 = A of Ploc.t * t0 * int list * (int * bool)
type 'a pt2 = { it2: 'a }
type t2 = B of string * t0 * t1 pt2 | C of bool | D
type 'a pt3 = { it : 'a ; z : pvt ; z2 : pvt2 ; extra : int ; dropped_field: string }
type t4 = t2 pt3
type t4' = t2 pt3
type t5 = unit
type t6 = E of { x : int } | F of { y : bool }
end

module AST2 = struct
type pvt2 = [ `C | `D ]
type t0 = int
type t1 = A of Ploc.t * t0 * int list * (int * int * bool)
type 'a pt2 = { it2: 'a }
type t2 = B of string * t0 * t1 pt2 | C of int | E
type 'a pt3 = { it : 'a ; z : pvt ; z2 : pvt2 ; extra : int ; new_field : int }
type t4 = t2 pt3
type t4' = t2 pt3
type t5 = unit
type t6 = E of { x : int } | F of { y : bool }
end

module AST3 = struct
type t1 = A of int list * float list
end

module AST4 = struct
type t1 = A of int list * float list
end
