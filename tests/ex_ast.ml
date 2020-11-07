
module Ploc = struct
  include Ploc
let preeq_t x y = x = y
let prehash_t x = Hashtbl.hash x
let hash_t = prehash_t
end

open Ploc

module AST1 = struct
type t0 = string
type t1 = A of Ploc.t * t0 * int list
type 'a pt2 = { it2: 'a }
type t2 = B of string * t0 * t1 pt2 | C of bool | D
type 'a pt3 = { it : 'a ; extra : int ; dropped_field: string }
type t4 = t2 pt3
type t4' = t2 pt3
end

module AST2 = struct
type t0 = int
type t1 = A of Ploc.t * t0 * int list
type 'a pt2 = { it2: 'a }
type t2 = B of string * t0 * t1 pt2 | C of int | E
type 'a pt3 = { it : 'a ; extra : int ; new_field : int }
type t4 = t2 pt3
type t4' = t2 pt3
end

module AST3 = struct
type t1 = A of int list * float list
end

module AST4 = struct
type t1 = A of int list * float list
end
