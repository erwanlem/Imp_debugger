

module Env = Map.Make(String)

type value =
  | VInt  of int
  | VBool of bool
  | VArray of value array
  | Fun of Imp.seq * value Env.t
  | Null

exception Return of value