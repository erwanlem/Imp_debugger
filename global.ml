open Imp
module Env = Map.Make(String)

exception Return of value


(********************************************)
(*            GLOBAL VARIABLES              *)
(********************************************)


(* Pile environnements locaux aux fonctions *)
let local_env_stack = (ref [] : value Env.t list ref)

(* Variable temporaire résultat de fonction *)
let tmp = (ref [] : value list ref)

(* Environnement local *)
let env = (ref Env.empty : value Env.t ref)

(* Environnement global *)
let global_env = (ref Env.empty : value Env.t ref)

(* Stack for step back *)
let undo_stack = (ref [] : (instr list * value Env.t * value Env.t * value Env.t list * value list * int) list ref)

(* breakpoint lines *)
let breakpoints = (Hashtbl.create 10 : (int, unit) Hashtbl.t)

(********************************************)
(********************************************)
(********************************************)

