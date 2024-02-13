
module Env = Map.Make(String)

type value =
  | VInt  of int
  | VBool of bool
  | VArray of value array
  | VNull


let string_of_value v =
  match v with 
    | VInt i  -> Printf.sprintf "%d%!" i
    | VBool b -> Printf.sprintf "%b%!" b
    | _       -> "Null"


let instruction_id =
  let id = ref 0 in
  fun () ->
    let r = !id in
    id := !id+1;
    r


type unop = Not | Opp
type binop = Add | Sub | Mul | Div | Rem
           | Lt | Le | Gt | Ge | Eq | Neq
           | And | Or


type expr =
  | Null
  | Int    of int
  | Bool   of bool
  | Var    of string
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  | Call   of string * expr list
  | Array  of expr list
  | GetArr of expr * expr
  | Continuation

type instr =
  | Print  of expr * int
  | Set    of string * expr * int
  | If     of expr * seq * seq * int
  | While  of expr * seq * int
  | Return of expr * int
  | Expr   of expr * int
  | SetArr of expr * expr * expr * int
and seq = instr list

type function_def = {
  name:   string;
  id  :   int;
  params: string list;
  locals: (string * expr option * int) list;
  code:   seq;
}

type program = {
  globals:   (string * expr option * int) list;
  functions: function_def list;
  main: function_def;
}
