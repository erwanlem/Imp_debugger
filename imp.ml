
module Env = Map.Make(String)

type id_array = { array : value array; id : int }
and value =
  | VInt  of int
  | VBool of bool
  | VArray of id_array
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
  | Print  of expr * int * Lexing.position
  | Set    of string * expr * int * Lexing.position
  | If     of expr * seq * seq * int * Lexing.position
  | While  of expr * seq * int * Lexing.position
  | Return of expr * int * Lexing.position
  | Expr   of expr * int * Lexing.position
  | SetArr of expr * expr * expr * int * Lexing.position
and seq = instr list

type function_def = {
  name  : string;
  id    : int;
  params: string list;
  locals: (string * expr option * int * Lexing.position) list;
  code  : seq;
  line  : Lexing.position;
}

type program = {
  globals  : (string * expr option * int * Lexing.position) list;
  functions: function_def list;
  main     : function_def;
}
