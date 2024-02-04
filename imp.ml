
module Env = Map.Make(String)

type unop = Not | Opp
type binop = Add | Sub | Mul | Div | Rem
           | Lt | Le | Gt | Ge | Eq | Neq
           | And | Or


type expr =
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
  | Print  of expr
  | Set    of string * expr
  | If     of expr * seq * seq
  | While  of expr * seq
  | Return of expr
  | Expr   of expr
  | SetArr of expr * expr * expr
and seq = instr list

type value =
  | VInt  of int
  | VBool of bool
  | VArray of value array
  | Null



type function_def = {
  name:   string;
  params: string list;
  locals: (string * expr option) list;
  code:   seq;
}

type program = {
  globals:   (string * expr option) list;
  functions: function_def list;
  main: function_def;
}
