open Format
open Imp

let bop2string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Rem -> "%"
  | Lt  -> "<"
  | Le  -> "<="
  | Gt  -> ">"
  | Ge  -> ">="
  | Eq  -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or  -> "||"
let uop2string = function
  | Not -> "!"
  | Opp -> "-"

let rec print_expr  = function
  | Int n -> sprintf  "%d" n
  | Bool b -> sprintf  "%b" b
  | Var x -> sprintf  "%s" x
  | Unop(op, e) ->
     sprintf "@[%s%s@]" (uop2string op) (print_expr e)
  | Binop(op, e1, e2) -> 
     sprintf  "(@[%s %s@ %s@])" (print_expr e1) (bop2string op) (print_expr e2)
  | Call(f, args) -> sprintf  "%s(@[%s@])" f (print_args args)
  | Array(elts) -> sprintf  "{@[%s@]}" (print_elts elts)
  | GetArr(e1, e2) -> sprintf  "@[%s[%s]@]" (print_expr e1) (print_expr e2)
  | _ -> ""
and print_args  = function
  | [] -> sprintf  ""
  | [a] -> sprintf  "%s" (print_expr a)
  | a::args -> sprintf  "%s,@ %s" (print_expr a) (print_args args)
and print_elts  = function
  | [] -> sprintf  ""
  | e::elts -> sprintf  "%s;@ %s" (print_expr e) (print_elts elts)

let rec print_instr  = function
  | Print (e, id) -> sprintf  "print(@[%s@]);" (print_expr e)
  | Set(x, e, id) -> sprintf  "%s = @[%s@];" x (print_expr e)
  | If(e, s1, s2, id) -> sprintf  
                       "@[<v>@[<v 2>if (@[%s@]) {@,%s@]@,@[<v 2>} else {@,%s@]@,}@]" 
                       (print_expr e) (print_seq s1) (print_seq s2)
  | While(e, s, id) -> sprintf  "@[<v>@[<v 2>while (@[%s@]) {@,%s@]@,}@]"
                     (print_expr e) (print_seq s)
  | Return (e, id) -> sprintf  "return(@[%s@]);" (print_expr e)
  | Expr (e,id) -> sprintf  "@[%s@];" (print_expr e)
  | SetArr(t, i, e, id) -> sprintf  "%s[%s] = @[%s@];" 
                         (print_expr t) (print_expr i) (print_expr e)
and print_seq  = function
  | [] -> sprintf  ""
  | [i] -> sprintf  "%s" (print_instr i)
  | i::seq -> sprintf  "@[<v>%s@,%s@]" (print_instr i) (print_seq seq)

let rec print_params  = function
  | [] -> sprintf  ""
  | [x] -> sprintf  "%s" x
  | x::params -> sprintf  "%s,@ %s" x (print_params params)

let rec print_vars  = function
  | [] -> sprintf  ""
  | (x, None)::vars -> sprintf  "@[<v>var %s;@,%s@]" x (print_vars vars)
  | (x, Some e)::vars -> sprintf  "@[<v>var %s = @[%s@];@,%s@]" 
                           x (print_expr e) (print_vars vars)

let print_fdef  fdef =
  sprintf  "@[<v>@[<v 2>function %s(@[%s@]) {@,%s@,%s@]@,}@,@]" 
    fdef.name (print_params fdef.params) (print_vars fdef.locals) (print_seq fdef.code)

let rec print_functions  = function
  | [] -> sprintf  ""
  | fdef::functions -> sprintf  "@[<v>%s@,%s@]" (print_fdef fdef) (print_functions functions)

let print_program p =
  sprintf  "@[<v>%s@,%s@]@." (print_vars p.globals) (print_functions p.functions)
