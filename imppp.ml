open Format
open Imp

let find_instr = ref false
let instr_id = ref 0

let write_out msg =
  let file = "log.txt" in
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" msg;
  (* write something *)
  close_out oc



let tags_funs =
  {
    mark_open_stag = (fun s -> match s with String_tag s -> ("<" ^ s ^ ">") | _ -> "");
    mark_close_stag = (fun s -> match s with String_tag s -> ("</" ^ s ^ ">") | _ -> "");
    print_open_stag = (fun _ -> ());
    print_close_stag = (fun _ -> ())
  }



let print_if_find id str =
  if id = !instr_id then (fprintf str_formatter "@{<color>%s@}" str; flush_str_formatter () )
  else str

let mark_open fmt id =
  if id = !instr_id then 
    fprintf fmt "@{<color>"
  else ()

let mark_close fmt id =
  if id = !instr_id then
    fprintf fmt "@}"
  else ()

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

  (*
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
  | a::args -> sprintf  "%s, %s" (print_expr a) (print_args args)
and print_elts  = function
  | [] -> sprintf  ""
  | e::elts -> sprintf  "%s;@ %s" (print_expr e) (print_elts elts)

let rec print_instr  = function
  | Print (e, id) -> (fprintf str_formatter "print(@[%s@]);" (print_expr e));
                    print_if_find id (flush_str_formatter ())
  | Set(x, e, id) -> (fprintf str_formatter "%s = @[%s@];" x (print_expr e));
                    print_if_find id (flush_str_formatter ())
  | If(e, s1, s2, id) -> if id = !instr_id then 
                          (fprintf str_formatter
                          "@[<v>@[<v 4>@{<color>if (@[%s@])@} {%s@]@,@[<v 2>@ } else {%s@]@,}@]" 
                          (print_expr e) (print_seq s1) (print_seq s2))
                        else
                          (fprintf str_formatter
                          "@[<v>@[<v 4>if (@[%s@]) {@,@[<v 2>%s@]@]@,@[<v 2>@ } else {%s@]@,}@]" 
                          (print_expr e) (print_seq s1) (print_seq s2));
                        flush_str_formatter ()
  | While(e, s, id) -> (fprintf str_formatter "@[<v>@[<v 2>while (@[%s@]) {@,%s@]@,}@]"
                     (print_expr e) (print_seq s));
                     print_if_find id (flush_str_formatter ())
  | Return (e, id) -> (fprintf str_formatter "return(@[%s@]);" (print_expr e));
                      print_if_find id (flush_str_formatter ())
  | Expr (e,id) -> (fprintf str_formatter "@[%s@];" (print_expr e));
                      print_if_find id (flush_str_formatter ())
  | SetArr(t, i, e, id) -> (fprintf str_formatter "%s[%s] = @[%s@];"
                         (print_expr t) (print_expr i) (print_expr e));
                         print_if_find id (flush_str_formatter ())
and print_seq  = function
  | [] -> fprintf str_formatter ""; flush_str_formatter ()
  | [i] -> fprintf str_formatter "%s" (print_instr i); flush_str_formatter ()
  | i::seq -> fprintf str_formatter "@[<v>%s@,%s@]" (print_instr i) (print_seq seq);
              flush_str_formatter ()

let rec print_params  = function
  | [] -> fprintf str_formatter ""; flush_str_formatter ()
  | [x] -> fprintf str_formatter "%s" x; flush_str_formatter ()
  | x::params -> fprintf str_formatter "%s, %s" x (print_params params); flush_str_formatter ()

let rec print_vars  = function
  | [] -> fprintf str_formatter ""; flush_str_formatter ()
  | (x, None, id)::vars -> (fprintf str_formatter "@[<v>var %s;@,%s@]" x (print_vars vars)); 
                            flush_str_formatter ()
  | (x, Some e, id)::vars -> (fprintf str_formatter "@[<v>var %s = @[%s@];@,%s@]" 
                           x (print_expr e) (print_vars vars) ); flush_str_formatter ()

let print_fdef  fdef =
  (fprintf str_formatter "@[<v>@[<v 2>function %s(@[%s@]) {@,%s@,%s@]@,}@,@]" 
    fdef.name (print_params fdef.params) (print_vars fdef.locals) (print_seq fdef.code));
    flush_str_formatter ()

let rec print_functions  = function
  | [] -> fprintf str_formatter ""; flush_str_formatter ()
  | fdef::functions -> fprintf str_formatter "@[<v>%s@,%s@]" (print_fdef fdef) (print_functions functions); 
                      flush_str_formatter ()

let print_program p =
  pp_set_tags str_formatter true;
  pp_set_formatter_stag_functions str_formatter tags_funs;
  fprintf str_formatter "@[<v>%s@,%s@]@." (print_vars p.globals) (print_functions p.functions);
  flush_str_formatter ()
  *)


  let rec print_expr fmt = function
  | Int n -> fprintf fmt "%d" n
  | Bool b -> fprintf fmt "%b" b
  | Var x -> fprintf fmt "%s" x
  | Unop(op, e) ->
     fprintf fmt "@[%s%a@]" (uop2string op) print_expr e
  | Binop(op, e1, e2) -> 
     fprintf fmt "(@[%a %s@ %a@])" print_expr e1 (bop2string op) print_expr e2
  | Call(f, args) -> fprintf fmt "%s(@[%a@])" f print_args args
  | Array(elts) -> fprintf fmt "{@[%a@]}" print_elts elts
  | GetArr(e1, e2) -> fprintf fmt "@[%a[%a]@]" print_expr e1 print_expr e2
  | _ -> ()
and print_args fmt = function
  | [] -> fprintf fmt ""
  | [a] -> fprintf fmt "%a" print_expr a
  | a::args -> fprintf fmt "%a,@ %a" print_expr a print_args args
and print_elts fmt = function
  | [] -> fprintf fmt ""
  | e::elts -> fprintf fmt "%a;@ %a" print_expr e print_elts elts

let rec print_instr fmt = function
  | Print (e, id) -> mark_open fmt id; fprintf fmt "print(@[%a@]);" print_expr e; mark_close fmt id
  | Set(x, e, id) -> mark_open fmt id; fprintf fmt "%s = @[%a@];" x print_expr e; mark_close fmt id
  | If(e, s1, s2, id) ->if !instr_id = id then 
                        fprintf fmt 
                        "@[<v>@[<v 2>@{<color>if (@[%a@])@} {@,%a@]@,@[<v 2>} else {@,%a@]@,}@]" 
                        print_expr e print_seq s1 print_seq s2
                        else
                          fprintf fmt 
                          "@[<v>@[<v 2>if (@[%a@]) {@,%a@]@,@[<v 2>} else {@,%a@]@,}@]" 
                          print_expr e print_seq s1 print_seq s2
  | While(e, s, id) -> mark_open fmt id; fprintf fmt "@[<v>@[<v 2>while (@[%a@]) {@,%a@]@,}@]"
                     print_expr e print_seq s; mark_close fmt id
  | Return (e, id) -> mark_open fmt id; fprintf fmt "return(@[%a@]);" print_expr e; mark_close fmt id
  | Expr (e, id) -> mark_open fmt id; fprintf fmt "@[%a@];" print_expr e; mark_close fmt id
  | SetArr(t, i, e, id) -> mark_open fmt id; fprintf fmt "%a[%a] = @[%a@];" 
                         print_expr t print_expr i print_expr e; mark_close fmt id
and print_seq fmt = function
  | [] -> fprintf fmt ""
  | [i] -> fprintf fmt "%a" print_instr i
  | i::seq -> fprintf fmt "@[<v>%a@,%a@]" print_instr i print_seq seq

let rec print_params fmt = function
  | [] -> fprintf fmt ""
  | [x] -> fprintf fmt "%s" x
  | x::params -> fprintf fmt "%s,@ %a" x print_params params

let rec print_vars fmt = function
  | [] -> fprintf fmt ""
  | (x, None, id)::vars -> mark_open fmt id;  fprintf fmt "@[<v>var %s;@,%a@]" x print_vars vars; mark_close fmt id
  | (x, Some e, id)::vars -> mark_open fmt id;  fprintf fmt "@[<v>var %s = @[%a@];@,%a@]" 
                           x print_expr e print_vars vars; mark_close fmt id

let print_fdef fmt fdef =
  fprintf fmt "@[<v>@[<v 2>function %s(@[%a@]) {@,%a@,%a@]@,}@,@]" 
    fdef.name print_params fdef.params print_vars fdef.locals print_seq fdef.code

let rec print_functions fmt = function
  | [] -> fprintf fmt ""
  | fdef::functions -> fprintf fmt "@[<v>%a@,%a@]" print_fdef fdef print_functions functions

let print_program fmt p =
  pp_set_tags str_formatter true;
  pp_set_formatter_stag_functions str_formatter tags_funs;
  fprintf fmt "@[<v>%a@,%a@]@." print_vars p.globals print_functions p.functions;
  flush_str_formatter ()
