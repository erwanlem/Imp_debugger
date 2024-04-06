open Format
open Imp
open Global

let find_instr = ref false
let instr_id = ref (0)

let write_out msg =
  let file = "log.txt" in
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" msg;
  (* write something *)
  close_out oc


let color_breakpoint l =
  if Hashtbl.mem breakpoints l then
    Format.sprintf "\x1b[31m%d\x1b0\x1b[37m" l
  else
    Format.sprintf "\x1b[97m%d\x1b0\x1b[37m" l


let tags_funs =
  {
    mark_open_stag = (fun s -> match s with String_tag s -> ("<" ^ s ^ ">") | _ -> "");
    mark_close_stag = (fun s -> match s with String_tag s -> ("</" ^ s ^ ">") | _ -> "");
    print_open_stag = (fun _ -> ());
    print_close_stag = (fun _ -> ())
  }

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
  | Print (e, id, l) -> if !instr_id = id then
                          fprintf fmt "%s @{<color>print(@[%a@]);@}" (color_breakpoint l.pos_lnum) print_expr e
                        else
                          fprintf fmt "%s print(@[%a@]);" (color_breakpoint l.pos_lnum) print_expr e
  | Set(x, e, id, l) -> if !instr_id = id then
                          fprintf fmt "%s @{<color>%s = @[%a@];@}" (color_breakpoint l.pos_lnum) x print_expr e
                        else
                          fprintf fmt "%s %s = @[%a@];" (color_breakpoint l.pos_lnum) x print_expr e
  | If(e, s1, s2, id, l) ->
                        if !instr_id = id then begin
                          if s2 = [] then 
                            fprintf fmt 
                            "@[<v>@[<v 2>%s @{<color>if (@[%a@])@} {@,%a@]@,@[<v 2>} @]@]" 
                              (color_breakpoint l.pos_lnum) print_expr e print_seq s1
                          else
                            fprintf fmt 
                            "@[<v>@[<v 2>%s @{<color>if (@[%a@])@} {@,%a@]@,@[<v 2>} else {@,%a@]@,}@]" 
                              (color_breakpoint l.pos_lnum) print_expr e print_seq s1 print_seq s2
                        end
                        else begin
                          if s2 = [] then 
                            fprintf fmt 
                            "@[<v>@[<v 2>%s if (@[%a@]) {@,%a@]@,@[<v 2>} @]@]" 
                              (color_breakpoint l.pos_lnum) print_expr e print_seq s1
                          else
                            fprintf fmt 
                            "@[<v>@[<v 2>%s if (@[%a@]) {@,%a@]@,@[<v 2>} else {@,%a@]@,}@]" 
                              (color_breakpoint l.pos_lnum) print_expr e print_seq s1 print_seq s2
                        end
  | While(e, s, id, l) ->
                        if !instr_id = id then
                          fprintf fmt " @[<v>@[<v 2>%s @{<color>while (@[%a@]) {@}@,%a@]@,}@]"
                          (color_breakpoint l.pos_lnum) print_expr e print_seq s
                        else
                          fprintf fmt " @[<v>@[<v 2>%s while (@[%a@]) {@,%a@]@,}@]"
                          (color_breakpoint l.pos_lnum) print_expr e print_seq s
  | Return (e, id, l) -> if !instr_id = id then
                          fprintf fmt "%s @{<color>return(@[%a@]);@}" (color_breakpoint l.pos_lnum) print_expr e
                        else
                          fprintf fmt "%s return(@[%a@]);" (color_breakpoint l.pos_lnum) print_expr e
  | Expr (e, id, l) -> if !instr_id = id then
                        fprintf fmt "%s @{<color>@[%a@];@}" (color_breakpoint l.pos_lnum) print_expr e
                      else
                        fprintf fmt "%s @[%a@];" (color_breakpoint l.pos_lnum) print_expr e
  | SetArr(t, i, e, id, l) -> if !instr_id = id then
                                fprintf fmt "%s @{<color>%a[%a] = @[%a@];@}"
                                  (color_breakpoint l.pos_lnum) print_expr t print_expr i print_expr e
                            else
                              fprintf fmt "%s %a[%a] = @[%a@];"
                                  (color_breakpoint l.pos_lnum) print_expr t print_expr i print_expr e
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
  | (x, None, id, (l:Lexing.position))::vars ->
                          if id = !instr_id then
                            fprintf fmt "@[<v>%s @{<color>var %s;@}@,%a@]" (color_breakpoint l.pos_lnum) x print_vars vars
                          else
                            fprintf fmt "@[<v>%s var %s;@,%a@]" (color_breakpoint l.pos_lnum) x print_vars vars
  | (x, Some e, id, (l:Lexing.position))::vars -> 
                          if id = !instr_id then
                            fprintf fmt "@[<v>%s @{<color>var %s = @[%a@];@}@,%a@]" (color_breakpoint l.pos_lnum) x print_expr e print_vars vars
                          else
                            fprintf fmt "@[<v>%s var %s = @[%a@];@,%a@]" (color_breakpoint l.pos_lnum) x print_expr e print_vars vars

let print_fdef fmt fdef =
  if fdef.id = !instr_id then
    fprintf fmt " @[<v>@[<v 2>%s @{<color>function %s(@[%a@]) @} {@,%a@,%a@]@,}@,@]" 
    (color_breakpoint fdef.line.pos_lnum) fdef.name print_params fdef.params print_vars fdef.locals print_seq fdef.code
  else
    fprintf fmt " @[<v>@[<v 2>%s function %s(@[%a@]) {@,%a@,%a@]@,}@,@]" 
    (color_breakpoint fdef.line.pos_lnum) fdef.name print_params fdef.params print_vars fdef.locals print_seq fdef.code

let rec print_functions fmt = function
  | [] -> fprintf fmt ""
  | fdef::functions -> fprintf fmt "@[<v>%a@,%a@]" print_fdef fdef print_functions functions

let print_program fmt p =
  pp_set_tags str_formatter true;
  pp_print_if_newline str_formatter ();
  pp_set_formatter_stag_functions str_formatter tags_funs;
  fprintf fmt "@[<v>%a@,%a@]@." print_vars p.globals print_functions p.functions;
  flush_str_formatter ()
