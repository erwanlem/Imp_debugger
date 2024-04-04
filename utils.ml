open Imp

(* Generates unique id for arrays *)
let gen_array_id =
  let i = ref 0 in
  fun () ->
    let v = !i in
    i := !i + 1;
    v

(* Returns id of an instruction *)
let get_instr_id = function
  | Print(_, id, _) | Return(_, id, _) | Expr(_, id, _) -> id
  | Set(_, _, id, _) | While(_, _, id, _) -> id
  | If(_, _, _, id, _) | SetArr(_, _, _, id, _) -> id

(* Returns line id of an instruction *)
let get_instr_line = function
  | Print(_, _, l) | Return(_, _, l) | Expr(_, _, l) -> l.pos_lnum
  | Set(_, _, _, l) | While(_, _, _, l) -> l.pos_lnum
  | If(_, _, _, _, l) | SetArr(_, _, _, _, l) -> l.pos_lnum