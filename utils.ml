open Imp


let get_instr_id = function
  | Print(_, id, _) | Return(_, id, _) | Expr(_, id, _) -> id
  | Set(_, _, id, _) | While(_, _, id, _) -> id
  | If(_, _, _, id, _) | SetArr(_, _, _, id, _) -> id

let get_instr_line = function
  | Print(_, _, l) | Return(_, _, l) | Expr(_, _, l) -> l.pos_lnum
  | Set(_, _, _, l) | While(_, _, _, l) -> l.pos_lnum
  | If(_, _, _, _, l) | SetArr(_, _, _, _, l) -> l.pos_lnum