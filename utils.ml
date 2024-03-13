open Imp


let get_instr_id = function
  | Print(_, id) | Return(_, id) | Expr(_, id) -> id
  | Set(_, _, id) | While(_, _, id) -> id
  | If(_, _, _, id) | SetArr(_, _, _, id) -> id