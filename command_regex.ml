(*
  This file contains regular expressions to match with user inputs
*)



let reg_next = Str.regexp "next"
let reg_undo = Str.regexp "undo"
let reg_so = Str.regexp "so"
let reg_exit = Str.regexp "exit"
let reg_break = Str.regexp "break \\(0\\|[1-9][0-9]*\\)"
let reg_int_param = Str.regexp "0\\|[1-9][0-9]*"
let reg_step = Str.regexp "step"


(* 
  -- Get command value from user entry --
  If it finds a regular expression that match with the entry returns the command name (without parameters)
  Otherwise returns empty string
*)
let get_command c =
  if Str.string_match reg_next c 0 then "next"
  else if Str.string_match reg_undo c 0 then "undo"
  else if Str.string_match reg_so c 0 then "so"
  else if Str.string_match reg_exit c 0 then "exit"
  else if Str.string_match reg_break c 0 then "break"
  else if Str.string_match reg_step c 0 then "step"
  else ""

(*
  Get int parameter from user input
  Suppose that there's only one parameter
  If there is no integer throw error 
*)
let get_int_param c =
  if Str.search_forward reg_int_param c 0 > 0 then int_of_string (Str.matched_string c)
  else failwith "Int parameter not found"