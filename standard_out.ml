open Imp

let instr_id = ref 0


let buffer = ref ""


let get_window () =
  ()




(********************************************
    
      Tag detection for code string

*********************************************)

exception NoColorTag

let tag_name = "color"

let reg = Str.regexp ("<color>\\(.\\|[\n]\\)*</color>")
let open_reg = Str.regexp ("<" ^ tag_name ^ ">")
let close_reg = Str.regexp ("</" ^ tag_name ^ ">")


let get_color_tag s =
  try 
    ignore (Str.search_forward reg s 0);
    (let s = Str.matched_string s in
    let b = Str.match_beginning () in
    let e = Str.match_end () in
    let s' = Str.global_replace open_reg "" s in
    (b, e, (Str.global_replace close_reg "" s')))
  with Not_found -> raise NoColorTag


(* Returns string
    (before color tag, after color tag , color tag) 
*)
let get_str_parts str =
  let b, e, t = get_color_tag str in
  let part1 = String.sub str 0 b in
  let part2 = String.sub str e (String.length str-e) in
  (part1, part2, t)


(*******************************************************)
(*******************************************************)
(*******************************************************)







let rec match_key match_entry =
  let input = read_line () in
  match_entry input; match_key match_entry


(* get the console ready for display
  Mainly used when using ncurses library   
*)
let init_console () =
  if (Sys.command "clear") = 0 then ()
  else failwith "Can't init console"
  
let close_console () : unit =
  Printf.printf "\nExecution finished\n";
  exit 0



let print_env env env_global =
  (* Affichage des variables *)
  let rec print_value = function
  | VBool b  -> Printf.sprintf "%b" b
  | VInt  i  -> Printf.sprintf "%d" i
  | VArray a -> "[ " ^ Array.fold_left (fun acc e -> acc ^ print_value e ^ Printf.sprintf "; ") "" a ^ "]"
  | VNull     -> Printf.sprintf "Null"
  in
  (Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      Printf.printf "%s" out
      ) env_global);
  Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      Printf.printf "%s" out
      ) env


let print_line str =
  Printf.printf "%s" str

let clear_console () =
  Sys.command "clear"



let print_code prog =
  Imppp.instr_id := !instr_id;
  Imppp.find_instr := false;
  let code = Format.sprintf "%s" (Imppp.print_program Format.str_formatter prog) in
  (try 
  let p1, p2, t = get_str_parts code in
  
  Printf.printf "\x1b[37m%s\x1b0" p1;

  (* current instruction coloration *)
  Printf.printf "\x1b[31m%s\x1b0" t;
  
  Printf.printf "\x1b[37m%s\x1b0" p2;

  Imppp.write_out (code)
  with NoColorTag -> (Printf.printf "%s" code; Imppp.write_out code) )



let write_out msg =
  let file = "log.txt" in
  (* Write message into file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" msg;
  (* write something *)
  close_out oc