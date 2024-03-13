open Imp

let instr_id = ref 0


let buffer = ref ""

let window = ref None

let get_window () =
  match !window with
  | Some w -> w
  | None   -> failwith "Uninitialized window"




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
  let window = get_window () in
  let _err = Curses.noecho () in
  let input = Curses.getch () in
  if input >= 0 && input <= 255 then
    (* Caractères ASCII *)
    (if input = Char.code '\n' then
        (let _err=Curses.clrtoeol () in ();
        let _err=Curses.deleteln () in ();
        let y, x = Curses.getmaxyx window in
        (*let _err = Curses.wmove window (y-1) 0 in ();*)
        let _err = Curses.refresh () in ();
        (if not (match_entry (!buffer) ()) then ()
        else (buffer := ""; match_key match_entry)))
    else
      let _err = Curses.addch input in
      (buffer := !buffer ^ (Char.escaped (char_of_int input)); match_key match_entry) )
  else (* Entrées caractères spéciaux *)
    (if input = Curses.Key.left then
      if not (match_entry "undo" ()) then () else match_key match_entry
    else
      if input = Curses.Key.right then
        if not (match_entry "next" ()) then () else match_key match_entry
    else
      if input = Curses.Key.backspace then
        (let y, x = Curses.getyx window in
        let _err = Curses.mvdelch y (x-1) in
        if String.length (!buffer) > 0 then
          buffer := String.sub (!buffer) 0 (String.length (!buffer)-1);
        let _err = Curses.wrefresh window in
        match_key match_entry)
    else
      if input = Curses.Key.up then
        let _err = Curses.wscrl window (-1) in
        let _err = Curses.wrefresh window in
        match_key match_entry
    else
      if input = Curses.Key.down then
        let _err = Curses.wscrl window (1) in
        let _err = Curses.wrefresh window in
        match_key match_entry
    else
      match_key match_entry
    )


let init_console () =
  let w = Curses.initscr () in
  let _err = Curses.keypad w true in
  let y, x = Curses.getmaxyx w in
  (*let _err = Curses.wmove win (y-1) (0) in*)
  Curses.scrollok w true;
  window := Some w
  

let close_console () : unit =
  Curses.endwin ()




let print_env env env_global =
  let window = get_window () in
  let y, x = Curses.getyx window in
  let h, w = Curses.get_size () in
  let begy, begx = Curses.getbegyx window in
  (*let _err = Curses.wmove window begy begx in*)
  Curses.hline (Char.code '-') w;
  (*ignore (Curses.wmove window (begy+1) (begx));*)

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
      let _err = Curses.waddstr window out in ()
      
      ) env_global);
  Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      let _err = Curses.waddstr window out in ()
      
      ) env;

  (*let _err = Curses.wmove window y x in ();*)
  let _err = Curses.refresh () in ()

  (* Affichage des variables *)
  (*let rec print_value = function
  | VBool b  -> Printf.sprintf "%b" b
  | VInt  i  -> Printf.sprintf "%d" i
  | VArray a -> "[ " ^ Array.fold_left (fun acc e -> acc ^ print_value e ^ Printf.sprintf "; ") "" a ^ "]"
  | Null     -> Printf.sprintf "Null"
  in
  Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "" in
      Printf.printf "%s%!" out
      
      ) env;
  Printf.printf "\n%!";
  let y, x = Curses.getyx win in
  let _err = Curses.move y 1 in ()*)


let print_line str =
  let window = get_window () in
  let _err = Curses.waddstr window str in
  let y, x = Curses.getyx window in
  (*let _err = Curses.wmove window (y+1) 1 in*)
  ()

let clear_console () =
  let window = get_window () in
  let y, x = Curses.getyx window in
  Curses.erase ();
  Curses.clear ()
  (*Curses.wmove window y x*)



let print_code prog =
  Imppp.instr_id := !instr_id;
  Imppp.find_instr := false;
  let window = get_window () in
  let y, x = Curses.getyx window in
  let begy, begx = Curses.getbegyx window in
  (*let _err = Curses.wmove window (begy+10) begx in*)
  let code = Format.sprintf "%s" (Imppp.print_program Format.str_formatter prog) in
  (try 
  let p1, p2, t = get_str_parts code in
  
  ignore (Curses.waddstr window (p1));
  let _err = Curses.start_color () in
  let _err = Curses.init_pair 1 Curses.Color.black Curses.Color.green in
  Curses.attron (Curses.A.color_pair 1);
  ignore (Curses.waddstr window (t));
  Curses.attr_off (Curses.A.color_pair 1);
  let _err = Curses.use_default_colors () in
  ignore (Curses.waddstr window (p2));
  Imppp.write_out (code)
  with NoColorTag -> (ignore (Curses.waddstr window (code)); Imppp.write_out code) );
  (*let _err = Curses.wmove window y x in ();*)
  let _err = Curses.refresh () in ()



let write_out msg =
  let file = "log.txt" in
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" msg;
  (* write something *)
  close_out oc