open Imp

let instr_id = ref 0


let buffer = ref ""

let window = ref None

let get_window () =
  match !window with
  | Some w -> w
  | None   -> failwith "Uninitialized window"

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
        let _err = Curses.move (y-1) 0 in ();
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
      if input = Curses.Key.up then
        let _err = Curses.addch (int_of_char 'U') in match_key match_entry
    else
      if input = Curses.Key.down then
        let _err = Curses.addch (int_of_char 'D') in match_key match_entry
    else
      if input = Curses.Key.backspace then
        (let y, x = Curses.getyx window in
        let _err = Curses.mvdelch y (x-1) in
        if String.length (!buffer) > 0 then
          buffer := String.sub (!buffer) 0 (String.length (!buffer)-1);
        let _err = Curses.refresh () in
        match_key match_entry)
    else
      match_key match_entry
    )


let init_console () =
  let w = Curses.initscr () in
  let _err = Curses.keypad w true in
  let y, x = Curses.getmaxyx w in
  let _err = Curses.move (y-1) (0) in
  window := Some w
  

let close_console () : unit =
  Curses.endwin ()




let print_env env env_global =
  let window = get_window () in
  let y, x = Curses.getyx window in
  let h, w = Curses.get_size () in
  let begy, begx = Curses.getbegyx window in
  let _err = Curses.move begy begx in
  Curses.hline (Char.code '-') w;
  ignore (Curses.move (begy+1) (begx));

  (* Affichage des variables *)
  let rec print_value = function
  | VBool b  -> Printf.sprintf "%b" b
  | VInt  i  -> Printf.sprintf "%d" i
  | VArray a -> "[ " ^ Array.fold_left (fun acc e -> acc ^ print_value e ^ Printf.sprintf "; ") "" a ^ "]"
  | Null     -> Printf.sprintf "Null"
  in
  (Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      let _err = Curses.addstr out in ()
      
      ) env_global);
  Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      let _err = Curses.addstr out in ()
      
      ) env;

  let _err = Curses.move y x in ()

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
  let _err = Curses.addstr str in
  let y, x = Curses.getyx window in
  let _err = Curses.move (y+1) 1 in
  ()

let clear_console () =
  let window = get_window () in
  let y, x = Curses.getyx window in
  Curses.erase ();
  Curses.clear ();
  Curses.move y x



let print_code prog =
  Imppp.instr_id := !instr_id;
  Imppp.find_instr := false;
  let window = get_window () in
  let y, x = Curses.getyx window in
  let begy, begx = Curses.getbegyx window in
  let _err = Curses.move (begy+10) begx in
  let code = Printf.sprintf "%s" (Imppp.print_program prog) in
  let current_instr = List.nth (String.split_on_char '%' code) 1 in
  let code = List.nth (String.split_on_char '%' code) 2 in
  let _err = Curses.start_color () in
  let _err = Curses.init_pair 1 Curses.Color.green Curses.Color.green in
  Curses.attron (Curses.color_pairs ());
  let _err = Curses.addstr (current_instr) in
  Curses.attr_off (Curses.color_pairs ());
  let _err = Curses.use_default_colors () in

  let _err = Curses.addstr (code) in
  let _err = Curses.move y x in ()



let write_out msg =
  let file = "log.txt" in
  (* Write message to file *)
  let oc = open_out file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" msg;
  (* write something *)
  close_out oc