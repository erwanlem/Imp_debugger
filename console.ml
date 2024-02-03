open Imp

let buffer = ref ""


let rec match_key win match_entry =
  let _err = Curses.noecho () in
  let input = Curses.getch () in
  if input >= 0 && input <= 255 then
    (* Caractères ASCII *)
    (if input = Char.code '\n' then
        (let _err=Curses.clrtoeol () in ();
        let _err=Curses.deleteln () in ();
        let y, x = Curses.getmaxyx win in
        let _err = Curses.move (y-1) 0 in ();
        let _err = Curses.refresh () in ();
        (if not (match_entry (!buffer) ()) then ()
        else (buffer := ""; match_key win match_entry)))
    else
      let _err = Curses.addch input in
      (buffer := !buffer ^ (Char.escaped (char_of_int input)); match_key win match_entry) )
  else (* Entrées caractères spéciaux *)
    (if input = Curses.Key.left then
      if not (match_entry "undo" ()) then () else match_key win match_entry
    else
      if input = Curses.Key.right then
        if not (match_entry "next" ()) then () else match_key win match_entry
    else
      if input = Curses.Key.up then
        let _err = Curses.addch (int_of_char 'U') in match_key win match_entry
    else
      if input = Curses.Key.down then
        let _err = Curses.addch (int_of_char 'D') in match_key win match_entry
    else
      if input = Curses.Key.backspace then
        (let y, x = Curses.getyx win in
        let _err = Curses.mvdelch y (x-1) in
        if String.length (!buffer) > 0 then
          buffer := String.sub (!buffer) 0 (String.length (!buffer)-1);
        let _err = Curses.refresh () in
        match_key win match_entry)
    else
      match_key win match_entry
    )


let init_console () : Curses.window =
  let window = Curses.initscr () in
  let _err = Curses.keypad window true in
  let y, x = Curses.getmaxyx window in
  let _err = Curses.move (y-1) (0) in
  window

let close_console () : unit =
  Curses.endwin ()




let print_env win env =
  let y, x = Curses.getyx win in
  let h, w = Curses.get_size () in
  let begy, begx = Curses.getbegyx win in
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
  Env.iter (
    fun s v ->
      let out = s ^ ": " ^ (print_value v) ^ "\n" in
      let _err = Curses.addstr out in ()
      
      ) env;

  let _err = Curses.move y x in ()


let print_out win v =
  (*let y, x = Curses.getyx win in*)
  ()

let clear_console win =
  let y, x = Curses.getyx win in
  Curses.erase ();
  Curses.clear ();
  Curses.move y x;
