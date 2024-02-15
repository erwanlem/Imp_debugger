open Format

exception NoColorTag

let tags_funs =
  {
    mark_open_stag = (fun s -> match s with String_tag s -> ("<" ^ s ^ ">") | _ -> "");
    mark_close_stag = (fun s -> match s with String_tag s -> ("</" ^ s ^ ">") | _ -> "");
    print_open_stag = (fun _ -> ());
    print_close_stag = (fun _ -> ())
  }


let () =
  pp_set_tags str_formatter true;
  pp_set_formatter_stag_functions str_formatter tags_funs;
  fprintf str_formatter "@{<color>Hello World !@} @."


let w = {| Lorem ipsum dolor sit amet, consectetur adipiscing elit.
 Vivamus vitae erat tempor, tincidunt orci ultrices, facilisis turpis. 
 Nullam lorem est, sodales eu elementum nec, dapibus eu lectus. Pellentesque 
 at ex porta, blandit purus sed, <color>sollicitudin est</color>. Nam molestie ultricies magna. |}


let tag_name = "color"

let reg = Str.regexp ("<" ^ tag_name ^ ">.*<\\/" ^ tag_name ^ ">")
let open_reg = Str.regexp ("<" ^ tag_name ^ ">")
let close_reg = Str.regexp ("<\\/" ^ tag_name ^ ">")


let get_color_tag s =
  try 
    ignore (Str.search_forward reg s 0);
    (let s = Str.matched_string w in
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
  let part1 = String.sub w 0 b in
  let part2 = String.sub w e (String.length w-e) in
  (part1, part2, t)

let p1, p2, tg = get_str_parts w
let () = Printf.printf "%s\n\n%!" p1
let () = Printf.printf "%s\n\n%!" p2
let () = Printf.printf "%s\n\n%!" tg