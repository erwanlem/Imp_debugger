open Format

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


let () = printf "%s" (flush_str_formatter ()) 