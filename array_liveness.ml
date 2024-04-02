open Global
open Imp

let array_addr = (Hashtbl.create 10 : (int, bool * id_array) Hashtbl.t)

let add_array (a : Imp.id_array) =
  Hashtbl.replace array_addr a.id (true, a)

let reset_mark () =
  Hashtbl.iter (fun k (b, v) ->
    Hashtbl.replace array_addr k (false, v)) array_addr

let mark_liveness () =
  let rec mark (a : id_array) =
    Hashtbl.replace array_addr a.id (true, a);
    Array.iter (fun e ->
      match e with
      | VArray a' -> mark a'
      | _ -> ()) a.array
  in
  Env.iter (fun _ v ->
    match v with
    | VArray a -> mark a
    | _ -> () ) !env;
  Env.iter (fun _ v ->
    match v with
    | VArray a -> mark a
    | _ -> () ) !global_env