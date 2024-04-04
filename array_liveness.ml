open Global
open Imp

let arrays = ref [| |]
let marked = ref []

let recover = ref []

(*
let array_addr = ref (Hashtbl.create 10 : (int, bool * id_array) Hashtbl.t)
*)


let state_back () =
  arrays := List.hd !recover;
  recover := List.tl !recover

let save_state () =
  recover := (Array.copy !arrays) :: !recover

let add_array (a : Imp.id_array) =
  if not (Array.mem a !arrays) then
    arrays := Array.append !arrays [| a |]

let list_arrays () =
  Array.fold_left (fun acc (e:id_array) -> 
    if (List.mem e.id !marked) then
      (true, e) :: acc
    else
      (false, e) :: acc
    ) [] !arrays

let reset_mark () =
  marked := []


let mark_liveness () =
  let rec mark (a : id_array) =
    add_array a;
    marked := a.id :: !marked;
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