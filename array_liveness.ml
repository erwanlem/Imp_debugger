open Global
open Imp

(**
    This file contains functions which follow arrays in memory.
    At each instruction of the program, arrays are added to 
    the `arrays` variable (if they're not already in).
    We then follow variables and each time we find an array
    we add its id in the marked variable.
    Every array id in marked are alive. The others are free.
*)


(* List of all arrays (alive and free) *)
let arrays = ref []

(* Id of followed array *)
let marked = ref []

(* Stack of the states *)
let recover = ref []

(*
let array_addr = ref (Hashtbl.create 10 : (int, bool * id_array) Hashtbl.t)
*)


(* Gets back to the previous state *)
let state_back () =
  arrays := List.hd !recover;
  recover := List.tl !recover

(* Makes a snapshot of the current memory state *)
let save_state () =
  recover := !arrays :: !recover

(* Appends array `a` in the `arrays` variable
   if it's not already in 
*)
let add_array (a : Imp.id_array) =
  if not (List.mem a !arrays) then
    arrays := a::!arrays

(* Get the current memory state with for each 
   array in memory:
   (true, a)  if a is alive
   (false, a) if a is free  
*)
let list_arrays () =
  List.fold_left (fun acc (e:id_array) -> 
    if (List.mem e.id !marked) then
      (true, e) :: acc
    else
      (false, e) :: acc
    ) [] (List.sort (fun (a:id_array) b -> if a.id < b.id then 1 else (if a.id = b.id then 0 else -1) ) !arrays)

(* Reset marks *)
let reset_mark () =
  marked := []

  
(* Follows variables and marks arrays alive in memory *)
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