type typ =
  | TVar of string
  | TInt
  | TBool
  | TNull
  | TArray of typ
  | Fun of typ * typ


let rec var_in v t =
  match t with
  | TVar name -> v = name
  | Fun (t1, t2) -> var_in v t1 || var_in v t2
  | _ -> false


(* Remplace x par t dans h
   avec c une liste de contraintes *)
let substitution x t c =
  List.fold_left (
    fun acc (s, t') ->
      match s, t' with
      | TVar v, TVar v' -> 
        if s = t && t' = t then (t, t) :: acc 
        else if s = t then (t, t')::acc 
        else if t' = t then (s, t)::acc 
        else (s,t')::acc 
      
      | TVar v, _ ->
        if s = t then (t, t')::acc
        else (s, t')::acc
      | _, TVar v ->
        if t' = t then (s, t)::acc
        else (s, t')::acc
      | _, _ -> (s, t)::acc
  ) [] c
  
(*
(* Unification des contraintes *)
let rec unify c h =
  let if_pattern v cond =
    match v with
    | TVar v -> if cond = "TVar" then v else ""
    | Type t -> if cond = "Type" then "." else ""
    | Fun (c1, c2) -> if cond = "Fun" then "." else ""
  in
  match c with
  | [] -> ()
  | (s, t) :: c' when s = t -> unify c' h*
  | (TVar x, t) :: c' | ... when ... -> 
    else if if_pattern s "TVar" <> "" && not (var_in (if_pattern s "TVar") t) then
      match s with TVar v ->
        ()
      | _ -> failwith "Fail"
    else if if_pattern t "TVar" <> "" && not (var_in (if_pattern t "TVar") s) then
      match t with TVar t' -> ()
      | _ -> failwith "Fail"
    else if if_pattern s "Fun" <> "" && if_pattern t "Fun" <> "" then
      match s, t with 
      Fun (s1, s2), Fun (t1, t2) -> ()
      | _ -> failwith "Fail"
    else
      failwith "Fail unification"

*)
