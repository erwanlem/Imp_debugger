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
  

(* Unification des contraintes *)
let rec unify c =
  match c with
  | [] -> []
  | (s, t) :: c' when s = t -> unify c'
  | (TVar x, t) :: c' when not (var_in x t) -> unify (substitution x t c') @ (substitution x t [(TVar x, t)])
  | (t, TVar x) :: c' when not (var_in x t) -> unify (substitution x t c') @ (substitution x t [(t, TVar x)])
  | (Fun (s1, s2), Fun (t1, t2)) :: c' -> unify (c' @ [(s1, t1); (s2, t2)])
  | _ -> failwith "Fail"
