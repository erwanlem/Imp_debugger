type typ =
  | TVar of string
  | TInt
  | TBool
  | TNull
  | TArray of typ
  | Fun of typ * typ

let rec typ_to_string = function
| TInt -> "int"
| TBool -> "bool"
| TArray a -> "array"
| TNull  -> "null"
| TVar v  -> "Var " ^ v
| Fun (t1, t2) -> "Fun " ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2   


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
        if s = x && t' = x then (t, t) :: acc 
        else if s = x then (t, t')::acc 
        else if t' = x then (s, t)::acc 
        else (s,t')::acc 
      
      | TVar v, _ ->
        if s = x then (t, t')::acc
        else (s, t')::acc
      | _, TVar v ->
        if t' = x then (s, t)::acc
        else (s, t')::acc
      | _, _ -> (s, t)::acc
  ) [] c
  

(* Unification des contraintes *)
let rec unify c =
  match c with
  | [] -> []
  | (s, t) :: c' when s = t -> unify c'
  | (TVar x, t) :: c' when not (var_in x t) -> substitution (TVar x) t (unify ((substitution (TVar x) t c'))) 
  | (t, TVar x) :: c' when not (var_in x t) -> substitution (TVar x) t (unify ((substitution (TVar x) t c')))
  | (Fun (s1, s2), Fun (t1, t2)) :: c' -> unify (c' @ [(s1, t1); (s2, t2)])
  | (s, t) :: _ -> failwith ("Fail " ^ (typ_to_string s) ^ " = " ^ (typ_to_string t))