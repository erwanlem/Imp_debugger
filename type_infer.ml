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
  let rec sub_aux x t v =
    match v with
      | TVar _ when v = x -> t
      | Fun (a, b) -> Fun(sub_aux x t a, sub_aux x t b)
      | _ -> v
  in
  List.fold_left (
    fun acc (s, t') ->
      (sub_aux x t s, sub_aux x t t') :: acc
  ) [] c
  

(* Unification des contraintes *)
let rec unify c =
  match c with
  | [] -> []
  | (s, t) :: c' when s = t -> unify c'
  | (TVar x, t) :: c' when not (var_in x t) -> (unify ((substitution (TVar x) t c'))) @ [(TVar x, t)]
  | (t, TVar x) :: c' when not (var_in x t) -> (unify ((substitution (TVar x) t c'))) @ [(TVar x, t)]
  | (TArray a, TArray b) :: c' -> unify (c' @ [(a, b)])
  | (Fun (s1, s2), Fun (t1, t2)) :: c' -> unify (c' @ [(s1, t1); (s2, t2)])
  | (s, t) :: _ -> failwith ("Fail " ^ (typ_to_string s) ^ " = " ^ (typ_to_string t))