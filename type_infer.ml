(* Types used in typechecker *)
type typ =
  | TVar of string
  | TInt
  | TBool
  | TNull
  | TArray of typ


let rec typ_to_string = function
| TInt -> "int"
| TBool -> "bool"
| TArray a -> "array"
| TNull  -> "null"
| TVar v  -> "Var " ^ v


(* check if var v is in type t *)
let rec var_in v t =
  match t with
  | TVar name -> v = name
  | _ -> false


(* Remplace x par t dans h
   avec c une liste de contraintes *)
let substitution x t c =
  let rec sub_aux x t v =
    match v with
      | TVar _ when v = x -> t
      | _ -> v
  in
  List.fold_left (
    fun acc (s, t') ->
      (sub_aux x t s, sub_aux x t t') :: acc
  ) [] c
  

(* Constraints unification *)
let rec unify c =
  match c with
  | [] -> []
  | (s, t) :: c' when s = t -> unify c'
  | (TVar x, t) :: c' when not (var_in x t) -> (unify ((substitution (TVar x) t c'))) @ [(TVar x, t)]
  | (t, TVar x) :: c' when not (var_in x t) -> (unify ((substitution (TVar x) t c'))) @ [(TVar x, t)]
  | (TArray a, TArray b) :: c' -> unify (c' @ [(a, b)])
  | (s, t) :: _ -> failwith ("Fail " ^ (typ_to_string s) ^ " = " ^ (typ_to_string t))