type base_type =
  | TInt
  | TBool
  | TArray
  | TNull
  | TAlpha


type typ =
  | TVar of string * base_type
  | Type of base_type
  | Fun of typ * typ


let rec var_in v t =
  match t with
  | TVar (name, bt) -> if v = name then true else false
  | Type _ -> false
  | Fun (t1, t2) -> var_in v t1 && var_in v t2 




let rec unify c =
  match c with
  | [] -> ()
  | (s, t) :: c' ->
    if s = t then
      unify c'
    else
      match s, t with
        | TVar (s, bt), _ -> ()
        | _, TVar (s, bt) -> ()
        | Fun (s1, s2), Fun (t1, t2) -> () 
        | _ -> failwith "fail"