type value =
  | TInt
  | TBool
  | TArray
  | TNull


type typ =
  | Var of string
  | Type of value
  | Fun of typ * typ


let rec var_in v t =
  match t with
  | Var name -> if v = name then true else false
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
        | Var s, _ -> ()
        | _, Var s -> ()
        | Fun (s1, s2), Fun (t1, t2) -> () 
        | _ -> failwith "fail"