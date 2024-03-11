open Imp

type types =
  | TInt
  | TBool
  | TArray
  | TUndef
  | TNull

let types_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArray -> "array"
  | TUndef -> ""
  | TNull  -> "null"

exception Error of string
let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (types_to_string ty_expected) (types_to_string ty_actual))

module Env = Map.Make(String)
type tenv = types Env.t

let add_env l tenv =
  List.fold_left (fun env (s, t, _) -> match t with Some t -> Env.add s TUndef env
                                                    | None -> Env.add s TUndef env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let func =
    List.fold_left (fun env f -> Env.add f.name f env) Env.empty p.functions in


  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ && typ_e <> TUndef && typ <> TUndef then type_error typ_e typ

  and type_expr e tenv = match e with
    | Null   -> TNull
    | Int i  -> TInt
    | Bool _ -> TBool
    | Var s  -> TUndef
    (* Binop INT *)
    | Binop(Add, e1, e2) | Binop(Sub, e1, e2) | Binop(Mul, e1, e2) 
    | Binop(Div, e1, e2) | Binop(Rem, e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TInt
    (* Binop BOOL *)
    | Binop(Lt , e1, e2) | Binop(Le , e1, e2) 
    | Binop(Gt , e1, e2) | Binop(Ge , e1, e2) -> check e1 TInt tenv; check e2 TInt tenv; TBool
    | Binop(Neq, e1, e2) | Binop(Eq, e1, e2)  -> check e1 (type_expr e2 tenv) tenv; TBool
    | Binop(And, e1, e2) | Binop(Or, e1, e2)  -> check e1 TBool tenv; check e2 TBool tenv; TBool
    (* Unop *)
    | Unop(Opp, e)       -> check e TInt tenv; TInt
    | Unop(Not, e)       -> check e TBool tenv; TBool
    
    | Call (s, l)    -> 
      if not (Env.mem s func) then error ("Undefined function " ^ s)
      else let f = Env.find s func in
      if List.length f.params = List.length l then TUndef else error "Invalid arguments"

    | Array l        -> TArray
    | GetArr (a, i)  -> TUndef
    | Continuation   -> TUndef
                            
  in

  let rec check_instr i tenv = match i with
    | Print (e, _) -> () (* Accepte tous les types *)
    | If(e, i1, i2, _) -> check e TBool tenv
    | While(e, s, _)   -> check e TBool tenv
    | Expr(e, _)       -> check e TUndef tenv
    | Return(e, _)     -> ()
    | Set(m, e, _)     -> check e TUndef tenv
    | SetArr (e1, e2, e3, _) -> check e1 TArray tenv; check e2 TInt tenv; check e3 TUndef tenv
  and check_seq s tenv =
    List.iter (fun i -> check_instr i tenv) s
  in

  let rec check_functions f tenv =
    let env = add_env f.locals (add_env (List.fold_left (fun acc s -> (s, Some TNull, 0) :: acc) [] f.params) tenv) in
    check_seq f.code env
  in

  List.iter (fun c -> check_functions c tenv) p.functions;
  check_functions p.main tenv
