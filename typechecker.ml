open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  let cls = List.fold_left (fun env o -> Env.add o.class_name o env) Env.empty p.classes in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int i  -> TInt
    | Bool _ -> TBool
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
    
    | 
                            
  in

  let rec check_instr i tenv = match i with
    | Print e -> () (* Accepte tous les types *)
    | If(e, i1, i2) -> check e TBool tenv
    | While(e, s)   -> check e TBool tenv
    | Expr(e)       -> ()
    | Return(e)     -> ()
    | Set(m, e)     -> ()  
  and check_seq s tenv =
    List.iter (fun i -> check_instr i tenv) s
  in

  let rec check_functions f tenv =
    let env = add_env f.locals (add_env f.params tenv) in
    check_seq f.code env
  in

  List.iter (fun c -> check_functions c tenv) p.functions;
