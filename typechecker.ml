open Imp
open Type_infer

exception Error of string
let error s = raise (Error s)

let constraints_to_int c =
  List.fold_left (fun acc (c1, c2) -> Printf.sprintf "%s = %s\n%!" (typ_to_string c1) (typ_to_string c2) ^ acc) "" c




let get_var_name =
  let count = ref 0 in
  fun () ->
    count := !count +1;
    ("X" ^ (string_of_int !count))

let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  
  List.fold_left (fun (env, code) (s, e, id, _) ->
    match e with
    | None -> (Env.add s (get_var_name ()) env, code)
    | Some e -> (Env.add s (get_var_name ()) env, Set(s, e, id, {pos_fname=""; pos_lnum=0; pos_bol=0; pos_cnum=0}) :: code)
    ) (tenv, []) l

let typecheck_prog p =
  let tenv, global_set_variables = add_env p.globals Env.empty in
  let func =
    List.fold_left (fun env f -> Env.add f.name (List.fold_left (fun acc e -> get_var_name () :: acc) [] f.params, get_var_name ()) env) Env.empty p.functions in


  let rec type_expr e tenv = match e with
    | Null   -> ([], TNull)
    | Int i  -> ([], TInt)
    | Bool _ -> ([], TBool)
    | Var s  -> (try let n = Env.find s tenv in ([], TVar n) with Not_found -> failwith ("Variable " ^ s ^ " not found") )
    (* Binop INT *)
    | Binop(Add, e1, e2) | Binop(Sub, e1, e2) | Binop(Mul, e1, e2)
    | Binop(Div, e1, e2) | Binop(Rem, e1, e2) -> 
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let n = get_var_name () in
      ([(TVar n, TInt); (t1, TInt); (t2, TInt)] @ c1 @ c2, TVar n)
    (* Binop BOOL *)
    | Binop(Lt , e1, e2) | Binop(Le , e1, e2)
    | Binop(Gt , e1, e2) | Binop(Ge , e1, e2) ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let n = get_var_name () in
      ([(t1, TInt); (t2, TInt); (TVar n, TBool)] @ c1 @ c2, TVar n)
    | Binop(Neq, e1, e2) | Binop(Eq, e1, e2)  ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let n = get_var_name () in
      ([(TVar n, TBool); (t1, t2)] @ c1 @ c2, TVar n)
    | Binop(And, e1, e2) | Binop(Or, e1, e2)  ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let n = get_var_name () in
      ([(TVar n, TBool); (t1, TBool); (t2, TBool)] @ c1 @ c2, TVar n)
    (* Unop *)
    | Unop(Opp, e)       -> 
      let c, t = type_expr e tenv in
      let n = get_var_name () in 
      ([(TVar n, TInt); (t, TInt)] @ c, TVar n)
    | Unop(Not, e)       ->
      let c, t = type_expr e tenv in
      let n = get_var_name () in
      ([(TVar n, TBool); (t, TBool)] @ c, TVar n)
    
    | Call (s, l)    ->
      if not (Env.mem s func) then error ("Undefined function " ^ s)
      else 
        let params, ret = Env.find s func in
        (try
      (List.fold_left2 (
        fun acc a b ->
          let c, t = type_expr b tenv in
          (TVar a, t) :: List.rev_append c acc
        ) [] params l, TVar ret)
        with Invalid_argument _ -> error ("Invalid argument for function " ^ s) )
    | Array l        ->
      let c0, t0 = type_expr (List.hd l) tenv in
      let cond = List.fold_left (fun acc e -> let c, t = type_expr e tenv in (t, t0) :: (c @ acc)) [] (List.tl l) in
      let n = get_var_name () in 
      (cond @ c0, TArray (TVar n))

    | GetArr (a, i)  ->
      let c1, t1 = type_expr i tenv in
      let c2, t2 = type_expr a tenv in
      let n = get_var_name () in
      ([(t1, TInt); (t2, TArray (TVar n))] @ c1 @ c2, TVar n)

    | Continuation   -> ([], TNull) (* Never matched *)
                            
  in

  let rec check_instr i tenv var = match i with
    | Print (e, _, _) ->
      let c, t = type_expr e tenv in
      (c) (* Accepte tous les types *)
    | If(e, i1, i2, _, _) ->
      let c1, t = type_expr e tenv in
      let c2 = check_seq i1 tenv var in
      let c3 = check_seq i2 tenv var in
      ([(t, TBool)] @ c1 @ c2 @ c3)
    | While(e, s, _, _)   ->
      let c = check_seq s tenv var in
      let c1, t1 = type_expr e tenv in
      ([(t1, TBool)] @ c1 @ c)
    | Expr(e, _, _)       ->
      let c, t = type_expr e tenv in
      (c)
    | Return(e, _, _)     ->
      let c, t = type_expr e tenv in 
      ([(TVar var, t)] @ c)
    | Set(m, e, _, _)     ->
      let c, t = type_expr e tenv in
      let var_name = try Env.find m tenv with Not_found -> (failwith "Variable " ^ m ^ " not found") in
      ([(TVar var_name, t)] @ c)
    | SetArr (e1, e2, e3, _, _) ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let c3, t3 = type_expr e3 tenv in
      ([(t2, TInt)] @ c1 @ c2 @ c3)
  and check_seq s tenv var =
    (List.fold_left (
      fun acc i ->
        let c = check_instr i tenv var in c @ acc
      ) [] s)
  in

  let rec check_function f tenv =
    let var = get_var_name () in
    let var_params = Env.find f.name func in
    let env = List.fold_left2 (fun acc a b -> Env.add a b acc) tenv (List.rev f.params) (fst var_params) in
    let env, code' = add_env f.locals env in
    let s = check_seq (code' @ f.code) env var in
    s
  in
  let constraints = List.fold_left (
    fun acc f -> 
      let c = check_function f tenv in
      c@acc
    ) [] p.functions in
  let main_constr = check_function p.main tenv in
  let constr = constraints @ main_constr in
  ignore(unify constr);
  Standard_out.write_out (constraints_to_int constr)

  (*print_constraints (unify constr)*)
  
