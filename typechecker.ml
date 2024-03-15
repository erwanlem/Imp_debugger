open Imp
open Type_infer

exception Error of string
let error s = raise (Error s)

let print_constraints c =
  Printf.printf "[";
  List.iter (fun (c1, c2) -> Printf.printf "%s = %s\n%!" (typ_to_string c1) (typ_to_string c2)) c;
  Printf.printf "]"


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
  List.fold_left (fun env (s, t, _) -> match t with Some t -> Env.add s (get_var_name ()) env
                                                    | None -> Env.add s (get_var_name ()) env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in
  (*let func =
    List.fold_left (fun env f -> Env.add f.name f env) Env.empty p.functions in*)


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
      ([(t1, t2); (TVar n, TInt)] @ c1 @ c2, TVar n)
    (* Binop BOOL *)
    | Binop(Lt , e1, e2) | Binop(Le , e1, e2)
    | Binop(Gt , e1, e2) | Binop(Ge , e1, e2) ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let n = get_var_name () in
      ([(t1, t2); (t1, TInt); (t2, TInt); (TVar n, TBool)] @ c1 @ c2, TVar n)
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
      let n = get_var_name () in 
      ([], TVar n) (* TODO *)
      (*if not (Env.mem s func) then error ("Undefined function " ^ s)
      else let f = Env.find s func in
      if List.length f.params = List.length l then TNull else error "Invalid arguments"*)

    | Array l        ->
      let n = get_var_name () in 
      ([], TArray (TVar n)) (* TODO *)
    | GetArr (a, i)  ->
      let c1, t1 = type_expr i tenv in
      let n = get_var_name () in
      ([(t1, TInt)] @ c1, TVar n) (* TODO *)
    | Continuation   -> ([], TNull) (* Never used *)
                            
  in

  let rec check_instr i tenv = match i with
    | Print (e, _) ->
      let c, t = check_instr i tenv in
      let n = get_var_name () in
      (c, TVar n) (* Accepte tous les types *)
    | If(e, i1, i2, _) ->
      let c1, t = type_expr e tenv in
      let c2, t = check_seq i1 tenv in
      let c3, t = check_seq i2 tenv in
      let n = get_var_name () in
      ([(t, TBool)] @ c1 @ c2 @ c3, TVar n)
    | While(e, s, _)   ->
      let c, t = check_seq s tenv in
      let n = get_var_name () in
      (c, TVar n)
    | Expr(e, _)       ->
      let c, t = type_expr e tenv in
      let n = get_var_name () in
      (c, TVar n)
    | Return(e, _)     ->
      let c, t = type_expr e tenv in 
      let n = get_var_name () in
      (c, TVar n)
    | Set(m, e, _)     ->
      let c, t = type_expr e tenv in
      let var_name = try Env.find m tenv with Not_found -> (failwith "Variable " ^ m ^ " not found") in
      let n = get_var_name () in
      ([(TVar var_name, t)] @ c, TVar n) (* TODO *)
    | SetArr (e1, e2, e3, _) ->
      let c1, t1 = type_expr e1 tenv in
      let c2, t2 = type_expr e2 tenv in
      let c3, t3 = type_expr e3 tenv in
      let n = get_var_name () in
      ([(t2, TInt)] @ c1 @ c2 @ c3, TVar n)
  and check_seq s tenv =
    let n = get_var_name () in
    (List.fold_left (
      fun acc i ->
        let c, t = check_instr i tenv in
        c @ acc
      ) [] s, TVar n)
  in

  let rec check_functions f tenv =
    let env = add_env f.locals (add_env (List.fold_left (fun acc s -> (s, Some TNull, 0) :: acc) [] f.params) tenv) in
    check_seq f.code env
  in

  let constraints = List.fold_left (fun acc f -> let c, t = check_functions f tenv in c @ acc ) [] p.functions in
  let main_constr, _ = check_functions p.main tenv in
  let constr = constraints @ main_constr in
  print_constraints constr;

  Printf.printf "\n\n";

  print_constraints (unify constr)
  
