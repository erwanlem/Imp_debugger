open Imp


let local_env_stack = ref []
let tmp = ref Null

let env = ref Env.empty
let undo_stack = ref []


(* Fonction principale *)
let exec_prog (p : program): unit =
  
  let rec exec_instr (i : instr) env =
    match i with
    | Print e        ->
      (match eval e env with
      | VInt i  -> Printf.printf "%d%!" i
      | VBool b -> Printf.printf "%b%!" b
      | _       -> Printf.printf "Null%!"); ([], env)
    | Set (s, e)     -> let env' = Env.add s (eval e env) env in ([], env')

    | If (e, s1, s2) ->
      (match eval e env with
      | VBool b -> if b then (s1, env) else (s2, env)
      | _ -> ([], env))
    | While (e, s)   ->
      (match eval e env with
      | VBool b ->  if b then (s @ [While(e, s)], env) else ([], env)
      | _ -> assert false)
    
    | Return e       -> tmp := eval e env;
                        let env = List.hd !local_env_stack in
                        local_env_stack := List.tl !local_env_stack;
                        ([], env)

    | Expr e         -> ignore (eval e env); ([], env)

    | SetArr (e1, e2, e3) -> let a = Array.copy (evala e1 env) in
                            let i = evali e2 env in
                            a.(i) <- eval e3 env;
                            match e1 with
                            | Var n -> let env' = Env.add n (VArray(a)) env in ([], env')
                            | _ -> ([], env)

  and evali e env = match eval e env with
    | VInt n -> n
    | _ -> assert false
  and evalb e env = match eval e env with
    | VBool b -> b
    | _ -> assert false
  and evala e env = match eval e env with
    | VArray a -> a
    | _ -> assert false
  and evalf v = match v with
    | Fun e -> 
    | v     -> v
  and eval (e : expr) env =
    match e with
    | Int i               -> VInt i
    | Bool b              -> VBool b
    | Var string          -> Env.find string env
    | Unop (Not, e)       -> VBool (not (evalb e env))
    | Unop (Opp, e)       -> VInt (- (evali e env))
    | Binop (Add, e1, e2) -> VInt(evali e1 env + evali e2 env)
    | Binop (Sub, e1, e2) -> VInt(evali e1 env - evali e2 env)
    | Binop (Mul, e1, e2) -> VInt(evali e1 env * evali e2 env)
    | Binop (Div, e1, e2) -> VInt(evali e1 env / evali e2 env)
    | Binop (Rem, e1, e2) -> VInt(evali e1 env mod evali e2 env)
    | Binop (Lt, e1, e2)  -> VBool(evali e1 env < evali e2 env)
    | Binop (Le, e1, e2)  -> VBool(evali e1 env <= evali e2 env)
    | Binop (Gt, e1, e2)  -> VBool(evali e1 env > evali e2 env)
    | Binop (Ge, e1, e2)  -> VBool(evali e1 env >= evali e2 env)
    | Binop (Eq, e1, e2)  -> VBool(evali e1 env = evali e2 env)
    | Binop (Neq, e1, e2) -> VBool(evali e1 env <> evali e2 env)
    | Binop (And, e1, e2) -> VBool(evalb e1 env && evalb e2 env)
    | Binop (Or, e1, e2)  -> VBool(evalb e1 env || evalb e2 env)
    | Call (s, l)         -> let f = List.find (fun f -> f.name = s) p.functions in
                            let instr, env' = eval_call f l env in
                            Null
    | Array el            -> let r = Array.make (List.length el) (VInt 0) in
                            List.iteri (fun i e -> r.(i) <- VInt (evali e env)) el;
                            VArray( r )
    | GetArr (e1, e2)     -> let a = evala e1 env in
                            let i = evali e2 env in
                            a.(i)
  and eval_call f args env =
    local_env_stack := (env :: !local_env_stack);
    (f.code, Env.empty)
  
  in
  
  let window = Console.init_console () in


  (* fonction avance d'un pas *)
  let step seq env =
    match seq with
    | [] -> ([], env)
    | instr :: l' -> let instr', env' = (exec_instr instr env) 
                  in 
                  ignore (Console.clear_console window); 
                  Console.print_env window env'; 
                  ((instr' @ l'), env')
  in

  (* fonction retour arrière *)
  let step_back prev seq env =
    let instr, env' = prev in
    ignore (Console.clear_console window);
    Console.print_env window env';
    (instr::seq, env')
  
  in



  let p = ref (p.main.code) in

  let match_entry entry =
    fun () ->
      if (!p) = [] then (Console.close_console (); false) else 
      match entry with
      | "exit"      -> Console.close_console (); false


      | "next"      -> undo_stack := ((List.hd (!p)), !env) :: !undo_stack;
                        let p', env' = step (!p) (!env) in
                        p := p';
                        env := env';
                        true

                        
      | "undo"      -> if List.length (!undo_stack) > 0 then
                       let p', env' = step_back (List.hd (!undo_stack)) (!p) (!env) in
                        undo_stack := List.tl (!undo_stack);
                        p := p';
                        env := env';
                        true
                      else true


      | c           -> true

  in
  Console.match_key window match_entry;

  (*exec_steps p.code env*)
  ()