open Imp

(* Pile environnements locaux aux fonctions *)
let local_env_stack = ref []

(* Variable temporaire résultat de fonction *)
let tmp = ref Null

(* Environnement local *)
let env = ref Env.empty

let undo_stack = ref []

(*
   Appels de fonction
  
   On s'inspire des continuations. À chaque instruction on appelle evalf qui renvoie None s'il n'y a 
   pas d'appel de fonction dans l'expression. Sinon la fonction rend Some(f, e) avec f la fonction à 
   appeler et e l'expression à évaluer ensuite.
   
*)



(* Fonction principale *)
let exec_prog (p : program): unit =
  
  let rec exec_instr (i : instr) env =
    match i with
    | Print e        ->
      (match evalf e env with
      | None -> (match eval e env with VInt i  -> Printf.printf "%d%!" i
                                    | VBool b -> Printf.printf "%b%!" b
                                    | _       -> Printf.printf "Null%!"); ([], env)
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Print(e')], Env.empty))
    | Set (s, e)     ->
      (match evalf e env with
      | None -> let env' = Env.add s (eval e env) env in ([], env')
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Set(s, e')], Env.empty) )

    | If (e, s1, s2) ->
      (match evalf e env with
        | None -> if evalb e env then (s1, env) else (s2, env)
        | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                          (f.code @ [If(e', s1, s2)], Env.empty) )

    | While (e, s)   ->
      (match evalf e env with
        | None -> if evalb e env then (s @ [While(e, s)], env) else ([], env)
        | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                          (f.code @ [While(e', s)], Env.empty) )
    
    | Return e       ->
      (match evalf e env with
      | None -> 
          tmp := eval e env;
          let env = List.hd !local_env_stack in
          local_env_stack := List.tl !local_env_stack;
          ([], env) 
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Return e'], Env.empty) )

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
  and evalf e env =
    match e with
    | Call (s, l)         -> let f = List.find (fun f -> f.name = s) p.functions in
                            Some (f, Continuation)
    | Unop (op, e)       -> (match evalf e env with None -> None | Some (f, c) -> Some (f, Unop (op, c)) )
    | Binop (op, e1, e2) -> (match evalf e1 env, evalf e2 env with
                              | None, None -> None
                              | Some (f, c), _ -> Some (f, Binop (op, c, e2))
                              | None, Some (f, c) -> Some (f, Binop (op, e1, c)))
    | Array el            -> 
        (* On parcourt la liste et s'il y a un appel de fonction on renvoie Some(f, c)
           et la liste modifiée pour avoir l'appel de fonction remplacé par Continuation *)
        let rec find_array_fun l r =
          (match l with
          | [] -> ([], r)
          | e :: ll -> (match evalf e env with 
                        | None -> let l', r' = find_array_fun ll r in (e::l', r') 
                        | Some (f, c) -> if r = None then let l', r' = find_array_fun ll (Some (f, c)) in (c::l', r') 
                                        else let l', r' = find_array_fun ll r in (e::l', r')) )
        in let l', r = find_array_fun el None
        in (match r with
            | None -> None
            | Some(f, c) -> Some(f, Array l')) 
    
    | GetArr (e1, e2)     -> 
      (match evalf e1 env, evalf e2 env with
      | None, None -> None
      | Some (f, c), _ -> Some (f, GetArr (c, e2))
      | None, Some (f, c) -> Some (f, GetArr (e1, c))
      )

    | _     -> None

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
    | Call (s, l)         -> Null (* Cas non utilisé *)
                            
    | Array el            -> let r = Array.make (List.length el) (VInt 0) in
                            List.iteri (fun i e -> r.(i) <- VInt (evali e env)) el;
                            VArray( r )
    | GetArr (e1, e2)     -> let a = evala e1 env in
                            let i = evali e2 env in
                            a.(i)
    | Continuation        -> !tmp
  
  in
  
  let window = Console.init_console () in


  (* fonction avance d'un pas *)
  let step seq env =
    match seq with
    | [] -> ([], env)
    | instr :: l' -> let instr', env' = (exec_instr instr env) 
                  in 
                  (*ignore (Console.clear_console window); *)
                  Console.print_env window env'; 
                  ((instr' @ l'), env')
  in

  (* fonction retour arrière *)
  let step_back prev seq env =
    let instr, env' = prev in
    (*ignore (Console.clear_console window);*)
    Console.print_env window env';
    (instr::seq, env')
  
  in



  let p = ref (p.main.code) in

  let match_entry entry =
    fun () ->
      if (!p) = [] then (Console.close_console (); false) else 
      match entry with
      | "exit"      -> Console.close_console (); false


      | "next"      ->  undo_stack := ((List.hd (!p)), !env) :: !undo_stack;
                        let p', env' = step (!p) (!env) in
                        p := p';
                        env := env';
                        true

                        
      | "undo"      -> if List.length (!undo_stack) > 0 then
                       let p', env' = step_back (List.hd (!undo_stack)) (!p) (!env) in
                        Printf.printf "%d\n%!" (List.length !undo_stack);
                        p := p';
                        env := env';
                        true
                      else true


      | c           -> true

  in
  Console.match_key window match_entry;

  (*exec_steps p.code env*)
  ()