open Imp

(* Pile environnements locaux aux fonctions *)
let local_env_stack = ref []

(* Variable temporaire résultat de fonction *)
let tmp = ref []

(* Environnement local *)
let env = ref Env.empty

let undo_stack = ref []

(*
   Appels de fonction
  
   On s'inspire des continuations. À chaque instruction on appelle evalf qui renvoie None s'il n'y a 
   pas d'appel de fonction dans l'expression. Sinon la fonction rend Some(f, e) avec f la fonction à 
   appeler et e l'expression à évaluer ensuite.
   

   Pour chaque instruction:
   - Si l'instruction contient un appel de fonction
        - ajouter le code de la fonction
        - ajouter l'instruction avec l'appel de fonction qui attend le résultat
   - Si pas d'appel de fonction alors évaluer l'instruction
*)



(* Fonction principale *)
let exec_prog (p : program): unit =
  let p_seq = ref (p.main.code) in
  
  let rec exec_instr (i : instr) env =
    match i with
    | Print (e, id)        ->
      Console.instr_id := id;
      (match evalf e env with
      | None -> (match eval e env with VInt i  -> Printf.printf "%d%!" i
                                    | VBool b -> Printf.printf "%b%!" b
                                    | _       -> Printf.printf "Null%!"); ([], env)
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Print(e', id)], Env.empty))
    | Set (s, e, id)     ->
      Console.instr_id := id;
      (match evalf e env with
      | None -> let env' = Env.add s (eval e env) env in ([], env')
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Set(s, e', id)], Env.empty) )

    | If (e, s1, s2, id) ->
      Console.instr_id := id;
      (match evalf e env with
        | None -> if evalb e env then (s1, env) else (s2, env)
        | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                          (f.code @ [If(e', s1, s2, id)], Env.empty) )

    | While (e, s, id)   ->
      Console.instr_id := id;
      (match evalf e env with
        | None -> if evalb e env then (s @ [While(e, s, id)], env) else ([], env)
        | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                          (f.code @ [While(e', s, id)], Env.empty) )
    
    | Return (e, id)       ->
      Console.instr_id := id;
      (match evalf e env with
      | None ->
          let ev = eval e env in
          tmp := ev :: !tmp;
          let env = List.hd !local_env_stack in
          local_env_stack := List.tl !local_env_stack;
          ([], env) 
      | Some (f, e') -> local_env_stack := (env :: !local_env_stack);
                        (f.code @ [Return (e', id)], Env.empty) )

    | Expr (e, id)         -> Console.instr_id := id; ignore (eval e env); ([], env)

    | SetArr (e1, e2, e3, id) ->
        Console.instr_id := id;
        let a = Array.copy (evala e1 env) in
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
    | Continuation        -> let h = List.hd !tmp in 
                              tmp := List.tl !tmp; 
                              h
  
  in
  
  Console.init_console ();


  (* fonction avance d'un pas *)
  let step seq env =
    match seq with
    | [] -> ([], env)
    | instr :: l' -> let instr', env' = (exec_instr instr env) 
                  in 
                  ignore (Console.clear_console ());
                  Console.print_env env';
                  Console.print_code p;
                  ((instr' @ l'), env')
  in


  (* fonction retour arrière *)
  let step_back prev seq env =
    let instr, env', stack, ret = prev in
    ignore (Console.clear_console ());
    Console.print_env env';
    (instr, env', stack, ret)
  
  in


  let match_entry entry =
    fun () ->
      if (!p_seq) = [] then (Console.close_console (); false) else 
      match entry with
      | "exit"      -> Console.close_console (); false


      | "next"      ->  (* ajoute instruction, environnement et pile des env locaux sur la pile d'actions *)
                        undo_stack := (!p_seq, !env, !local_env_stack, !tmp) :: !undo_stack;
                        
                        let pr_list = List.fold_left (fun acc e -> acc ^ (string_of_value e ^ "; ")) "" !tmp in
                        Console.write_out (pr_list);

                        let p', env' = step (!p_seq) (!env) in
                        p_seq := p';
                        env := env';
                        true

                        
      | "undo"      -> if List.length (!undo_stack) > 0 then
                      (
                        (* Récupère l'état précédent *)
                       let p', env', stack, ret = step_back (List.hd (!undo_stack)) (!p_seq) (!env) in
                       undo_stack := List.tl !undo_stack;
                        p_seq := p'; env := env'; tmp := ret;
                        local_env_stack := stack;
                        Console.write_out (string_of_int (List.length !p_seq));
                        true)
                      else true


      | c           -> true

  in
  Console.match_key match_entry;

  (*exec_steps p.code env*)
  ()