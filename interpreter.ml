open Imp

(* Pile environnements locaux aux fonctions *)
let local_env_stack = ref []

(* Variable temporaire résultat de fonction *)
let tmp = ref []

(* Environnement local *)
let env = ref Env.empty

(* Environnement global *)
let global_env = ref Env.empty

let undo_stack = ref []

(*
   Appels de fonction
  
   On s'inspire des continuations. À chaque instruction on appelle find_call qui renvoie None s'il n'y a 
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
  let f_main = List.find (fun e -> e.name = "main") p.functions in
  let code = List.fold_right (fun (s, v, id) acc -> 
    match v with
    | None -> Set(s, Null, id) :: acc
    | Some e ->
      Set(s, e, id) :: acc ) p.globals [(Expr(Call("main", []), f_main.id))]
  in

  let p_seq = ref code in
  
  let rec exec_instr (i : instr) env =
    match i with
    | Print (e, id)        ->
      Console.instr_id := id;
      (match find_call e env with
      | None -> (match eval e env with VInt i  -> Printf.printf "%d%!" i
                                    | VBool b -> Printf.printf "%b%!" b
                                    | _       -> Printf.printf "Null%!"); ([], env)
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Print(e', id)] env )

    | Set (s, e, id)     ->
      Console.instr_id := id;
      (match find_call e env with
      | None -> if Env.mem s env then let env' = Env.add s (eval e env) env in ([], env') 
              else ( global_env := Env.add s (eval e env) !global_env; ([], env))
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Set(s, e', id)] env )

    | If (e, s1, s2, id) ->
      Console.instr_id := id;
      (match find_call e env with
        | None -> if evalb e env then (s1, env) else (s2, env)
        | Some (f, p, e') -> 
          local_env_stack := (env :: !local_env_stack);
          call_fun f p [If(e', s1, s2, id)] env )

    | While (e, s, id)   ->
      Console.instr_id := id;
      (match find_call e env with
        | None -> if evalb e env then (s @ [While(e, s, id)], env) else ([], env)
        | Some (f, p, e') -> 
          local_env_stack := (env :: !local_env_stack);
          call_fun f p [While(e', s, id)] env )
    
    | Return (e, id)       ->
      Console.instr_id := id;
      (match find_call e env with
      | None ->
          let ev = eval e env in
          tmp := ev :: !tmp;
          let env = List.hd !local_env_stack in
          local_env_stack := List.tl !local_env_stack;
          ([], env) 
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Return (e', id)] env)

    | Expr (e, id)         -> 
      Console.instr_id := id; 
      (match find_call e env with
      | None ->
        let ev = eval e env in
        tmp := ev :: !tmp;
        let env = List.hd !local_env_stack in
        local_env_stack := List.tl !local_env_stack;
        ([], env)
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [] env
      )


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

  and call_fun f param next_instr env =
    let call_env = List.fold_left2 (fun acc a b -> Env.add a (eval b env) acc) Env.empty f.params param in
    let fun_code = List.fold_right (fun (s, v, id) acc ->
      match v with
      | None -> Set(s, Null, id) :: acc
      | Some e -> Set(s, e, id) :: acc ) f.locals f.code
    in
    (fun_code @ next_instr, call_env)


  and find_call e env =
    match e with
    | Call (s, l)         -> let f = List.find (fun f -> f.name = s) p.functions in
                             let rec call_in_param p find =
                              (match p with
                              | [] -> ([], find)
                              | e :: ll ->
                                if find <> None then let p', r = call_in_param ll find in (e :: p', r)
                                else
                                  match find_call e env with
                                  | None -> let p', r = call_in_param ll find in (e :: p', r)
                                  | Some (f, p, c) ->
                                    let p', r = call_in_param ll (Some (f, p, c)) in
                                    (Continuation :: p', r)
                              ) in
                            (match call_in_param l None with
                            | p', None -> Some (f, l, Continuation)
                            | p', Some (f, p, c) -> Some (f, p, Call(s, p')) )


    | Unop (op, e)       -> (match find_call e env with None -> None | Some (f, p, c) -> Some (f, p, Unop (op, c)) )
    | Binop (op, e1, e2) -> (match find_call e1 env, find_call e2 env with
                              | None, None -> None
                              | Some (f, p, c), _ -> Some (f, p, Binop (op, c, e2))
                              | None, Some (f, p, c) -> Some (f, p, Binop (op, e1, c)))
    | Array el            -> 
        (* On parcourt la liste et s'il y a un appel de fonction on renvoie Some(f, c)
           et la liste modifiée pour avoir l'appel de fonction remplacé par Continuation *)
        let rec find_array_fun l r =
          (match l with
          | [] -> ([], r)
          | e :: ll -> (match find_call e env with 
                        | None -> let l', r' = find_array_fun ll r in (e::l', r') 
                        | Some (f, p, c) -> if r = None then let l', r' = find_array_fun ll (Some (f, p, c)) in (c::l', r') 
                                        else let l', r' = find_array_fun ll r in (e::l', r')) )
        in let l', r = find_array_fun el None
        in (match r with
            | None -> None
            | Some(f, p, c) -> Some(f, p, Array l')) 
    
    | GetArr (e1, e2)     -> 
      (match find_call e1 env, find_call e2 env with
      | None, None -> None
      | Some (f, p, c), _ -> Some (f, p, GetArr (c, e2))
      | None, Some (f, p, c) -> Some (f, p, GetArr (e1, c))
      )

    | _     -> None

  and eval (e : expr) env =
    match e with
    | Int i               -> VInt i
    | Bool b              -> VBool b
    | Var string          -> (try Env.find string env 
                              with Not_found -> 
                                Console.write_out (Printf.sprintf "Variable %s not found" string); 
                                Console.close_console (); 
                                exit 0)
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
                            
    | Array el            -> let r = Array.make (List.length el) (VInt 0) in
                            List.iteri (fun i e -> r.(i) <- VInt (evali e env)) el;
                            VArray( r )
    | GetArr (e1, e2)     -> let a = evala e1 env in
                            let i = evali e2 env in
                            a.(i)
    | Continuation        -> let h = List.hd !tmp in 
                              tmp := List.tl !tmp; 
                              h
    | _                   -> VNull

  
  in
  
  Console.init_console ();


  (* fonction avance d'un pas *)
  let step seq env =
    match seq with
    | [] -> ([], env)
    | instr :: l' -> let instr', env' = (exec_instr instr env) 
                  in 
                  ignore (Console.clear_console ());
                  Console.print_env env' !global_env;
                  Console.print_code p;
                  ((instr' @ l'), env')
  in


  (* fonction retour arrière *)
  let step_back prev seq env =
    let instr, env', stack, ret = prev in
    ignore (Console.clear_console ());
    Console.print_env env' !global_env;
    Console.print_code p;
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

  ()