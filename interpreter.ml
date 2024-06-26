open Global
open Imp

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

  let code = List.fold_right (fun (s, v, id, l) acc -> 
    match v with
    | None -> Set(s, Null, id, l) :: acc
    | Some e ->
      Set(s, e, id, l) :: acc ) p.globals [(Expr(Call("main", []), f_main.id, f_main.line))]
  in

  (* Get the first instruction *)
  if List.hd code = (Expr(Call("main", []), f_main.id, f_main.line)) then begin
    if f_main.locals = [] then Standard_out.instr_id := Utils.get_instr_id (List.hd f_main.code)
    else 
      let _, _, id, _ = List.hd f_main.locals in
      Standard_out.instr_id := id
    end
  else
    Standard_out.instr_id := Utils.get_instr_id (List.hd code);


  let p_seq = ref code in
  
  let rec exec_instr (i : instr) env =
    match i with
    | Print (e, id, line)        ->
      Standard_out.instr_id := id;
      (match find_call e env with
      | None -> (match eval e env with VInt i  -> Printf.printf "%d%!" i
                                    | VBool b -> Printf.printf "%b%!" b
                                    | _       -> Printf.printf "Null%!"); ([], env)
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Print(e', id, line)] env )

    | Set (s, e, id, line)     ->
      Standard_out.instr_id := id;
      (match find_call e env with
      | None -> if Env.mem s env then let env' = Env.add s (eval e env) env in ([], env') 
                else ( global_env := Env.add s (eval e env) !global_env; ([], env))
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Set(s, e', id, line)] env )

    | If (e, s1, s2, id, line) ->
      Standard_out.instr_id := id;
      (match find_call e env with
        | None -> if evalb e env then (s1, env) else (s2, env)
        | Some (f, p, e') -> 
          local_env_stack := (env :: !local_env_stack);
          call_fun f p [If(e', s1, s2, id, line)] env )

    | While (e, s, id, line)   ->
      Standard_out.instr_id := id;
      (match find_call e env with
        | None -> if evalb e env then (s @ [While(e, s, id, line)], env) else ([], env)
        | Some (f, p, e') -> 
          local_env_stack := (env :: !local_env_stack);
          call_fun f p [While(e', s, id, line)] env )
    
    | Return (e, id, line)       ->
      Standard_out.instr_id := id;
      (match find_call e env with
      | None ->
          let ev = eval e env in
          tmp := ev :: !tmp;
          let env = List.hd !local_env_stack in
          local_env_stack := List.tl !local_env_stack;
          ([], env) 
      | Some (f, p, e') -> 
        local_env_stack := (env :: !local_env_stack);
        call_fun f p [Return (e', id, line)] env)

    | Expr (e, id, line)         -> 
      Standard_out.instr_id := id; 
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


    | SetArr (e1, e2, e3, id, line) ->
        Standard_out.instr_id := id;
        let a = evala e1 env in
        let arr = a.array in
        let arr_id = a.id in
        let i = evali e2 env in
        arr.(i) <- eval e3 env;
        match e1 with
        | Var n -> 
          if Env.mem n env then let env' = Env.add n (VArray({array=arr; id=arr_id})) env in ([], env') 
          else ( global_env := Env.add n (VArray({array=arr; id=arr_id})) !global_env; ([], env))
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
    let call_env = List.fold_left2 (fun acc a b -> Env.add a (eval b env) acc) Env.empty (List.rev f.params) param in
    let call_env = List.fold_left (fun acc (s, v, id, l) -> Env.add s VNull acc) call_env f.locals in
    let fun_code = List.fold_right (fun (s, v, id, l) acc ->
      match v with
      | None -> Set(s, Null, id, l) :: acc
      | Some e -> Set(s, e, id, l) :: acc ) f.locals f.code
    in
    (fun_code @ next_instr, call_env)


    (* Renvoie la fonction à appliquer, les paramètres de l'appel et 
       l'instruction à appliquer à la fin de l'appel 
      
       Returns
       None if function call not found
       Some(function, args, Continue) otherwise
    *)
  and find_call e env =
    match e with
    | Call (s, l)         -> let f = List.find (fun f -> f.name = s) p.functions in
                              Standard_out.instr_id := f.id;
                              (* Récupère les appels dans les paramètres de l'appel 
                                 Returns
                                  Some(function, args, Continue) si appel
                                  None sinon
                              *)
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
                            | p', None -> Some (f, p', Continuation) (* Pas d'appel dans les paramètres *)
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
                                (try Env.find string !global_env 
                              with Not_found -> 
                                Standard_out.close_console ();
                                exit 0
                                ); 
                                )
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
                            List.iteri (fun i e -> r.(i) <- eval e env) el;
                            VArray( {array=r; id=Utils.gen_array_id () } )
    | GetArr (e1, e2)     -> let a = evala e1 env in
                            let i = evali e2 env in
                            a.array.(i)
    | Continuation        -> let h = List.hd !tmp in
                              tmp := List.tl !tmp;
                              h
    | _                   -> VNull (* Should never happened *)

  
  in
  
  Standard_out.init_console ();
  Standard_out.print_env !env !global_env;
  Standard_out.print_code p;

  
  (****************************************************)
  (*                Debugger features                 *)
  (****************************************************)

  (* Next line fuction *)
  let step seq env =
    match seq with
    | [] -> ([], env)
    | instr :: l' -> let instr', env' = (exec_instr instr env) 
                  in
                  let seq' = instr' @ l' in
                  if seq' <> [] then Standard_out.instr_id := Utils.get_instr_id (List.hd seq');
                  (seq', env')
  in

  (* Next breakpoint function *)
  let next_break seq env =
    let seq, env = step seq env in
    let rec loop seq env =
      match seq with
      | [] -> ([], env)
      | instr :: l' -> 
        if Hashtbl.mem breakpoints (Utils.get_instr_line instr) then (seq, env)
        else begin
          Array_liveness.mark_liveness ();
          let instr', env' = (exec_instr instr env) in
          loop (instr' @ l') env'
        end
    in
    let seq', env' =
    (match seq with
    | [] -> ([], env)
    | instr :: l' ->
      let seq', env' = loop seq env in (seq', env')) 
    in
      if seq' <> [] then Standard_out.instr_id := Utils.get_instr_id (List.hd seq');
      (seq', env')
  in
  
  (* Step over function *)
  let step_over seq env =

    let has_fun_call = function
      | Print(e, _, _) | Return(e, _, _) | Expr(e, _, _) | Set(_, e, _, _) -> not (find_call e env = None)
      | SetArr(e1, e2, e3, id, _) ->
        not (find_call e1 env = None && find_call e2 env = None && find_call e3 env = None)
      | _ -> false
    in

    match seq with
    | [] -> ([], env)
    | instr :: l' ->
      let step_id =
      match instr with
      | While(_, _, _, _) ->
        (match l' with
        | [] -> -2
        | instr' :: ll' -> Utils.get_instr_id instr')
      | If (_, _, _, _, _) ->
        (match l' with
        | [] -> -2
        | instr' :: ll' -> Utils.get_instr_id instr')
      | _ -> 
        if has_fun_call instr then
          (match l' with
          | [] -> -2
          | instr' :: ll' -> Utils.get_instr_id instr')
        else -1
      in
      let rec loop seq env =
        match seq with
        | [] -> ([], env)
        | instr :: l' -> 
          if Utils.get_instr_id instr = step_id then (seq, env)
          else 
            let instr', env' = (exec_instr instr env) in
            loop (instr' @ l') env'
      in
      if step_id = -1 then
        step seq env
      else begin
      let seq', env' = loop seq env in
      if seq' <> [] then
        Standard_out.instr_id := Utils.get_instr_id (List.hd seq')
      else 
        Standard_out.instr_id := -1;
      (seq', env') end
      


  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  
  in

  let match_entry entry =
      if (!p_seq) = [] then begin (Standard_out.close_console (); false) end else begin
      let ret =
     match Command_regex.get_command entry with
      | "exit"      -> Standard_out.close_console (); false

      | "next"      ->  (* ajoute instruction, environnement, pile des env locaux, retour fonction et id instr sur la pile d'actions *)
                        undo_stack := (!p_seq, !Global.env, !global_env, !local_env_stack, !tmp, !Standard_out.instr_id) :: !undo_stack;
                        Array_liveness.save_state ();
                        (*Standard_out.write_out (Format.sprintf "Undo stack size = %d" (List.length !undo_stack));*)
                        Standard_out.write_out (Printf.sprintf "len local stack = %d\nlen current local memory = %d\n%!" (List.length !Global.local_env_stack) (Env.cardinal !Global.env));
                        
                        let p', env' = step (!p_seq) (!Global.env) in
                        p_seq := p';
                        Global.env := env';
                        ignore (Standard_out.clear_console ());
                        Standard_out.print_arrays ();
                        Standard_out.print_env !Global.env !Global.global_env;
                        Standard_out.print_code p;
                        true

      | "step"      ->  undo_stack := (!p_seq, !Global.env, !global_env, !local_env_stack, !tmp, !Standard_out.instr_id) :: !undo_stack;
                        Array_liveness.save_state ();
                        let p', env' = next_break (!p_seq) (!Global.env) in
                        p_seq := p';
                        Global.env := env';
                        ignore (Standard_out.clear_console ());
                        Standard_out.print_arrays ();
                        Standard_out.print_env !Global.env !Global.global_env;
                        Standard_out.print_code p;
                        true
        
      | "so"        ->  (* ajoute instruction, environnement, pile des env locaux, retour fonction et id instr sur la pile d'actions *)
                        undo_stack := (!p_seq, !Global.env, !global_env, !local_env_stack, !tmp, !Standard_out.instr_id) :: !undo_stack;
                        Array_liveness.save_state ();
                        
                        let p', env' = step_over (!p_seq) (!Global.env) in
                        p_seq := p';
                        Global.env := env';
                        ignore (Standard_out.clear_console ());
                        Standard_out.print_arrays ();
                        Standard_out.print_env !Global.env !Global.global_env;
                        Standard_out.print_code p;
                        true
                        
      | "undo"      -> if List.length (!undo_stack) > 0 then
                      (
                        let instr, env', globals, stack, ret, id_instr = List.hd (!undo_stack) in

                        (* Récupère l'état précédent *)
                        (*let p', env', globals, stack, ret = step_back (List.hd (!undo_stack)) (!p_seq) (!env) in*)
                        undo_stack := List.tl !undo_stack;
                        p_seq := instr; Global.env := env'; tmp := ret; global_env := globals;
                        local_env_stack := stack;

                        Standard_out.instr_id := Utils.get_instr_id (List.hd instr);
                        Array_liveness.state_back ();
                        ignore (Standard_out.clear_console ());
                        Standard_out.print_arrays ();
                        Standard_out.print_env !Global.env !Global.global_env;
                        Standard_out.print_code p;
                        (*Standard_out.write_out (Format.sprintf "Undo stack size = %d" (List.length !undo_stack));*)
                        true)
                      else true

      | "break"     ->  let line = Command_regex.get_int_param entry in
                        if Hashtbl.mem breakpoints line then Hashtbl.remove breakpoints line
                        else Hashtbl.replace breakpoints line ();
                        ignore (Standard_out.clear_console ());
                        Standard_out.print_arrays ();
                        Standard_out.print_env !Global.env !Global.global_env;
                        Standard_out.print_code p;
                        true

      | c           -> true 
    in

    (* Check if there are still instructions *)
    if (!p_seq) = [] then begin 
      Standard_out.close_console ();
      false 
    end
    else ret

  end
  in
  ignore (Standard_out.match_key match_entry);

  ()