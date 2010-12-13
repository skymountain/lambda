open Misc
open Syntax
open Value
open Type

exception Exit
  
let err s = print_endline s 
let p_msg s = print_endline s
  
let rec read_eval_print prompt fun_lexbuf tctx env err =
  print_string prompt;
  flush stdout;
  try
    let prog = Parser.main Lexer.main @< Lexing.from_function fun_lexbuf in
    match prog with
      EOF -> (tctx, env)
    | TypDef _ -> assert false
    | Eval e -> begin
        let (tctx, id_typ, typ) = Typing.typing tctx e in
        let (newenv, id ,v) = Eval.eval env e in
        assert (id_typ = id);
        Printf.printf "val %s : %s = %s" id (Type.pps_typ typ) (pps_val v);
        print_newline ();
        read_eval_print prompt fun_lexbuf tctx newenv err
      end
  with
    e -> let f s = err s; read_eval_print prompt fun_lexbuf tctx env err in
    (match e with
       Parsing.Parse_error   -> f "Syntax error" 
     | Lexer.Lexical_error s -> f s
     | Eval.Eval_error s     -> f s
     | Typing.Typing_error s -> f s
     | _ -> raise e)

let refill_buffer ch =
  let rec fill_buff dst idx len =
    if len = idx then idx
    else begin
      try 
        let c = input_char ch in
        dst.[idx] <- c;
        if c = '\n' then idx+1
        else fill_buff dst (idx+1) len
      with
        End_of_file -> idx
    end
  in
  let body buf len = fill_buff buf 0 len in
  body

let init_env binds =
  List.fold_right (fun (var, v, typ) (acc_env, acc_tenv) ->
                     (Env.extend acc_env var v, Env.extend acc_tenv var typ))
    binds (Env.empty, Env.empty)

let (env, tenv) =
  init_env [
    ("i", IntV 1, TyInt); ("ii", IntV 2, TyInt);
    
    ("+", FunV ("x", Fun ("y", IntT, BinOp (Plus, Var "x", Var "y")), ref Env.empty),
     TyFun (TyInt, TyFun(TyInt, TyInt)));
    
    ("-", FunV ("x", Fun ("y", IntT, BinOp (Minus, Var "x", Var "y")), ref Env.empty),
     TyFun (TyInt, TyFun(TyInt, TyInt)));
    
    ("*", FunV ("x", Fun ("y", IntT, BinOp (Mult, Var "x", Var "y")), ref Env.empty),
     TyFun (TyInt, TyFun(TyInt, TyInt)));
    
    ("/", FunV ("x", Fun ("y", IntT, BinOp (Div, Var "x", Var "y")), ref Env.empty),
     TyFun (TyInt, TyFun(TyInt, TyInt)));
    
    ("<", FunV ("x", Fun ("y", IntT, BinOp (Lt, Var "x", Var "y")), ref Env.empty),
     TyFun (TyInt, TyFun(TyInt, TyBool)));
  ]

let tctx = { typ_env = tenv; typvar_map = TypvarMap.empty }

let main () =
  let files = ref [] in
  let interact = ref None in
  let _ =
    Arg.parse ["-i", Arg.Unit (fun _ -> interact := Some true), "interact with interpretor"]
      (fun f -> files := f::!files; if !interact = None then interact := Some false else ())
      "Usage: lambda [file...]"
  in
  let interact = match !interact with None -> true | Some b -> b in
  let ferr s = err s; raise Exit in
  let files =
    List.fold_left
      (fun xs file ->
         ((try open_in file with Sys_error _ -> err @< Printf.sprintf "%s: No such file." file; raise Exit), ferr, Some file, "")::xs)
      [] !files
  in
  let ins = List.append files @< if interact then [(stdin, (fun s -> err s), None, "> ")] else [] in
  List.fold_left
    (fun (tctx, env) (ichann, err, file, prompt) ->
       (* print file name *)
       (match file with
          None -> ()
        | Some file -> p_msg @< Printf.sprintf "will load %s" file);
       read_eval_print prompt (refill_buffer ichann) tctx env err)
    (tctx, env)
    ins

let _ = try ignore @< main () with Exit -> ();
