open Misc

exception Exit
  
let err s = print_endline s 
let p_msg s = print_endline s
  
let rec read_eval_print prompt fun_lexbuf tenv env err =
  print_string prompt;
  flush stdout;
  try 
    let prog = Parser.main Lexer.main @< Lexing.from_function fun_lexbuf in
    match prog with
      Syntax.EOF -> (tenv, env)
    | _   -> begin
        let (newtenv, id_typ, typ) = Typing.typing tenv prog in
        let (newenv, id ,v) = Eval.eval env prog in
        assert (id_typ = id);
        print_string @< "val "^id;
        print_string " : ";
        Typing.pp_typ typ;
        print_string " = ";
        Eval.pp_val v;
        print_newline ();
        read_eval_print prompt fun_lexbuf newtenv newenv err
      end
  with
    e -> let f s = err s; read_eval_print prompt fun_lexbuf tenv env err in
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

let init_env = Env.extend (Env.extend Env.empty "i" @< Eval.IntV 1) "ii" @< Eval.IntV 2
let init_tenv = Env.empty
  
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
    (fun (tenv, env) (ichann, err, file, prompt) ->
       (* print file name *)
       (match file with
          None -> ()
        | Some file -> p_msg @< Printf.sprintf "will load %s" file);
       read_eval_print prompt (refill_buffer ichann) tenv env err)
    (init_tenv, init_env)
    ins

let _ = try ignore @< main () with Exit -> ();