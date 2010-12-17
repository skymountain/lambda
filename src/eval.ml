open Misc
open OptionMonad
open Syntax
open Value

exception Eval_error of string
  
let err s = raise (Eval_error (Printf.sprintf "Runtime error: %s" s))

(* evaluation for constant *)
let eval_const = function
    CInt i      -> IntV i
  | CBool b     -> BoolV b
  | CNullList _ -> ListV []
    
(* evaluation for binary operator *)
let eval_binop = function
  (* airthmetic expression *)
    Plus, IntV vl, IntV vr             -> IntV (vl + vr)
  | Minus, IntV vl, IntV vr            -> IntV (vl - vr)
  | Mult, IntV vl, IntV vr             -> IntV (vl * vr)
  | Div, IntV vl, IntV vr when vr <> 0 -> IntV (vl / vr)
  | Div, IntV _, IntV _                -> err "division by zero isn't allowed"
  | Lt, IntV vl, IntV vr               -> BoolV (vl < vr)
  | (Plus as op, _, _) | (Minus as op, _, _) | (Mult as op, _, _) | (Div as op, _, _) | (Lt as op, _, _) ->
      err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  (* cons *)
  | Cons, v, ListV vs      -> ListV (v::vs)
  | Cons, _, _             -> err "right-side of %s must be list type" @< str_of_binop Cons
      
(* evaluation for exp *)
let rec eval_exp env = function
    Var var -> begin
      begin match Env.lookup env var with
        None -> err @< Printf.sprintf "%s is not bound" var
      | Some v -> v
      end
    end
  | Const c -> eval_const c
  | BinOp (op, el, er) -> eval_binop (op, eval_exp env el, eval_exp env er)
  | IfExp (cond, then_exp, else_exp) -> begin
      match eval_exp env cond with
        BoolV b -> eval_exp env @< if b then then_exp else else_exp
      | _       -> err "value of conditional expression must be bool"
    end
  | Fun (var, _, body) -> FunV (var, body, ref env)
  | App (f, act) -> begin
      begin match eval_exp env f, eval_exp env act with
        FunV (formal, body, env'), act ->
          eval_exp (Env.extend !env' formal act) body
      | ConstrV (cname, vs), v ->
          ConstrV (cname, vs@[v])
      | _ -> err "not-function value is applied"
      end
    end
  | Let (var, exp, body) -> begin
      let v = eval_exp env exp in
      eval_exp (Env.extend env var v) body
    end
  | LetRec (var, _, exp, body) -> begin
      let v = eval_rec env var exp in
      eval_exp (Env.extend env var v) body
    end
  | ListLit exps -> begin
      let vs = List.map (eval_exp env) exps in
      ListV vs
    end
  | TypedExpr (exp, _) -> eval_exp env exp
  | MatchExp (exp, branches) -> begin
      let v = eval_exp env exp in
      let branch =
        List.fold_left (fun acc (pat, body) ->
                          match acc with
                            Some _ -> acc
                          | None -> 
                              Patmatch.ematch v pat >>= fun pat_env -> Some (Env.extend_by_env env pat_env, body))
          None branches
      in
      match branch with
        Some (env, exp) -> eval_exp env exp
      | None            -> err "match failure"
    end
  | Construct (cname, _) ->
      ConstrV (cname, [])

(* evaluation for recursive def *)
and eval_rec env var exp =
  match exp with
    Fun (para, _, fbody) -> begin
      let dummy = ref Env.empty in
      let v = FunV (para, fbody, dummy) in
      let env' = Env.extend env var v in
      dummy := env';
      v
    end
  | _ -> err "non-function values cannot be defined recursively"
      
(* evaluation for program *)
let eval env =
  let return env var v =
    Env.extend env var v, var, v
  in
  function
    Exp exp -> return env "it" @< eval_exp env exp
  | Decl (var, exp) -> return env var @< eval_exp env exp
  | DeclRec (var, _, exp) -> return env var @< eval_rec env var exp
