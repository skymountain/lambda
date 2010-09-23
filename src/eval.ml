open Misc
open Syntax

exception Eval_error of string
  
type value =
    IntV  of int
  | BoolV of bool
  | FunV  of id * exp * (id * value) Env.t ref
      
let err s = raise (Eval_error (Printf.sprintf "Runtime error: %s" s))

(* pretty printer for value *)
let pps_val = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | FunV _ -> "<fun>"

let pp_val v =
  print_string @< pps_val v

(* evaluation for binary operator *)
let eval_binop = function
  (* airthmetic expression *)
    (Plus, IntV vl, IntV vr) -> IntV (vl + vr)
  | (Minus, IntV vl, IntV vr) -> IntV (vl - vr)
  | (Mult, IntV vl, IntV vr) -> IntV (vl * vr)
  | (Div, IntV vl, IntV vr) when vr <> 0  -> IntV (vl / vr)
  | (Div, IntV _, IntV _)  -> err "division by zero isn't allowed"
  | (Plus as op, _, _) | (Minus as op, _, _) | (Mult as op, _, _) | (Div as op, _, _) ->
      err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  (* equal *)
  | (Eq, IntV v1, IntV v2) -> BoolV (v1 = v2)
  | (Eq, BoolV v1, BoolV v2) -> BoolV (v1 = v2)
  | (Eq, FunV _, FunV _) -> err "functions cannot be compared"
  | (Eq, _, _) -> err "both arguments of = must be same type"

(* evaluation for exp *)
let rec eval_exp env = function
    Var var ->
      begin match Env.lookup env var with
        None -> err @< Printf.sprintf "%s is not bound" var
      | Some v -> v
      end
  | IntLit  i -> IntV i
  | BoolLit b -> BoolV b
  | BinOp (op, el, er) -> eval_binop (op, eval_exp env el, eval_exp env er)
  | IfExp (cond, then_exp, else_exp) -> begin
      match eval_exp env cond with
        BoolV b -> eval_exp env @< if b then then_exp else else_exp
      | _       -> err "value of conditional expression must be bool"
    end
  | Fun (var, _, body) -> FunV (var, body, ref env)
  | App (f, act) ->
      begin match eval_exp env f, eval_exp env act with
        FunV (formal, body, env'), act ->
          eval_exp (Env.extend !env' formal act) body
      | _ -> err "not-function value is applied"
      end
  | Let (var, exp, body) ->
      let v = eval_exp env exp in
      eval_exp (Env.extend env var v) body
  | LetRec (var, _, exp, body) -> begin
      let v = eval_rec env var exp in
      eval_exp (Env.extend env var v) body
    end
      
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
  | EOF -> assert false
