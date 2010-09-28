open Misc
open Syntax

exception Typing_error of string
let err s = raise (Typing_error (Printf.sprintf "Typing error: %s" s))

(* typing for constant *)
let typ_const = function
    CInt _ -> IntT
  | CBool _ -> BoolT
  | CNullList typ -> begin
      match typ with
        ListT _ -> typ
      | _       -> err "specified type isn't list type"
    end
      
(* typing for binary operator *)      
let typ_binop typ1 typ2 = function
    (Plus | Minus | Mult | Div) as op ->
      if typ1 = IntT && typ2 = IntT then IntT
      else err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop op
  | Lt ->
      if typ1 = IntT && typ2 = IntT then BoolT
      else err @< Printf.sprintf "both arguments of %s must be integer" @< str_of_binop Lt
  | Eq ->
      if Type.eq_typ typ1 typ2 then BoolT
      else err @< Printf.sprintf "both arguments of %s must be same types" @< str_of_binop Eq
  | Cons -> begin
      match typ2 with
        ListT etyp ->
          if Type.eq_typ typ1 etyp then ListT etyp
          else err @< Printf.sprintf "element types of %s must be same types" @< str_of_binop Cons
      | _ -> err @< Printf.sprintf "right-side of %s must be list type" @< str_of_binop Cons
    end
      
(* typing for exp *)
let rec typ_exp tenv = function
    Var var -> begin
      match Env.lookup tenv var with
        Some t -> t
      | None   -> err @< Printf.sprintf "%s is not bound" var
    end
      
  | Const c -> typ_const c
      
  | BinOp (op, exp1, exp2) -> begin
      let typ1, typ2 = typ_exp tenv exp1, typ_exp tenv exp2 in
      typ_binop typ1 typ2 op
    end
      
  | IfExp (cond, then_exp, else_exp) -> begin
      match typ_exp tenv cond with
        BoolT -> begin
          let then_typ = typ_exp tenv then_exp in
          let else_typ = typ_exp tenv else_exp in
          if Type.eq_typ then_typ else_typ then then_typ
          else err "types of then and else expressions must be same"
        end
      | _ -> err "type of conditional expression must be boolean"
    end
      
  | Fun (var, typ, body) ->
      FunT (typ, typ_exp (Env.extend tenv var typ) body)
        
  | App (exp1, exp2) -> begin
      match typ_exp tenv exp1 with
        FunT (arg_typ, ret_typ) ->
          if Type.eq_typ arg_typ @< typ_exp tenv exp2 then ret_typ
          else err "type of actual argument must correspond with one of formal argument"
      | _ -> err "only function type can be applied"
    end
      
  | Let (var, exp, body) ->
      typ_exp (Env.extend tenv var @< typ_exp tenv exp) body
        
  | LetRec (var, typ, exp, body) ->
      typ_exp (Env.extend tenv var @< typ_letrec tenv var typ exp) body

  | ListLit exps -> begin
      let typs = List.map (typ_exp tenv) exps in
      let typ, typs = match typs with typ::typs -> (typ, typs) | _ -> assert false in (* assume exps is not empty *)
      List.iter (fun typ' -> if not (Type.eq_typ typ typ') then err "element types of list must be same") typs;
      ListT typ
    end
      
  | TypedExpr (exp, typ) -> begin
      let typ' = typ_exp tenv exp in
      if Type.eq_typ typ typ' then typ
      else err "expression's type doesn't cossrespond with the specified type"
    end
      
  | MatchExp (exp, branches) -> begin
      let typ = typ_exp tenv exp in
      let rec iter cond_typ = function
          [(pat, body)] -> begin
            let tenv' = Patmatch.tmatch err cond_typ pat in
            let tenv = Env.extend_by_env tenv tenv' in
            typ_exp tenv body
          end
        | (pat, body)::t -> begin
            let tenv' = Patmatch.tmatch err cond_typ pat in
            let tenv = Env.extend_by_env tenv tenv' in
            let btyp = typ_exp tenv body in
            let btyp' = iter cond_typ t in
            if Type.eq_typ btyp btyp' then btyp
            else err @< Printf.sprintf "%s doesn't match with %s: all branch expresions must be same types"
              (Type.pps_typ btyp) (Type.pps_typ btyp')
          end
        | _ -> assert false
      in
      iter typ branches
    end
      
(* typing for let-rec *)
and typ_letrec tenv var typ exp =
  match exp, typ with
    Fun _, FunT _ -> begin
      let tenv' = (Env.extend tenv var typ) in
      let etyp = typ_exp tenv' exp in
      if Type.eq_typ typ etyp then etyp
      else err "expression's type doesn't cossrespond with the specified type"
    end
  | _    -> err "only values which are functions can be defined recursively"
    
(* typing for program *)
let typing tenv =
  let return tenv var typ =
    Env.extend tenv var typ, var, typ
  in
  function
    Exp exp -> return tenv "it" @< typ_exp tenv exp
  | Decl (var, exp) -> return tenv var @< typ_exp tenv exp
  | DeclRec (var, typ, exp) -> return tenv var @< typ_letrec tenv var typ exp
  | EOF -> assert false
