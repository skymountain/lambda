open Misc
open Type

type context =
    {
      typ_env   : (Syntax.id * typ) Env.t;
      typdef_env: (Ident.t * typdef) Env.t;
      constr_env: (string * typdef) Env.t;
    }

let empty = {
  typ_env    = Env.empty;
  typdef_env = Env.empty;
  constr_env = Env.empty;
}

let find_typ tctx ident = Env.lookup tctx.typdef_env ident

let lookup_var tctx = Env.lookup tctx.typ_env
let lookup_typ tctx name = Env.fold tctx.typdef_env
  (fun x (k, v) ->
     if x <> None then x
     else if Ident.name k = name then Some (k, v)
     else None)
  None
let lookup_constr tctx = Env.lookup tctx.constr_env

let add_typ tctx typdef ident =
  let constr_env =
    match typdef.td_kind with
      TkAlias _             -> tctx.constr_env
    | TkVariant constr_list -> begin
        List.fold_left
          (fun constr_env (constr_name, _) -> Env.extend constr_env constr_name typdef)
          tctx.constr_env constr_list
      end
  in
  { tctx with typdef_env = Env.extend tctx.typdef_env ident typdef; constr_env = constr_env }
let add_var tctx var typ = { tctx with typ_env = Env.extend tctx.typ_env var typ }

let extend_typ_env tctx typ_env = { tctx with typ_env = Env.extend_by_env tctx.typ_env typ_env }
