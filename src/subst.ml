open Misc
open Common
open OptionMonad
open Types
open TypeContext

type t = typvar * typ list
type eq = typ * typ

let empty = []

(* substitution *)
let rec subst_typvar subst id =
  let rec subst_per_eq src_typ id dst_typ eqs =
    match src_typ with
    | TyVar id' -> begin
        if id = id' then body dst_typ eqs
        else body src_typ eqs
      end
    | TyFun (ftyp, rtyp) ->
        TyFun (subst_per_eq ftyp id dst_typ eqs, subst_per_eq rtyp id dst_typ eqs)
    | TyVariant (typs, ident) ->
        TyVariant (List.map (fun src_typ -> subst_per_eq src_typ id dst_typ eqs) typs, ident)
    | TyAlias (typ, typs, ident) ->
        TyAlias (subst_per_eq typ id dst_typ eqs,
                 List.map (fun src_typ -> subst_per_eq src_typ id dst_typ eqs) typs,
                 ident)

  and body typ = function
      [] -> typ
    | (id, dst_typ)::eqs ->
        subst_per_eq typ id dst_typ eqs
  in
  body (TyVar id) subst

let rec subst_typ subst = function
  | TyFun (ftyp, rtyp) -> TyFun (subst_typ subst ftyp, subst_typ subst rtyp)
  | TyVar id -> subst_typvar subst id
  | TyVariant (typs, ident) ->
      TyVariant (List.map (subst_typ subst) typs, ident)
  | TyAlias (typ, typs, ident) ->
      TyAlias (subst_typ subst typ, List.map (subst_typ subst) typs, ident)
      
let rec subst_type_scheme subst typ_scheme =
   let bound_typvars = TypeScheme.bound_typvars typ_scheme in
   let typ = TypeScheme.typ typ_scheme in
   let rec iter = function
     | TyFun (ftyp, rtyp) -> TyFun (iter ftyp, iter rtyp)
     | TyVar id -> begin
         if TypVarSet.mem id bound_typvars then TyVar id
         else subst_typvar subst id
       end
     | TyVariant (typs, ident) -> TyVariant (List.map iter typs, ident)
     | TyAlias (typ, typs, ident) ->
         TyAlias (iter typ, List.map iter typs, ident)
   in
   (bound_typvars, iter typ)

let subst_tenv subst tenv =
  let subst = subst_type_scheme subst in
  Env.extendl Env.empty @< List.rev @<
    Env.fold tenv
    (fun acc (var, typ_scheme) -> (var, subst typ_scheme)::acc) []

let subst_tctx subst tctx =
  { tctx with typ_env = subst_tenv subst tctx.typ_env }

(* unify *)
let equations_of subst = List.fold_left (fun acc (id, typ) -> (TyVar id, typ)::acc) [] subst

let subst_in_equations id typ =
  let subst = [(id, typ)] in
  List.map (fun (typ1, typ2) -> (subst_typ subst typ1, subst_typ subst typ2))

let rec invalid_eq id typ =
  match typ with
    TyVar id' when id = id' -> false
  | TyAlias (typ, _, _) -> invalid_eq id typ
  | _ -> TypVarSet.mem id @< TypeScheme.freevars typ

let unify_eqs eqs =
  let rec unify_eq acc eqs = function
    | TyVar id, (_ as typ) | (_ as typ), TyVar id ->
        if invalid_eq id typ then None
        else iter ((id, typ)::acc) @< subst_in_equations id typ eqs
    | TyFun (ftyp1, rtyp1), TyFun (ftyp2, rtyp2) ->
        iter acc @< (ftyp1, ftyp2)::(rtyp1, rtyp2)::eqs
    | TyVariant (typs1, ident1), TyVariant (typs2, ident2) -> begin
        if not (Ident.equal ident1 ident2) then None
        else iter acc @< List.combine typs1 typs2 @ eqs
      end
    | (TyAlias (atyp, _, _), typ) | (typ, TyAlias (atyp, _, _)) ->
        iter acc @< (atyp, typ)::eqs
    | (TyFun _|TyVariant _), _ -> None

  and iter acc = function
      [] -> Some acc
    | eq::eqs -> unify_eq acc eqs eq
  in
  (iter empty eqs) >>= fun l -> Some (List.rev l)
  
let unifyl subst eqs =
  unify_eqs @< eqs @ (equations_of subst)

let unify subst typ1 typ2 =
  unifyl subst [(typ1, typ2)]

let unify_monotyp_env subst env env' =
  let mem, mem' = Env.members env, Env.members env' in
  if not (VariableSet.equal mem mem') then None
  else
    let eqs =
      VariableSet.fold
        (fun var acc ->
           match Env.lookup env var, Env.lookup env' var with
           | (Some typ, Some typ')
               when TypVarSet.equal TypVarSet.empty @< TypeScheme.bound_typvars typ &&
                    TypVarSet.equal TypVarSet.empty @< TypeScheme.bound_typvars typ'  ->
               (TypeScheme.typ typ, TypeScheme.typ typ')::acc
           | _ -> assert false)
        mem []
    in
    unifyl subst eqs

(* let unify_list subst typ = *)
(*   let etyp = Types.fresh_typvar () in *)
(*   let ltyp = ListT etyp in *)
(*   unify subst typ ltyp >>= fun subst -> Some (subst, subst_typ subst etyp) *)
    
(* let merge subst1 subst2 =  *)
(*   unifyl subst1 @< equations_of subst2 *)
  
