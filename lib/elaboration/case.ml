module Term = Core.Term
module Level = Core.Level
module Environment = Core.Environment
module Eliminator = Core.Eliminator
module Global_context = Core.Global_context
module Substitution = Core.Substitution
open Utils
open State

let infer_scrut infer st scrut =
  let st, scrut_tm, scrut_ty = infer st scrut in
  let scrut_ty = Reduction.whnf st scrut_ty in
  (st, scrut_tm, scrut_ty)

let split_head ind_ty =
  let head, args = Term.Application.unfold ind_ty in
  match head with
  | Term.Constant (ind_id, lvls) -> Some (ind_id, lvls, args)
  | _ -> None

let split_ind st ind_ty =
  let* ind_id, lvls, args = split_head ind_ty in
  let* ind = Environment.get_inductive st.environment ind_id in
  let* elim = Environment.get_eliminator st.environment ind.eliminator_id in
  let params = take elim.parameter_count args in
  let indices = drop elim.parameter_count args in
  Some (ind_id, ind.eliminator_id, elim, lvls, params, indices)

let rec fresh_doms st span n doms =
  if n = 0 then (st, List.rev doms)
  else
    let st, dom = Meta_context.fresh_type st span in
    fresh_doms st span (n - 1) (dom :: doms)

let fresh_motive st (elim : Eliminator.t) span =
  let count = elim.parameter_count + elim.index_count + 1 in
  let st, doms = fresh_doms st span count [] in
  let st, body = Meta_context.fresh_type st span in
  (st, Term.Lambda.fold doms body)

let motive_constr st motive params indices scrut expected span =
  let lhs = Term.Application.fold motive (params @ indices @ [ scrut ]) in
  Constraints.add_conversion st lhs expected span

let ctor_by_name st ind_id name ctor_id =
  let ind_name = Global_context.name st.global_context ind_id in
  let ind_name = Option.value ind_name ~default:"" in
  match Global_context.name st.global_context ctor_id with
  | Some nm when nm = name || nm = ind_name ^ "." ^ name -> Some ctor_id
  | _ -> None

let find_ctor st ind_id name =
  let* ind = Environment.get_inductive st.environment ind_id in
  let* ctor_id = List.find_map (ctor_by_name st ind_id name) ind.constructors in
  let* ctor = Environment.get_constructor st.environment ctor_id in
  Some (ctor_id, ctor)

let pat_name (pat : Syntax.pattern) =
  match pat.value with
  | Syntax.Variable nm -> Some nm.value
  | Syntax.Wildcard | Syntax.Constructor _ -> None

let synthetic_arg_name pos i = Printf.sprintf "?arg%i_%i" pos i

let arg_name pos pats i =
  let pat = List.nth_opt pats i in
  let declared = Option.bind pat pat_name in
  let default = synthetic_arg_name pos i in
  Option.value declared ~default

let ih_name pos i = Printf.sprintf "?ih%i_%i" pos i

let rec introduce st names doms =
  match (names, doms) with
  | [], [] -> st
  | name :: names, dom :: doms ->
      let st = Local_context.add st name dom in
      introduce st names doms
  | _ -> assert false

let unset_self st =
  match st.self with
  | None -> st
  | Some s ->
      let self =
        { State.name = s.name; State.typ = s.typ; State.term = Term.Bad }
      in
      State.with_self st (Some self)

let set_self st recs =
  match (st.self, recs) with
  | Some s, (_, _) :: _ ->
      let ih_idx = List.length recs - 1 in
      let ih_ty = Local_context.get_type st ih_idx in
      let name = s.name in
      let doms = Term.Pi.domains s.typ in
      let arity = List.length doms in
      let embed_sub ih = Substitution.shift arity 0 ih in
      let tm = Term.Lambda.fold doms (embed_sub (Term.Variable ih_idx)) in
      let ty = Term.Pi.fold doms (embed_sub ih_ty) in
      State.with_self st (Some { State.name; State.typ = ty; State.term = tm })
  | _ -> st

let rec substitute_outer outer depth tm =
  match tm with
  | Term.Variable i ->
      if i < depth then Term.Variable i
      else
        let j = i - depth in
        let outer_n = List.length outer in
        if j < outer_n then
          (* An outer role: substitute, shifting for the binders below. *)
          Substitution.shift depth 0 (List.nth outer (outer_n - 1 - j))
        else
          (* A branch binder: drop the outer roles before it. *)
          Term.Variable (i - outer_n)
  | Term.Pi (a, b) ->
      Term.Pi
        (substitute_outer outer depth a, substitute_outer outer (depth + 1) b)
  | Term.Lambda (a, b) ->
      Term.Lambda
        (substitute_outer outer depth a, substitute_outer outer (depth + 1) b)
  | Term.Application (a, b) ->
      Term.Application
        (substitute_outer outer depth a, substitute_outer outer depth b)
  | tm -> tm

let branch_goal outer minor_ty =
  let doms = Term.Pi.domains minor_ty in
  let cod = Term.Pi.codomain minor_ty in
  let outer_n = List.length outer in
  let recs_n = List.length doms in
  (* Piece i sits under i branch binders and its frame carries
     outer_n + (total branch binders) roles... each piece was built in
     the frame [outer_roles @ all branch binders] for the FULL minor
     (eliminator.ml computes arg/IH/cod pieces in their own frames); the
     piece frames are:
       dom i  : outer_n + i roles (branch binders so far)
       cod    : outer_n + recs_n roles
     Re-index each with its own binder count as [depth]. *)
  let doms = List.mapi (fun i dom -> substitute_outer outer i dom) doms in
  let cod = substitute_outer outer recs_n cod in
  ignore outer_n;
  (doms, cod)

let branch_binders pos pats (ctor : Core.Constructor.t) =
  let args_n = List.length ctor.arguments in
  let recs = Core.Constructor.recursive_arguments ctor in
  let arg_names = List.init args_n (arg_name pos pats) in
  let rec_names = List.init (List.length recs) (ih_name pos) in
  let names = arg_names @ rec_names in
  (recs, names)

let branch_context st doms names recs =
  let st = unset_self st in
  let st = introduce st names doms in
  set_self st recs

let elaborate_constructor_branch check st motive params minors
    (lyt : Eliminator.Type.layout) pos ctor_id ctor pats body =
  let minor_ty = Eliminator.Type.minor_type lyt pos (ctor_id, ctor) in
  (* Roles preceding the branch binders, outermost first: parameters,
     then the motive, then the previously elaborated minor premises. *)
  let outer = params @ [ Term.Application.fold motive params ] @ minors in
  let doms, cod = branch_goal outer minor_ty in
  let recs, names = branch_binders pos pats ctor in
  let st = branch_context st doms names recs in
  let st, tm = check st body cod in
  (st, Term.Lambda.fold doms tm)

let constructor_branch check st motive params minors lyt pos name pats body =
  let* ctor = find_ctor st lyt.Eliminator.Type.id name in
  let ctor_id, ctor = ctor in
  Some
    (elaborate_constructor_branch check st motive params minors lyt pos ctor_id
       ctor pats body)

let report_expected_constructor st span =
  State.report st Expected_constructor span;
  None

let elaborate_branch check st motive params minors lyt pos (c : Syntax.case) =
  let pattern = c.value.pattern in
  match pattern.value with
  | Syntax.Constructor (name, pats) ->
      constructor_branch check st motive params minors lyt pos name.value
        pats.value c.value.body
  | Syntax.Variable name ->
      constructor_branch check st motive params minors lyt pos name.value []
        c.value.body
  | Syntax.Wildcard -> Some (st, Term.Bad)

let report_missing st (c : Syntax.case) =
  State.report st Expected_constructor c.value.pattern.span;
  (st, Term.Bad)

let elaborate_branches check st motive params lyt (cases : Syntax.cases) =
  let ctors =
    match Environment.get_inductive st.environment lyt.Eliminator.Type.id with
    | Some ind -> List.length ind.constructors
    | None -> 0
  in
  (* Each branch may reference the previously elaborated minors. *)
  let rec go minors st pos = function
    | [] -> (st, List.rev minors)
    | c :: rest -> (
        match elaborate_branch check st motive params minors lyt pos c with
        | Some (st, minor) -> go (minor :: minors) st (pos + 1) rest
        | None ->
            let st, bad = report_missing st c in
            go (bad :: minors) st (pos + 1) rest)
  in
  let st, minors = go [] st 0 cases.value in
  let wildcard_count =
    List.length (List.filter (fun p -> match p with Syntax.Wildcard -> true | _ -> false)
      (List.map (fun (c : Syntax.case) -> let pattern = c.value.pattern in pattern.value) cases.value))
  in
  let missing =
    if wildcard_count > 0 then 0
    else ctors - List.length minors
  in
  if missing > 0 then
    let span = cases.span in
    let st =
      List.fold_left
        (fun st _ ->
          State.report st (Inexhaustive_match missing) span;
          st)
        st
        (List.init missing (fun _ -> ()))
    in
    (st, minors @ List.init missing (fun _ -> Term.Bad))
  else (st, minors)

let rec bads n = if n = 0 then [] else Term.Bad :: bads (n - 1)

let build_app elim_id params motive minors indices scrut =
  let args = params @ [ motive ] @ minors @ indices @ [ scrut ] in
  Term.Application.fold (Term.constant elim_id []) args

let motive_ty (elim : Eliminator.t) ind_id lvls params =
  let fam = Term.Pi.fold (bads elim.index_count) Term.Bad in
  Eliminator.Type.layout ind_id params fam lvls (Level.join_all lvls)

let rec distinct_vars = function
  | [] -> Some []
  | Term.Variable i :: rest -> (
      match distinct_vars rest with
      | Some vars when List.exists (( = ) i) vars -> None
      | Some vars -> Some (i :: vars)
      | None -> None)
  | _ :: _ -> None

let motive_over st vars expected =
  if vars = [] then expected
  else
    let doms = List.map (fun i -> Local_context.get_type st i) vars in
    Term.Lambda.fold doms (Substitution.abstract vars expected)

let motive_for st scrut_tm indices expected =
  let scrut_vars = match scrut_tm with Term.Variable i -> [ i ] | _ -> [] in
  match distinct_vars indices with
  | Some index_vars -> motive_over st (scrut_vars @ index_vars) expected
  | None -> motive_over st scrut_vars expected

let open_elim st elim ind_id lvls params indices scrut_tm expected =
  let lyt = motive_ty elim ind_id lvls params in
  let motive = motive_for st scrut_tm indices expected in
  (st, lyt, motive)

let elaborate infer check st scrut cases expected span =
  let st, scrut_tm, scrut_ty = infer_scrut infer st scrut in
  match split_ind st scrut_ty with
  | Some (ind_id, elim_id, elim, lvls, params, indices) ->
      let st, lyt, motive =
        open_elim st elim ind_id lvls params indices scrut_tm expected
      in
      let st, minors = elaborate_branches check st motive params lyt cases in
      let st = unset_self st in
      let tm = build_app elim_id params motive minors indices scrut_tm in
      (st, tm)
  | None ->
      State.report st (Expected_inductive scrut_ty) span;
      (st, Term.Bad)
