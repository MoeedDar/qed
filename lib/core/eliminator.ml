open Utils

type t = {
  inductive_id : int;
  parameter_count : int;
  index_count : int;
  constructor_count : int;
  typ : Term.t;
}

module Type = struct
  module Scope = struct
    type role =
      | Parameter of int
      | Index of int
      | Motive
      | Minor of int
      | Scrutinee
      | Argument of int
      | Inductive_hypothesis of int

    type t = role list

    let position scope role =
      let rec go pos = function
        | [] -> invalid_arg "Eliminator.Type.Scope.position: unbound role"
        | role' :: scope -> if role' = role then pos else go (pos + 1) scope
      in
      go 0 scope

    let index scope role = List.length scope - 1 - position scope role
    let variable scope role = Term.Variable (index scope role)
    let variables scope roles = List.map (variable scope) roles
    let parameters n = List.init n (fun i -> Parameter i)
    let indices count = List.init count (fun i -> Index i)
    let minors n = List.init n (fun i -> Minor i)
    let arguments n = List.init n (fun i -> Argument i)
    let inductive_hypotheses n = List.init n (fun i -> Inductive_hypothesis i)

    let move src tgt tm =
      let src_len = List.length src in
      let tgt_len = List.length tgt in
      let map d var =
        if var < d then var
        else
          let pos' = src_len - var - d - 1 in
          let role = List.nth src pos' in
          tgt_len - 1 - position tgt role + d
      in
      Term.Binding.map map 0 tm
  end

  type layout = {
    id : int;
    level : Level.t;
    levels : Level.t list;
    parameters : Term.t list;
    index_family : Term.t;
    parameter_scope : Scope.t;
    indices_scope : Scope.t;
  }

  let layout id parameters index_family levels level =
    let parameter_scope = List.length parameters |> Scope.parameters in
    let indices_scope = Term.Pi.arity index_family |> Scope.indices in
    {
      id;
      levels;
      level;
      parameters;
      index_family;
      parameter_scope;
      indices_scope;
    }

  let inductive_at lyt scope =
    lyt.parameter_scope @ lyt.indices_scope
    |> Scope.variables scope
    |> Term.Application.of_constant lyt.id lyt.levels

  let motives_scope lyt = lyt.parameter_scope @ [ Scope.Motive ]
  let minor_scope lyt pos = motives_scope lyt @ Scope.minors pos

  let motive_application scope indices subj =
    let head = Scope.variable scope Scope.Motive in
    let args = indices @ [ subj ] in
    Term.Application.fold head args

  let motive_type lyt =
    let scope = lyt.parameter_scope @ lyt.indices_scope in
    let ind = inductive_at lyt scope in
    let sort = Term.Sort lyt.level in
    Term.Pi.append_domain lyt.index_family ind sort

  let argument_type lyt scope (arg_i, arg) =
    let prec = Scope.arguments arg_i in
    let src = lyt.parameter_scope @ prec in
    let tgt = scope @ prec in
    let ty = Constructor.Argument.typ arg in
    Scope.move src tgt ty

  let minor_binds scope arg_scope ihs =
    scope @ arg_scope @ Scope.inductive_hypotheses ihs

  let inductive_hypothesis_type lyt scope field_scope (i, indices) =
    let prec = Scope.arguments i in
    let src = lyt.parameter_scope @ prec in
    let tgt = minor_binds scope field_scope i in
    let indices = List.map (Scope.move src tgt) indices in
    let subj = Scope.variable tgt (Scope.Argument i) in
    motive_application tgt indices subj

  let minor_codomain lyt scope arg_scope inds id ctors =
    let tgt = minor_binds scope arg_scope inds in
    let src = lyt.parameter_scope @ arg_scope in
    let indices =
      List.map (Scope.move src tgt) ctors.Constructor.codomain_arguments
    in
    let args = Scope.variables tgt (lyt.parameter_scope @ arg_scope) in
    let subj = Term.Application.of_constant id lyt.levels args in
    motive_application tgt indices subj

  let index_domain lyt scope j dom =
    let src = lyt.parameter_scope @ take j lyt.indices_scope in
    let tgt = scope @ take j lyt.indices_scope in
    Scope.move src tgt dom

  let major_type lyt minors_n =
    let scope = motives_scope lyt @ Scope.minors minors_n in
    let ind_scope = scope @ lyt.indices_scope in
    let scrut_scope = ind_scope @ [ Scope.Scrutinee ] in

    let doms =
      Term.Pi.domains lyt.index_family |> List.mapi (index_domain lyt scope)
    in
    let ind = inductive_at lyt ind_scope in
    let mot =
      let scrut = Scope.variable scrut_scope Scope.Scrutinee in
      let indices = Scope.variables scrut_scope lyt.indices_scope in
      motive_application scrut_scope indices scrut
    in

    Term.Pi.fold doms (Pi (ind, mot))

  let minor_type lyt pos (id, ctor) =
    let scope = minor_scope lyt pos in

    let args = List.mapi (fun i arg -> (i, arg)) ctor.Constructor.arguments in
    let args_n = List.length args in
    let arg_scope = Scope.arguments args_n in
    let arg_type = argument_type lyt scope in
    let arg_doms = List.map arg_type args in

    let recs = Constructor.recursive_arguments ctor in
    let recs_n = List.length recs in
    let ih_type = inductive_hypothesis_type lyt scope arg_scope in
    let ih_doms = List.map ih_type recs in

    let doms = arg_doms @ ih_doms in
    let cod = minor_codomain lyt scope arg_scope recs_n id ctor in

    Term.Pi.fold doms cod

  let minor_types lyt ctors = List.mapi (minor_type lyt) ctors

  let typ ind_id params fam ctors lvls lvl =
    let lyt = layout ind_id params fam lvls lvl in
    let ctors_n = List.length ctors in

    let minors = minor_types lyt ctors in
    let major = major_type lyt ctors_n in
    let motive = motive_type lyt in

    let dom = lyt.parameters @ [ motive ] @ minors in

    Term.Pi.fold dom major
end

module Reduce = struct
  let minor_position elim ctor_pos = elim.parameter_count + 1 + ctor_pos
  let indices_position elim = minor_position elim elim.constructor_count
  let major_position elim = indices_position elim + elim.index_count

  let constructor_arguments elim major =
    Term.Application.arguments major |> drop elim.parameter_count

  let substitute_indices elim prefix args arg_pos =
    let params = take elim.parameter_count prefix in
    let prec = take arg_pos args in
    let ctx = List.rev (params @ prec) in
    List.nth ctx

  let substitute_all_indices elim minor_args args arg_pos indices =
    let subst = substitute_indices elim minor_args args arg_pos in
    List.map (Substitution.substitute subst) indices

let apply_inductive_hypothesis elim minor_args head args major (arg_pos, indices) =
   let ctor_args = constructor_arguments elim major in
   let indices = substitute_all_indices elim minor_args args arg_pos indices in
   let arg = List.nth ctor_args arg_pos in
   Term.Application.fold head (minor_args @ indices @ [ arg ])

let apply_inductive_hypotheses elim minor_args head args major ctor =
   let ih = apply_inductive_hypothesis elim minor_args head args major in
   let recs = Constructor.recursive_arguments ctor in
   List.map ih recs

let reduce elim head args ctor major minor =
   let ctor_args = constructor_arguments elim major in
   let minor_args = take (indices_position elim) args in
   let ihs = apply_inductive_hypotheses elim minor_args head args major ctor in
   let rest = drop (major_position elim + 1) args in
   let args = ctor_args @ ihs @ rest in
   Term.Application.fold minor args
end

let make inductive_id parameters index_family constructors levels =
  let lvl = Level.join_all levels in
  let typ =
    Type.typ inductive_id parameters index_family constructors levels lvl
  in
  let parameter_count = List.length parameters in
  let index_count = Term.Pi.arity index_family in
  let constructor_count = List.length constructors in
  { inductive_id; parameter_count; index_count; constructor_count; typ }

let typ = Type.typ
let major_position = Reduce.major_position
let minor_position = Reduce.minor_position
let reduce = Reduce.reduce
let inductive_id e = e.inductive_id
