open Core

let conversion left right =
  Span.locate (Constraints.Conversion (left, right)) Span.zero

let inequality left right =
  Span.locate (Constraints.Universe_inequality (left, right)) Span.zero

let solves terms constraint_ =
  let solution =
    Unification.solve terms Level_meta_context.empty Environment.empty
      [ constraint_ ]
  in
  List.is_empty solution.failed && List.is_empty solution.postponed

let check_solves_with name terms constraint_ expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) "solved" expected (solves terms constraint_))

let check_solves name constraint_ expected =
  check_solves_with name Term_meta_context.empty constraint_ expected

let rigid =
  [
    check_solves "same variable"
      (conversion (Term.Variable 0) (Term.Variable 0))
      true;
    check_solves "different variables"
      (conversion (Term.Variable 0) (Term.Variable 1))
      false;
    check_solves "same sort"
      (conversion (Term.Sort Level.Zero) (Term.Sort Level.Zero))
      true;
    check_solves "different sorts"
      (conversion (Term.Sort Level.Zero) (Term.Sort (Level.Succ Level.Zero)))
      false;
    check_solves "same pi"
      (conversion
         (Term.Pi (Term.Sort Level.Zero, Term.Variable 0))
         (Term.Pi (Term.Sort Level.Zero, Term.Variable 0)))
      true;
    check_solves "different pi codomains"
      (conversion
         (Term.Pi (Term.Sort Level.Zero, Term.Variable 0))
         (Term.Pi (Term.Sort Level.Zero, Term.Variable 1)))
      false;
    check_solves "pi vs sort"
      (conversion
         (Term.Pi (Term.Sort Level.Zero, Term.Variable 0))
         (Term.Sort Level.Zero))
      false;
    check_solves "same lambda"
      (conversion
         (Term.Lambda (Term.Sort Level.Zero, Term.Variable 0))
         (Term.Lambda (Term.Sort Level.Zero, Term.Variable 0)))
      true;
  ]

let pattern_terms () =
  let lc = Local_context.add Local_context.empty "x" (Term.Sort Level.Zero) in
  Term_meta_context.fresh Term_meta_context.empty lc (Term.Sort Level.Zero)
    Span.zero

let check_meta_solution name constraint_ expected =
  Alcotest.test_case name `Quick (fun () ->
      let solution =
        Unification.solve (pattern_terms ()) Level_meta_context.empty
          Environment.empty [ constraint_ ]
      in
      let solved =
        List.is_empty solution.failed && List.is_empty solution.postponed
      in
      Alcotest.(check bool) "all constraints solved" true solved;
      let assigned = Term_meta_context.find_term solution.terms 0 in
      Alcotest.(check bool) "meta solution" true (assigned = Some expected))

let pattern =
  [
    check_meta_solution "pattern ?m x =?= x"
      (conversion
         (Term.Application (Term.Meta_variable 0, Term.Variable 0))
         (Term.Variable 0))
      (Term.Lambda (Term.Sort Level.Zero, Term.Variable 0));
    check_solves_with "repeated variable is not a pattern" (pattern_terms ())
      (conversion
         (Term.Application
            ( Term.Application (Term.Meta_variable 0, Term.Variable 0),
              Term.Variable 0 ))
         (Term.Variable 0))
      false;
  ]

let levels =
  [
    check_solves "zero equals zero" (inequality Level.Zero Level.Zero) true;
    check_solves "one differs from two"
      (inequality (Level.Succ Level.Zero) (Level.Succ (Level.Succ Level.Zero)))
      false;
    check_solves "max 0 1 equals 1"
      (inequality
         (Level.Max (Level.Zero, Level.Succ Level.Zero))
         (Level.Succ Level.Zero))
      true;
  ]

let through_driver =
  [
    Alcotest.test_case "meta in hole is instantiated from expected type" `Quick
      (fun () ->
        let r = Driver.run (Support.nat_prelude ^ "def n : Nat := _") in
        match Driver.value_of r "n" with
        | None -> Alcotest.fail "global n not found"
        | Some tm ->
            Alcotest.(check string)
              "hole solved to" "Nat.zero"
              (Driver.pretty_normalised r tm));
  ]

let tests = rigid @ pattern @ levels @ through_driver
