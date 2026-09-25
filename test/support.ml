open Driver

let nat_prelude = "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n"

let prelude = Prelude.text

let check_ok name source =
  Alcotest.test_case name `Quick (fun () ->
      let r = run source in
      Alcotest.(check bool) "no crash" false r.crashed;
      Alcotest.(check (list string)) "no diagnostics" [] (messages r);
      Alcotest.(check int) "no failed constraints" 0
        (List.length r.Driver.failed_constraints))

let mentions expected message =
  let n = String.length expected in
  String.sub message 0 (min n (String.length message)) = expected

let check_diagnostic name source expected =
  Alcotest.test_case name `Quick (fun () ->
      let r = run source in
      Alcotest.(check bool) "no crash" false r.crashed;
      let found = List.exists (mentions expected) (messages r) in
      Alcotest.(check bool)
        (Printf.sprintf "diagnostics mention %S" expected)
        true found)

let check_value name source global expected =
  Alcotest.test_case name `Quick (fun () ->
      let r = run source in
      Alcotest.(check bool) "no crash" false r.crashed;
      Alcotest.(check (list string)) "no diagnostics" [] (messages r);
      match value_of r global with
      | None -> Alcotest.failf "global %s not found" global
      | Some tm ->
          Alcotest.(check string) "normalised value" expected
            (pretty_normalised r tm))

let check_type name source global expected =
  Alcotest.test_case name `Quick (fun () ->
      let r = run source in
      Alcotest.(check bool) "no crash" false r.crashed;
      Alcotest.(check (list string)) "no diagnostics" [] (messages r);
      match type_of r global with
      | None -> Alcotest.failf "global %s not found" global
      | Some tm -> Alcotest.(check string) "type" expected (pretty r tm))
