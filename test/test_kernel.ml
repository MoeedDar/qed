open Core
open Core.Term

let accepts name term typ =
  Alcotest.test_case name `Quick (fun () ->
      match Kernel.check Environment.empty Context.empty term typ with
      | () -> ()
      | exception Kernel.Error _ -> Alcotest.fail "kernel rejected a valid term")

let rejects name term typ =
  Alcotest.test_case name `Quick (fun () ->
      match Kernel.check Environment.empty Context.empty term typ with
      | () -> Alcotest.fail "kernel accepted an invalid term"
      | exception Kernel.Error _ -> ())

let identity = Lambda (Sort Level.Zero, Variable 0)
let identity_type = Pi (Sort Level.Zero, Sort Level.Zero)
let constant = Lambda (Sort Level.Zero, Sort Level.Zero)
let constant_type = Pi (Sort Level.Zero, Sort (Level.Succ Level.Zero))

let tests =
  [
    accepts "identity" identity identity_type;
    accepts "constant function" constant constant_type;
    rejects "type mismatch" constant (Sort Level.Zero);
    rejects "metavariable"
      (Lambda (Sort Level.Zero, Meta_variable 0))
      identity_type;
    rejects "error term" (Lambda (Sort Level.Zero, Bad)) identity_type;
    rejects "application of a non-function"
      (Application (Sort Level.Zero, Sort Level.Zero))
      (Sort Level.Zero);
    rejects "pi whose domain is not a type"
      (Pi (constant, Sort Level.Zero))
      (Sort Level.Zero);
  ]
