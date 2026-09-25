open Support

let basics =
  [
    check_ok "identity" "def id (A : Type) (x : A) : A := x";
    check_ok "constant function"
      "def const (A : Type) (B : Type) (x : A) (y : B) : A := x";
    check_diagnostic "polymorphic application"
      "def id (A : Type) (x : A) : A := x\ndef use : Type0 := id Type0 Type0"
      "type mismatch";
    check_ok "let binding"
      (nat_prelude ^ "def one : Nat := let k : Nat := Nat.zero in k");
  ]

let numerals =
  [
    check_ok "numeral literal" (nat_prelude ^ "def two : Nat := 2");
    check_value "numeral normalises" nat_prelude "Nat.two"
      "Nat.succ (Nat.succ Nat.zero)";
  ]

let errors =
  [
    check_diagnostic "unbound variable" "def broken : Type := nope"
      "unbound reference `nope`";
    check_diagnostic "apply non-function" "def oops : Type0 := Type0 Type0"
      "expected a function type";
    check_diagnostic "argument type mismatch"
      "def f (x : Type0) : Type0 := x\ndef bad : Type0 := f Type0"
      "type mismatch";
    check_diagnostic "Type0 does not inhabit itself" "def bad : Type0 := Type0"
      "type mismatch";
  ]

let holes =
  [
    check_ok "hole body" (nat_prelude ^ "def n : Nat := _");
    check_ok "hole in type" (nat_prelude ^ "def x : _ := Nat.zero");
  ]

let case_analysis =
  [
    check_ok "case with both constructors"
      (nat_prelude
      ^ "def is_zero (n : Nat) : Type0 | zero => Type0 | succ k => Type0");
    check_ok "recursive definition"
      (nat_prelude ^ "def pred (n : Nat) : Nat | zero => Nat.zero | succ k => k");
    check_diagnostic "case on non-inductive"
      "def f (x : Type0) : Type0 := case x of | y => y"
      "expected an inductive type";
  ]

let universes =
  [
    check_ok "Type0 inhabits Type1" "def t : Type1 := Type0";
    check_ok "level-polymorphic identity" "def i (A : Type1) : Type1 := A";
  ]

let parametrised =
  [
    check_ok "parameterised type" "def Box (A : Type) : Type\n| box : Box A\n";
    check_ok "Eq is a parameterised family"
      (nat_prelude
      ^ "def Eq (A : Type) (a : A) (b : A) : Type\n| refl : Eq A a a\n");
    check_ok "dependent family"
      (nat_prelude ^ "def V (A : Type) : A -> Type\n| v : (a : A) -> V A a\n");
    check_ok "family with two indices"
      (nat_prelude
      ^ "def W (A : Type) : (a : A) -> (b : A) -> Type\n\
          | w : (a : A) -> W A a a\n");
    check_ok "family with three indices"
      (nat_prelude
      ^ "def T3 : Nat -> Nat -> Nat -> Type\n\
          | t3 : (a : Nat) -> (b : Nat) -> (c : Nat) -> T3 a b c\n");
    check_ok "family with non-uniform indices"
      (nat_prelude
      ^ "def N (A : Type) : (a : Nat) -> A -> Type\n\
          | n : (a : Nat) -> (x : A) -> N A a x\n");
  ]

let constructors =
  [
    check_diagnostic "constructor takes no explicit parameters"
      (nat_prelude
      ^ "def Eq (A : Type) (a : A) (b : A) : Type\n| refl : Eq A a a\n\n"
      ^ "def u : Eq Nat Nat.zero Nat.zero := Eq.refl Nat Nat.zero Nat.zero\n")
      "expected a function type";
    check_diagnostic "a constructor cannot take them explicitly"
      (nat_prelude
      ^ "def Eq (A : Type) (a : A) (b : A) : Type\n| refl : Eq A a a\n\n"
      ^ "def u : Eq Nat Nat.zero Nat.zero := Eq.refl Nat Nat.zero Nat.zero\n")
      "expected a function type";
    check_ok "dependent constructor argument"
      (nat_prelude
      ^ "def List (A : Type) : Type\n| nil : List A\n"
      ^ "| cons : (a : A) -> List A -> List A\n");
  ]

let shared_prelude =
  [
    check_ok "prelude elaborates" prelude;
    check_ok "prelude addition" (prelude ^ "def two : Nat := 1 + 1\n");
  ]

let reported =
  [
    check_diagnostic "type mismatch names both sides"
      (nat_prelude ^ "def bad : Type0 := Type1")
      "type mismatch";
    check_diagnostic "expected inductive names the scrutinee's type"
      "def f (x : Type0) : Type0 := case x of | y => y"
      "expected an inductive type, but found";
    check_ok "wildcard case is exhaustive"
      (nat_prelude ^ "def f (n : Nat) | _ => Type0\n");
  ]

let mechanism =
  [
    check_ok "implicit parameter resolves from one argument"
      "def f {A : Type} (a : A) : A := a\ndef u : Nat := f Nat.zero\n";
    check_ok "implicit parameter resolves from two arguments"
      "def f {A : Type} (a : A) (b : A) : A := a\n\
       def u : Nat := f Nat.zero Nat.zero\n";
    check_ok "a family is usable as a type"
      "def t : Type := Eq Nat.zero Nat.zero\n";
    check_ok "a family is usable with numerals" "def t : Type := 0 = 0\n";
    check_diagnostic "a bare constructor is not a type"
      "def t : Type := Eq.refl\n"
      "type mismatch";
    check_ok "a constructor takes its explicit argument"
      "def u : Eq Nat.zero Nat.zero := Eq.refl Nat.zero\n";
    check_ok "a goal of the family opens with a hole" "def t : 0 = 0 := _\n";
    check_ok "a plain alias of a family is definitionally transparent"
      "def myeq (A : Type) (a : A) (b : A) : Type := Eq a b\n\
       def t : myeq Nat Nat.zero Nat.zero := Eq.refl Nat.zero\n";
    check_ok "an implicit alias of a family is definitionally transparent"
      "def myeq {A : Type} (a : A) (b : A) : Type := Eq a b\n\
       def t : myeq Nat.zero Nat.zero := Eq.refl Nat.zero\n";
    check_ok "a type-valued goal is unfolded"
      "def ret (A : Type) : Type := A\ndef t : ret Nat := Nat.zero\n";
    check_ok "a type-valued goal unfolds to a family"
      "def ty (A : Type) (a : A) : Type := Eq a a\n\
       def t : ty Nat Nat.zero := Eq.refl Nat.zero\n";
    check_ok "a body with no family at all"
      "def ty2 (A : Type) (a : A) : Type := Nat\n\
       def t : ty2 Nat Nat.zero := Nat.zero\n";
    check_ok "a body passing the parameter to a family explicitly"
      "def ty3 (A : Type) (a : A) : Type := Eq Nat Nat.zero Nat.zero\n\
       def t : ty3 Nat Nat.zero := Eq.refl Nat.zero\n";
    check_ok "a body whose family index is the parameter"
      "def ty4 (A : Type) (a : A) : Type := Eq A a a\n\
       def t : ty4 Nat Nat.zero := Eq.refl Nat.zero\n";
    check_ok "refl proves 0 = 0" "def t : 0 = 0 := Eq.refl Nat.zero\n";
     check_ok "refl proves 0 = 0 with a numeral" "def y : 0 = 0 := Eq.refl 0\n";
    check_value "addition evaluates"
      (prelude ^ "def y : Nat := 5 + 6\n")
      "y"
      "Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ Nat.zero))))))))))";
  ]

let families =
  [
    check_ok "direct, 1 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F1 {A : Type} : (x0 : A) -> Type\n\
       | F1c : (x0 : A) -> F1 A x0\n\
       def t : F1 Nat.zero := F1.F1c Nat.zero\n";
    check_ok "alias, implicit param, 1 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F1 {A : Type} : (x0 : A) -> Type\n\
       | F1c : (x0 : A) -> F1 A x0\n\
       def al1 {A : Type} (x0 : A) : Type := F1 x0\n\
       def t : al1 Nat.zero := F1.F1c Nat.zero\n";
    check_ok "alias, explicit param, 1 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F1 {A : Type} : (x0 : A) -> Type\n\
       | F1c : (x0 : A) -> F1 A x0\n\
       def alx1 (A : Type) (x0 : A) : Type := F1 x0\n\
       def t : alx1 Nat.zero := F1.F1c Nat.zero\n";
    check_ok "direct, 2 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F2 {A : Type} : (x0 : A) -> (x1 : A) -> Type\n\
       | F2c : (x0 : A) -> (x1 : A) -> F2 A x0 x1\n\
       def t : F2 Nat.zero Nat.zero := F2.F2c Nat.zero Nat.zero\n";
    check_ok "alias, implicit param, 2 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F2 {A : Type} : (x0 : A) -> (x1 : A) -> Type\n\
       | F2c : (x0 : A) -> (x1 : A) -> F2 A x0 x1\n\
       def al2 {A : Type} (x0 : A) (x1 : A) : Type := F2 x0 x1\n\
       def t : al2 Nat.zero Nat.zero := F2.F2c Nat.zero Nat.zero\n";
    check_ok "alias, explicit param, 2 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F2 {A : Type} : (x0 : A) -> (x1 : A) -> Type\n\
       | F2c : (x0 : A) -> (x1 : A) -> F2 A x0 x1\n\
       def alx2 (A : Type) (x0 : A) (x1 : A) : Type := F2 x0 x1\n\
       def t : alx2 Nat.zero Nat.zero := F2.F2c Nat.zero Nat.zero\n";
    check_ok "direct, 3 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F3 {A : Type} : (x0 : A) -> (x1 : A) -> (x2 : A) -> Type\n\
       | F3c : (x0 : A) -> (x1 : A) -> (x2 : A) -> F3 A x0 x1 x2\n\
       def t : F3 Nat.zero Nat.zero Nat.zero := F3.F3c Nat.zero Nat.zero \
       Nat.zero\n";
    check_ok "alias, implicit param, 3 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F3 {A : Type} : (x0 : A) -> (x1 : A) -> (x2 : A) -> Type\n\
       | F3c : (x0 : A) -> (x1 : A) -> (x2 : A) -> F3 A x0 x1 x2\n\
       def al3 {A : Type} (x0 : A) (x1 : A) (x2 : A) : Type := F3 x0 x1 x2\n\
       def t : al3 Nat.zero Nat.zero Nat.zero := F3.F3c Nat.zero Nat.zero \
       Nat.zero\n";
    check_ok "alias, explicit param, 3 indices"
      "def Nat : Type | zero : Nat | succ (n : Nat) : Nat\n\
       def F3 {A : Type} : (x0 : A) -> (x1 : A) -> (x2 : A) -> Type\n\
       | F3c : (x0 : A) -> (x1 : A) -> (x2 : A) -> F3 A x0 x1 x2\n\
       def alx3 (A : Type) (x0 : A) (x1 : A) (x2 : A) : Type := F3 x0 x1 x2\n\
       def t : alx3 Nat.zero Nat.zero Nat.zero := F3.F3c Nat.zero Nat.zero \
       Nat.zero\n";
  ]

let caseexpr =
  [
    check_ok "case in expression position"
      (nat_prelude
      ^ "def f (n : Nat) : Nat := case n | Nat.zero => Nat.zero | Nat.succ k \
         => k\n");
    check_ok "nested let in a clause"
      (nat_prelude ^ "def f (n : Nat) : Nat | zero => Nat.zero | succ k => k\n");
  ]

let soundness =
  [
    check_ok "a false statement may be stated" "def t : Type := 0 = 1\n";
    check_diagnostic "refl cannot prove 0 = 1"
      "def bad : 0 = 1 := Eq.refl Nat.zero\n" "type mismatch";
    check_diagnostic "a bare refl cannot prove 0 = 1"
      "def bad : 0 = 1 := Eq.refl\n" "type mismatch";
  ]

let puzzle =
  [
    check_ok "add_zero opens with ="
      "def add_zero (n : Nat) : Nat.zero + n = n :=\n  _\n";
    check_ok "mul_zero opens with ="
      "def mul_zero (n : Nat) : Nat.zero * n = Nat.zero :=\n  _\n";
    check_ok "add_zero is solved"
      (prelude
      ^ "def add_zero (n : Nat) : Nat.zero + n = n :=\n\
        \  case n\n\
        \  | Nat.zero => Eq.refl Nat.zero\n\
        \  | Nat.succ n => Eq.refl Nat.zero\n");
    check_ok "mul_zero is solved"
      (prelude
      ^ "def mul_zero (n : Nat) : Nat.zero * n = Nat.zero :=\n\
        \  case n\n\
        \  | Nat.zero => Eq.refl Nat.zero\n\
        \  | Nat.succ n => Eq.refl Nat.zero\n");
  ]

let exhaustiveness =
  [
    check_diagnostic "non-exhaustive case: missing succ"
      (nat_prelude ^ "def is_zero (n : Nat) | zero => Type0\n")
      "case expression is not exhaustive";
    check_ok "wildcard case is exhaustive"
      (nat_prelude ^ "def is_zero (n : Nat) | zero => Type0 | _ => Type0\n");
    check_ok "wildcard-only case is exhaustive"
      (nat_prelude ^ "def f (n : Nat) | _ => Type0\n");
  ]

let tests =
  basics
  @ numerals
  @ errors
  @ holes
  @ case_analysis
  @ universes
  @ parametrised
  @ constructors
  @ shared_prelude
  @ reported
  @ mechanism
  @ families
  @ caseexpr
  @ soundness
  @ puzzle
  @ exhaustiveness
