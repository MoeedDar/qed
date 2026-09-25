# qed

**qed** is a dependently typed, functional and theorem proving language, based on Martin-Löf's type theory.

## Quick start

**qed** currently does not maintain a binary release. The language is available to try in the playground [here](https://qed.moeed-dar.workers.dev/). Alternatively, you can build **qed** from source, see [Building](#building).

## Example

```qed
def Nat : Type
| zero : Nat
| succ : Nat -> Nat

def Eq {A : Type} : A -> A -> Bool
| refl : (a : A) -> Eq a a

def add (a : Nat) (b : Nat)
| zero a => b
| succ => succ (add a b)

def (10 + 11) a b := add a b

def (5 = 5) a b := Eq a b

-- Show that n + 0 = n for all n
def add_zero (n : Nat) : n + 0 = n :=
| zero => Eq.refl n
| succ n => Eq.refl (Nat.succ n)
```

## Building

Requirements:

- [OCaml](https://ocaml.org/)
- [Dune](https://dune.build/)
- [Bun](https://bun.sh/) (for running the playground)

To build **qed** from source:

```bash
dune build # build all libraries and executables
dune build web/public/lsp_js.js # build the JavaScript LSP
```

To run the playground locally:

```bash
cd web
bun install
bun run dev
```

## Status

**qed** is currently in the early stages of development. The language is not yet stable, and the API may change.

## Contributing

Contributions are welcome. If you would like to contribute, please open an issue or submit a pull request.
