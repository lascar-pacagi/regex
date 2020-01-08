# Regex

This repository contains the code to illustrate regular expressions and automata for the lexical analysis part of the `MiniJava` transpiler, which is documented on
[www.mrcoder.org](https://www.mrcoder.org/en/compiler/minijava/).

To use the code in the ocaml interpreter do the following.

```ocaml
utop # #use "regex.ml";;
```

You can create a regular expression with the following code.

```ocaml
utop # let re = RE.regex_from_string "0*(100*)*1?";;
```

You can now create a non-deterministic finite automata equivalent to this regular expression with

```ocaml
utop # let nfa = NFA.init re;;
```

And now, you can test if a string is matched by the regular expression with

```ocaml
utop # NFA.full_match nfa "101010";;
- : bool = true
```

If you want to launch the test suite, you will need [dune](https://github.com/ocaml/dune), you can install it with [opam](https://opam.ocaml.org/)
by executing the following command.

```bash
opam install dune
```

You can now launch the test suite with

```bash
dune exec ./test_re.exe
```
