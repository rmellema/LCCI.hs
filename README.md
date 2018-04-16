# LCCI
A framework for evaluating and working with LCCI formulas and models. This was
written to be used together with my Masters Thesis.

# Building
In order to build the library, you need the program `stack`, which takes care of
building everything and installing the requirements. `stack` can be downloaded
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

# Usage
The library can be build and loaded by executing:
```
stack ghci
```
This will drop you in a Haskell repl with all the packages loaded. In this mode,
one can define formulas and models, and use the program to interpret the
formulas in the models. For an example of defining models see the file
`src/Hex.hs`, while the definition for formulas can be found in `src/Syntax.hs`.
Interpreting of formulas in models is done by the function `supports :: Model ->
State -> Formula -> Bool`, as defined in `src/Evaluation.hs`.
