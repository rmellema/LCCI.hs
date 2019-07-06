# LCCI
A framework for evaluating and working with LCCI formulas and models. This was
written to be used together with my Masters Thesis.

# Building
In order to build the library, you need the program `stack`, which takes care of
building everything and installing the requirements. `stack` can be downloaded
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

# Documentation
The documenation for this project can be found in the `docs` folder or 
[online](https://rmellema.github.io/LCCI.hs/index.html). There is also a tutorial, which you 
can read [here](https://github.com/rmellema/LCCI.hs/blob/master/src/LCCI/Examples/hexa.pdf).

# Usage
The library can be build and loaded by executing the following command from this directory:
```
stack ghci
```
This will drop you in a Haskell repl with all the packages loaded. In this mode,
one can define formulas and models, and use the program to interpret the
formulas in the models. For an example of defining models see the file
`src/LCCI/Examples/Hexa.hs`, while the definition for formulas can be found in `src/LCCI/Syntax.hs`.
Interpreting of formulas in models is done by the function `supports :: Model ->
State -> Formula -> Bool`, as defined in `src/LCCI/Evaluation.hs`.

I you want to use this library in a different project managed by `stack`, then you can do that by 
adding this package to your `extra-deps` in the following manner:
```
extra-deps:
- github: rmellema/LCCI.hs
  commit: <commit>
```
Where `<commit>` is the hash of the commit that you want to use.
