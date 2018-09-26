lambda-rays
==========

New Haskell project using stack template `tasty-travis`.

Please read file `tutorial.md` for first steps in using the template.

*Notes*


[x] Use vmap to avoid code repetition
[ ] Refactor internal implementation to use a list, which will allow more expresiveness in algorithms 


In this branch (generalization-attempt) I tried to make *Quadruple* not be bound to *Double*, but to use any type conforming to *Floating* (that is, *Double* or *Float*). This triggered a series of warnings about how the compiler was defaulting to using *Double*, but warnings are treated as errors and thus tests can't be built and run. Disabling `-WError` makes tests build and pass. See https://www.haskell.org/tutorial/numbers.html#sect10.4 https://www.haskell.org/onlinereport/decls.html#default-decls

If we force-cast all numbers used as function parameters to Double (as in the first test), or the first occurence in each `shouldBe`, then no warnings are raised. However, that much verbosity makes the code cumbersome to write and read. Therefore, we selectively disable those warnings with `{-# OPTIONS_GHC -fno-warn-type-defaults #-}`