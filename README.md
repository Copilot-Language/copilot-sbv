[![Build Status](https://travis-ci.org/Copilot-Language/copilot-sbv.svg?branch=master)](https://travis-ci.org/Copilot-Language/copilot-sbv)

Overview
========
[copilot-sbv](http://hackage.haskell.org/package/copilot-sbv) Another back-end
that translates to [SBV](http://hackage.haskell.org/package/sbv), using its code
generator to generate hard real-time C code as well, with ACSL contracts, 
for the value-analysis plugin and compiling the code with CompCert. 

Copilot is a stream (i.e., infinite lists) domain-specific language (DSL) in
Haskell that compiles into embedded C.  Copilot is similar in spirit to
languages like Lustre.  Copilot contains an interpreter, multiple back-end
compilers, and other verification tools.

Examples
=========
Please see the files under the Examples directory in the
[Copilot](http://hackage.haskell.org/package/copilot) for a number of examples
showing the syntax, use of libraries, and use of the interpreter and back-ends.
The examples is the best way to start.

Installation
============
The Copilot library is cabalized. Assuming you have cabal and the GHC compiler
installed (the [Haskell Platform](http://hackage.haskell.org/platform/) is the
easiest way to obtain these), it should merely be a matter of running 
     
         cabal install copilot-sbv

However, we strongly recommend you install Copilot, which installs copilot-sbv
and other packages automatically.  Execute

         cabal install copilot

Dependencies
=============
copilot-sbv depends on the latest [SBV](http://hackage.haskell.org/package/sbv) library
to generate hard real-time C code. It is recommanded to obtain it from the git 
[repository](https://github.com/LeventErkok/sbv/), and compile it yourself (ghc 7.10 needed). 

For the [ACSL](http://frama-c.com/acsl.html) , you need an up-to-date frama-c (Sodium), 
and the value analysis plugin that goes with. Run to verify :
	
         make fval

For compiling it with [CompCert](http://compcert.inria.fr/),
you need to install it, install the Standard [C library](http://compcert.inria.fr/man/manual002.html) for it,
wait until SBV allows you to change the compiler (or do it manually by changing the makefile generated), and
run the following command :

         make all

There is also a [splint](http://www.splint.org/) support for the project. You need to install splint and run :

         make splint

More about ACSL
==============
copilot-sbv generates automatic ACSL contracts for all functions and for global variables (in the form of
global invariants, which needs an up to date value analysis plugin for frama-c). The most important part of 
generating contracts is transforming an expression about queues (such as drop 1 s1 + 3) into a ACSL contract.
This is done by a pretty printer, which translates each construct of the language into its ACSL equivalent.
However, some features are not implemented in the plugin yet, but are specified by ACSL (logical predicates ...).
This may result in a verification status "unknown" for predicates containing these expressions. Some are not specified
at all (asinh, ...), hence it compiles in a predicate that has to be user defined when implemented. 

Casts are badly supported (unknown status), hence it is recommanded to avoid them. Remember, your computer has more than
8kb of memory since 1980, use it !

Floats are very badly supported by SBV (only constant floats can be operands of floating functions).
Some issues are beeing fixed about that. 

Resources
=========
[copilot-sbv](http://hackage.haskell.org/package/copilot-sbv) is available on
Hackage.

**Sources** for each package are available on Github as well.  Just go to
[Github](github.com) and search for the package of interest.  Feel free to fork!

Copyright, License
==================
Copilot is distributed with the BSD3 license. The license file contains the
[BSD3](http://en.wikipedia.org/wiki/BSD_licenses) verbiage.

Thanks
======
We are grateful for NASA Contract NNL08AD13T to [Galois,
Inc](http://corp.galois.com/) and the [National Institute of
Aerospace](http://www.nianet.org/), which partially supported this work.
