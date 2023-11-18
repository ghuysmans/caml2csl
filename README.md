_(adapted from its 1996 announcement on the Caml list)_

A translator from Caml Light 0.7 to Caml Special Light.

It doesn't work like the convert tool: it was designed on the base of the
camlc compiler. So, it is able to analyze the structure of each sentence.
This allows the renaming of every identifier to fit the CSL conventions,
and prevents name clashes, variable captures.
It translates the constructions that changed, like `where`, infixes,
character constants, ...
_caml2csl_ also summarizes what remains to do by hand after the automatic
translation.

Bruno Barras

PS: This program has been used to translate the system Coq (about 50000
lines). It took less than a week to get a runable system. I hope this
will encourage you to use it!
