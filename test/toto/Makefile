
include ../../config

OCAMLLIB=-I $(INSTALLDIR)
CAMLC=ocamlc -c

toto: toto.cmo

clean:
	rm -f *.cm* *.ml *.mli *.zc

.SUFFIXES: .mli .ml .cmo .cmi .caml .camli

.camli.mli:
	caml2csl $(CONVLIB) $<

.caml.ml:
	caml2csl $(CONVLIB) $<

.mli.cmi:
	$(CAMLC) $(OCAMLLIB) $<

.ml.cmo:
	$(CAMLC) $(OCAMLLIB) $<



toto.cmo: toto.ml toto.cmi
toto.cmi: toto.mli tata.cmi
tata.cmi: tata.mli

## dependencies to add

toto.ml: toto.mli tata.mli
toto.mli: tata.mli
