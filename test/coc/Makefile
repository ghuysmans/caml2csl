
include ../../config

OCAMLLIB=-I $(INSTALLDIR)
CAMLC=ocamlc -c

coc: inf_opt.cmo gram.cmo
	ocamlc $(OCAMLLIB) -o coc caml__csl.cmo inf_opt.cmo gram.cmo

clean:
	rm -f *.cm* *.ml *.mli *.zc coc

.SUFFIXES: .mli .ml .cmo .cmi .caml .camli

.camli.mli:
	caml2csl $(CONVLIB) $<

.caml.ml:
	caml2csl $(CONVLIB) $<

.mli.cmi:
	$(CAMLC) $(OCAMLLIB) $<

.ml.cmo:
	$(CAMLC) $(OCAMLLIB) $<


inf_opt.cmo: inf_opt.ml
gram.cmo: gram.ml
