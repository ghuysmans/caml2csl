
include ../config

# TODO: graphics unix num arith_status str

TRANSLATOR=camlrun ../src/translator

ZCFILES= arg.zc baltree.zc filename.zc format.zc gc.zc genlex.zc \
         hashtbl.zc lexing.zc map.zc parsing.zc printexc.zc printf.zc \
         queue.zc random.zc set.zc sort.zc sys.zc \
         unix.zc graphics.zc 

OCAMLFILES= caml__csl.cmi caml__csl.cmo baltree.cmi baltree.cmo \
            caml_map.cmi caml_map.cmo caml_set.cmi caml_set.cmo

all: builtins.zlc $(ZCFILES) $(OCAMLFILES)

install: builtins.zlc
	cp builtins.zlc $(LIBNAME)
	cp $(ZCFILES) $(OCAMLFILES) $(INSTALLDIR)
	rm -f genlex.cm*

clean:
	rm -f *.zlc *.zc *.cm*


.SUFFIXES: .mlca .zlc .mlc .zc .ml .mli .cmi .cmo

.mlca.zlc:
	$(TRANSLATOR) -o $@ -c $<

.mlc.zc:
	$(TRANSLATOR) -clib ./builtins.zlc $<

.mli.cmi:
	ocamlc -c $<

.ml.cmo:
	ocamlc -c $<
