
include ../config


CAMLC=camlc -c
CAMLLINK=camlc
CAMLYACC=camlyacc -s

COMPFLAGS=-W
LINKFLAGS=-g -O fast

OBJS= location.zo globals.zo syntax.zo modules.zo print.zo \
      lexer.zo par_aux.zo parser.zo emit.zo conv.zo changes.zo enter.zo \
      parse_mlc.zo compiler.zo

GENSOURCES=lexer.ml parser.ml parser.mli


all: translator renamer

install:
	cp translator $(INSTALLDIR)/caml2csl
	cp renamer $(INSTALLDIR)

translator: translator.zo main.zo
	$(CAMLLINK) $(LINKFLAGS) -o translator translator.zo main.zo

translator.zo: $(OBJS)
	camllibr -o translator.zo $(OBJS)

renamer:
	$(CAMLLINK) $(LINKFLAGS) -o renamer renamer.ml


lexer.mll: lexer.mlp
	$(CPP) lexer.mlp > lexer.mll

lexer.ml: lexer.mll
	camllex lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

clean:
	rm -f *.zi *.zo
	rm -f $(GENSOURCES) lexer.mll translator renamer


.SUFFIXES : .mli .ml .zi .zo .mlp


.mli.zi:
	camlc -c $(COMPFLAGS) $<

.ml.zo:
	camlc -c $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@


depend: $(GENSOURCES)
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         camldep *.mli *.ml) > Makefile
	rm Makefile.bak


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
lexer.zi: parser.zi 
parser.zi: location.zo syntax.zo 
changes.zo: lexer.zi globals.zo conv.zo syntax.zo location.zo modules.zo \
    print.zo emit.zo 
compiler.zo: lexer.zi globals.zo parse_mlc.zo syntax.zo location.zo \
    modules.zo changes.zo parser.zi print.zo enter.zo emit.zo 
conv.zo: modules.zo globals.zo print.zo syntax.zo 
emit.zo: location.zo globals.zo print.zo syntax.zo 
enter.zo: changes.zo modules.zo globals.zo conv.zo emit.zo syntax.zo 
lexer.zo: lexer.zi location.zo parser.zi 
load.zo: lexer.zi globals.zo syntax.zo parser.zi print.zo emit.zo conv.zo \
    parse_mlc.zo modules.zo location.zo changes.zo compiler.zo enter.zo 
main.zo: lexer.zi globals.zo parse_mlc.zo modules.zo compiler.zo 
modules.zo: globals.zo syntax.zo 
par_aux.zo: modules.zo location.zo globals.zo syntax.zo 
parse_mlc.zo: modules.zo globals.zo enter.zo conv.zo syntax.zo 
parser.zo: parser.zi par_aux.zo lexer.zi location.zo globals.zo syntax.zo 
print.zo: modules.zo location.zo syntax.zo 
syntax.zo: location.zo globals.zo 
tools.zo: modules.zo globals.zo syntax.zo 
view.zo: modules.zo globals.zo syntax.zo 
