
include ../config

SCRIPTS=caml2csl.scr renamer.scr

all: $(SCRIPTS)

install: $(SCRIPTS)
	cp caml2csl.scr $(BINDIR)/caml2csl
	cp renamer.scr $(BINDIR)/renamer

clean:
	rm -f *.scr


.SUFFIXES: .tpl .scr

.tpl.scr:
	sed -e "s|INSTALLDIR|$(INSTALLDIR)|" -e "s|LIBNAME|$(LIBNAME)|" $< > $@
	@chmod a+x $@
