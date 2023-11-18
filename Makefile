
include config

noarguments:
	@echo "Consult README first."
	@echo "Targets are:"
	@echo "  make world"
	@echo "  make install"
	@echo "  make install-manpage"
	@echo "  make clean"

world: build-src


build-src:
	(cd src; make)
	(cd launch; make)
	(cd lib; make)
	@echo === Compilation completed ===

install:
	mkdir -p $(BINDIR)
	mkdir -p $(INSTALLDIR)
	(cd src; make install)
	(cd launch; make install)
	(cd lib; make install)
	@echo === Installation done ===

install-manpage:
	mkdir -p $(MANDIR)/man1
	cp man/caml2csl.1 $(MANDIR)/man1/caml2csl.1

clean:
	(cd src; make clean)
	(cd launch; make clean)
	(cd lib; make clean)
	(cd test/toto; make clean)
