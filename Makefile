wcl: wcl.ml
	ocamlopt -o wcl -annot unix.cmxa wcl.ml

ifndef PREFIX
PREFIX = $(HOME)
endif

ifndef BINDIR
BINDIR = $(PREFIX)/bin
endif

.PHONY: install uninstall
install:
	@if [ -f $(BINDIR)/wcl ]; \
	  then echo "Error: run '$(MAKE) uninstall' first."; \
	  else \
	    echo "Installing wcl into $(BINDIR)"; \
	    cp wcl $(BINDIR); \
	fi

uninstall:
	rm $(BINDIR)/wcl

.PHONY: clean
clean:
	rm -f *.cm[iox] *.o *.annot *~ wcl
