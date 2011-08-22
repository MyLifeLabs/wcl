wcl: wcl.ml
	ocamlopt -o wcl -annot unix.cmxa wcl.ml

.PHONY: clean
clean:
	rm -f *.cm[iox] *.o *.annot *~ wcl
