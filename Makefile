all: affichage.native

%.native: *.ml
		ocamlbuild -use-ocamlfind -pkg graphics $@
		
.PHONY: clean

clean: 
		ocamlbuild -clean
