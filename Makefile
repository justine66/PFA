all: affichage.native

%.native: *.ml
		ocamlbuild -use-ocamlfind -pkgs 'graphics,str' $@
		
.PHONY: clean

clean: 
		ocamlbuild -clean
