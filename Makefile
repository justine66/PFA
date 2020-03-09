all: main.native

%.native: *.ml
		ocamlbuild -use-ocamlfind -pkg graphics -pkg str $@
		
.PHONY: clean

clean: 
		ocamlbuild -clean
