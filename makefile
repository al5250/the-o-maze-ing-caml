all: main

main: main.ml
	ocamlbuild main.byte

clean:
	rm -rf _build *.byte