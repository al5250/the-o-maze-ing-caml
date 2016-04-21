all: main

moogle: main.ml
	ocamlbuild main.byte

clean:
	rm -rf _build *.byte