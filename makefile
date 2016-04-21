all: moogle

moogle: moogle.ml
	ocamlbuild moogle.byte

clean:
	rm -rf _build *.byte