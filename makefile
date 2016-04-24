all : clean main

clean : 
	@rm -rf _build
	@rm -f main.byte

main : main.ml
	ocamlbuild main.byte

# all: main

# main: main.ml
# 	ocamlc graphics.cma main.ml -o main

# clean:
# 	rm -rf _build *.byte