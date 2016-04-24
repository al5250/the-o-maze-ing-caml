all : clean main

clean : 
	@rm -rf _build
	@rm -f main.byte

main : 
	ocamlbuild graphics.cma main.ml

# all: main

# main: main.ml
# 	ocamlc graphics.cma main.ml -o main

# clean:
# 	rm -rf _build *.byte