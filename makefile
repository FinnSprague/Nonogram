.PHONY: build finn
build: main.ml
	ocamlopt -o main main.ml
	./main

finn: finn.ml
	ocamlopt -o finn finn.ml
	./finn
