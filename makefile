.PHONY: build
build:
	ocamlopt -o main main.ml
	./main
