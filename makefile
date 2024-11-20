.PHONY: build finn new_main
build: main.ml
	ocamlopt -o main main.ml
	./main

finn: finn.ml
	ocamlopt -o finn finn.ml
	./finn
new_main: new_main.ml
	ocamlopt -o new_main new_main.ml
	./new_main
