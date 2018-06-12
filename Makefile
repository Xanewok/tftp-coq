build:
	coqc server.v
	ocamlbuild main.native

run: build
	./main.native

clean:
	ocamlbuild -clean
	rm -f *.o *.vo *.glob .*.aux
