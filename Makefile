build: coq
	ocamlbuild -I core -pkg unix -tag thread main.native

.PHONY: coq

coq:
	$(MAKE) -C core

run: build
	./main.native

clean:
	ocamlbuild -clean
	rm -f *.o *.vo *.glob .*.aux
	$(MAKE) -C core clean
