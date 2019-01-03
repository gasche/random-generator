all: build

.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: docs
doc:
	ocamlbuild src/random_generator.docdir/index.html

.PHONY: install
install:
	dune build @install
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall
