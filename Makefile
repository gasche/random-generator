all: build

.PHONY: build
build:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: docs
doc:
	dune build @doc

.PHONY: install
install:
	dune build @install
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall
