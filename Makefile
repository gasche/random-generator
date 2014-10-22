TARGETS= random_generator.cma random_generator.cmxa random_generator.cmxs

all:
	ocamlbuild $(TARGETS)

clean:
	ocamlbuild -clean

doc:
	ocamlbuild random_generator.docdir/index.html

TO_INSTALL=META $(addprefix _build/, $(TARGETS) random_generator.a random_generator.cmi)

install:
	ocamlfind install random-generator $(TO_INSTALL)

uninstall:
	ocamlfind remove random-generator

.PHONY: all clean doc install uninstall
