TARGETS= generator.cma generator.cmxa generator.cmxs

all:
	ocamlbuild $(TARGETS)

clean:
	ocamlbuild -clean

doc:
	ocamlbuild generator.docdir/index.html

TO_INSTALL=META $(addprefix _build/, $(TARGETS) generator.a generator.cmi)

install:
	ocamlfind install generator $(TO_INSTALL)

uninstall:
	ocamlfind remove generator

.PHONY: all clean doc install uninstall
