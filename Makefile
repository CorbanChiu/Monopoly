MODULES=board author main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -plugin-tag 'package(bisect_ppx-ocamlbuild)' -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
	bisect-ppx-report html
	rm *.coverage

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	zip final.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private adventure.zip
