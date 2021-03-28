MODULES=board author main gui
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)
	rm *.coverage

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -plugin-tag 'package(bisect_ppx-ocamlbuild)' -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
	bisect-ppx-report html
	rm *.coverage

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
	rm *.coverage


check:
	@bash check.sh
	rm *.coverage

finalcheck:
	@bash check.sh final
	rm *.coverage

zip:
	rm *.coverage
	zip final.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile

clean:
	ocamlbuild -clean
