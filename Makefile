MODULES=board author main gui player cards state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
DEBUG=$(OCAMLBUILD) -tag debug $(MAIN) && ./$(MAIN)

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) $(TEST) && ./$(TEST) -runner sequential

debug:
	$(DEBUG) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	zip final.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile INSTALL.md

clean:
	ocamlbuild -clean

docs: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d _doc.public $(MLIS)
