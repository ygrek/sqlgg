
.PHONY: build clean install

DESTDIR=/usr/local
OCAMLBUILD=ocamlbuild -j 0
MODE=byte

build:
		$(OCAMLBUILD) sqlgg.$(MODE)

top:
		$(OCAMLBUILD) sqlgg.top

test:
		./sqlgg.$(MODE) -test

install:
		install _build/sqlgg.$(MODE) $(DESTDIR)/bin/sqlgg

clean:
		ocamlbuild -clean

