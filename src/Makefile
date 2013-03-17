
.PHONY: build clean install

DESTDIR=/usr/local
MODE=byte

build:
		ocamlbuild -j 0 sqlgg.$(MODE)

test:
		./sqlgg.$(MODE) -test

install:
		install _build/sqlgg.$(MODE) $(DESTDIR)/bin/sqlgg

clean:
		ocamlbuild -clean

