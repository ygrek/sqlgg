
.PHONY: build clean install release

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

NAME=sqlgg-$(VERSION)

release:
	[ -n "$(VERSION)" ] || (echo specify VERSION; exit 1)
	git tag -a -m $(VERSION) $(VERSION)
	(cd .. && \
		git archive --prefix=$(NAME)/ -o $(NAME).tar $(VERSION) && \
		mkdir -p $(NAME)/src && echo $(VERSION) > $(NAME)/src/version.id && \
		tar rf $(NAME).tar $(NAME)/src/version.id && \
		rm $(NAME)/src/version.id && rmdir $(NAME)/src && rmdir $(NAME) && \
		gzip $(NAME).tar && \
		gpg -a -b $(NAME).tar.gz)
