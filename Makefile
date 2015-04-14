# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

.PHONY: top
top:
		$(SETUP) -build src/sqlgg.top

VERSION=$(shell oasis query version)
NAME=sqlgg-$(VERSION)

.PHONY: release
release:
	git checkout -b release
	oasis setup
	git commit -m "oasis setup" setup.ml _tags
	git tag -a -m $(VERSION) $(VERSION)
	git archive --prefix=$(NAME)/ $(VERSION) | gzip > $(NAME).tar.gz
	git checkout master
	git branch -d release
	gpg -a -b $(NAME).tar.gz

.PHONY: oasis
# partially dynamic setup
oasis:
	oasis setup
	git checkout setup.ml _tags
	rm -f lib/*.mlpack lib/*.mllib lib/*.mldylib lib/META
