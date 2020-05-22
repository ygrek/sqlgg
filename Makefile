.PHONY: build doc test clean top

build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest
	./run_test

top:
	dune utop

VERSION:=$(shell date +%Y%m%d)
NAME=sqlgg-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) $(VERSION)
	git archive --prefix=$(NAME)/ $(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz -o $(NAME).tar.gz.asc
