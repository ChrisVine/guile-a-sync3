## Process this file with automake to produce Makefile.in

SUBDIRS = lib a-sync docs tests

.PHONY: docs
docs:
	$(MAKE) -C docs docs

.PHONY: test
test:
	$(MAKE) -C tests test

snapshot: dist
	mv @PACKAGE@-@VERSION@.tar.gz @PACKAGE@-`date +%y%m%d`.tar.gz 

EXTRA_DIST = m4/*.m4 autogen.sh config.rpath compile TODO

ACLOCAL_AMFLAGS = -I m4
