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

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-a-sync@GUILE_VER@.pc

EXTRA_DIST = m4/*.m4 autogen.sh config.rpath compile README.md guile-a-sync2.pc.in guile-a-sync2-30.pc.in

ACLOCAL_AMFLAGS = -I m4
