## Process this file with automake to produce Makefile.in

SUBDIRS = a-sync lib docs

snapshot: dist
	mv @PACKAGE@-@VERSION@.tar.gz @PACKAGE@-`date +%y%m%d`.tar.gz 

EXTRA_DIST = m4/*.m4 autogen.sh config.rpath compile TODO \
             docs/coroutines docs/event-loop docs/*.scm

ACLOCAL_AMFLAGS = -I m4
