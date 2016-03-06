## Process this file with automake to produce Makefile.in

.PHONY: test
test:
	for files in `ls *.scm`; do $(top_builddir)/tests/run-test.sh $$files; done

EXTRA_DIST = *.scm run-test.sh
