## Process this file with automake to produce Makefile.in

SOURCES = monotonic-time.scm coroutines.scm event-loop.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go

.scm.go:
	$(top_builddir)/a-sync/a-sync-compile.sh "$@" "$<"

moduledir = $(GUILE_SITE)/a-sync
module_DATA = $(SOURCES) $(GOBJECTS)

EXTRA_DIST = $(SOURCES) a-sync-compile.sh
