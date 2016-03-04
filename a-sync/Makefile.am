## Process this file with automake to produce Makefile.in

moduledir = $(GUILE_SITE)/a-sync
module_DATA = $(SOURCES)

SOURCES = monotonic-time.scm coroutines.scm event-loop.scm

if COMPILE_TO_BYTECODE
GOBJECTS = $(SOURCES:%.scm=%.go)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go

.scm.go:
	$(top_builddir)/a-sync/a-sync-compile.sh "$@" "$<"

module_DATA += $(GOBJECTS)
endif

EXTRA_DIST = $(SOURCES) a-sync-compile.sh
