## Process this file with automake to produce Makefile.in

SOURCES = monotonic-time.scm coroutines.scm event-loop.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go

.scm.go:
	guild compile -L .. -o "$@" "$<"

moduledir = $(GUILE_SITE)/a-sync
module_DATA = $(SOURCES) $(GOBJECTS)

EXTRA_DIST = $(SOURCES)
