## Process this file with automake to produce Makefile.in

SOURCES = monotonic-time.scm coroutines.scm event-loop.scm await-ports.scm compose.scm

moduledir = $(GUILE22_SITEDIR)/a-sync
module_DATA = $(SOURCES) gnome-glib.scm

# FIXME22 - prior compilation to bytecode requires a more final
# guile-2.2 release
#if COMPILE_TO_BYTECODE
#GOBJECTS = $(SOURCES:%.scm=%.go)

#CLEANFILES = $(GOBJECTS)

#SUFFIXES = .scm .go

#.scm.go:
#	$(top_builddir)/a-sync/a-sync-compile.sh "$@" "$<"

#module_DATA += $(GOBJECTS)
#endif

EXTRA_DIST = $(SOURCES) gnome-glib.scm a-sync-compile.sh
