## Process this file with automake to produce Makefile.in

SUBDIRS = g-golf guile-gi

SOURCES = monotonic-time.scm coroutines.scm event-loop.scm compose.scm await-ports.scm meeting.scm thread-pool.scm

moduledir = $(GUILE30_SITEDIR)/a-sync
module_DATA = $(SOURCES) gnome-glib.scm g-golf.scm guile-gi.scm

if COMPILE_TO_BYTECODE
GOBJECTS = $(SOURCES:%.scm=%.go)

gobjectsdir = $(GUILE30_OBJDIR)/a-sync
gobjects_DATA = $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES: .scm .go
.scm.go:
	$(top_builddir)/a-sync/a-sync-compile.sh "$<" "$@"

# we state dependencies here to handle parallel compilation better
monotonic-time.go: monotonic-time.scm
coroutines.go: coroutines.scm
event-loop.go: event-loop.scm monotonic-time.go coroutines.go
compose.go: compose.scm coroutines.go
await-ports.go: await-ports.scm event-loop.go
meeting.go: meeting.scm event-loop.go
thread-pool.go: thread-pool.scm coroutines.go event-loop.go

# make sure that timestamps on object files are after that of source
# files
install-data-hook:
	for files in $(GOBJECTS); do touch -c $(gobjectsdir)/$$files; done
endif

EXTRA_DIST = $(SOURCES) gnome-glib.scm g-golf.scm guile-gi.scm a-sync-compile.sh
