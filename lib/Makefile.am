## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libguile-a-sync2-0.la

libguile_a_sync2_0_la_SOURCES = monotonic_time.c

AM_CFLAGS = @GUILE_CFLAGS@ -I..

libguile_a_sync2_0_la_LIBADD = @GUILE_LIBS@

libguile_a_sync2_0_la_LDFLAGS = -shared -version-info @GUILE_A_SYNC2_SO_VERSION@
