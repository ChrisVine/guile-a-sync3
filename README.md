Overview
--------

This library supports guile-2.2 >= 2.1.3 and guile-3.0 >= 2.9.1.  It
provides a thread safe event loop (event-loop.scm) with support for
watches on ports/file descriptors and timeouts, and which permits
events to be posted by other tasks.  This includes tasks running on
other threads.

It also provides a coroutines interface (coroutines.scm) which
provides await semantics on such events, so as to avoid inversion of
control (aka "callback hell"), and provision for using these in
conjunction with guile-2.2/3.0's suspendable ports (await-ports.scm).

See the documentation mentioned below for further details, and the
docs/example.scm and docs/example-glib.scm files.

A separate guile-a-sync library is available for guile-2.0 here:
https://github.com/ChrisVine/guile-a-sync .  This library and
guile-a-sync for guile-2.0 are parallel installable.

Installation
------------

When first run from git, or a tarball obtained from github, it is
necessary to set up autotools.  This can be done with:

  ./autogen.sh --prefix=/usr

or on a 64-bit system, probably:

  ./autogen.sh --prefix=/usr --libdir=/usr/lib64

This generates a configure script and installs libtool, and will then
run the configure script.  By default the configure script will look
first for guile-3.0 >= 2.9.1 and if that is not found then for
guile-2.2 >= 2.1.3.  This can be overriden by passing autogen.sh
the --with-guile=2.2 or --with-guile=3.0 options to choose guile-2.2
or guile-3.0 explicitly.

Subsequent configuration can be done just with

  ./configure --prefix=/usr

and

  ./configure --prefix=/usr --libdir=/usr/lib64

respectively, adding the --with-guile option as necessary.

The library as compiled for guile-2.2 can be parallel installed with
the library as compiled for guile-3.0.  The pkg-config file produced
when compiled against guile-2.2 is guile-a-sync2.pc, and when compiled
against guile-3.0 is guile-a-sync2-30.pc, so the pkg-config files can
be separately interrogated.

On a 64 bit system you may also need to include -fPIC in your CFLAGS
options if libtool doesn't do that for you (libtool normally does when
necessary).

After compiling, install with 'make install' as root.

By default, the scheme files provided by this library will be
pre-compiled to guile bytecode and installed with the scheme files in
guile's object file directory.  If that behaviour is not wanted (say,
because a unix-like compile environment is not available which is
acceptable to the compile scripts), then configure with the
--disable-compile-to-bytecode option.

Documentation
-------------

Html documentation is available after installation in the default html
directory for the target installation (normally at
$(prefix)/share/doc/guile-a-sync2/html/index.html).

In addition, the documentation can be viewed at github at:
https://github.com/ChrisVine/guile-a-sync2/wiki
