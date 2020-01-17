Overview
--------

This library provides a thread safe event loop for guile-3.0
(event-loop.scm) with support for watches on ports/file descriptors
and timeouts, and which permits events to be posted by other tasks.
This includes tasks running on other threads.  It also provides a
coroutines interface (coroutines.scm) which provides await semantics
on such events, so as to avoid inversion of control (aka "callback
hell"), in conjunction with guile-2.2/3.0's suspendable ports
(await-ports.scm).  It requires guile >= 2.9.5.

This library has been forked from guile-a-sync2 to allow use of
guile-3.0's exception objects: this results in a few minor
incompatibilities compared with guile-a-sync2 (see end of this
README).  guile-a-sync2 still supports both guile-2.2 and 3.0 (with
old-style exceptions), so if you want user code which supports both of
those, then use guile-a-sync2 instead.  See the documentation
mentioned below for further details, and the docs/example.scm and
docs/example-glib.scm files.

A separate guile-a-sync library is available for guile-2.0 here:
https://github.com/ChrisVine/guile-a-sync , and guile-a-sync2 is
available here: https://github.com/ChrisVine/guile-a-sync2 .  This
library (guile-a-sync3) is parallel installable with guile-a-sync, and
also with guile-a-sync2 where guile-a-sync2 is compiled against
guile-2.2.  It is not parallel installable with guile-a-sync2 where
guile-a-sync2 is compiled against guile-3.0.

Installation
------------

When first run from git, or a tarball obtained from github, it is
necessary to set up autotools.  This can be done with:

  ./autogen.sh --prefix=/usr

or on a 64-bit system, probably:

  ./autogen.sh --prefix=/usr --libdir=/usr/lib64

This generates a configure script and installs libtool, and will then
run the configure script.

Subsequent configuration can be done just with

  ./configure --prefix=/usr

and

  ./configure --prefix=/usr --libdir=/usr/lib64

respectively, adding the --with-guile option as necessary.

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
$(prefix)/share/doc/guile-a-sync3/html/index.html).

In addition, the documentation can be viewed at github at:
https://github.com/ChrisVine/guile-a-sync3/wiki

Incompatibilities with guile-a-sync2
------------------------------------

Most code written for guile-a-sync2 will work fine with guile-a-sync3.
Incompatibilities arising from using the new exception system in
guile-3.0 are as follows:

1.  In guile-a-sync3 the exception handler procedures passed to
await-task-in-thread!, await-generator-in-thread!, thread-pool-add!,
await-task-in-thread-pool!, await-generator-in-thread-pool!,
await-glib-task-in-thread, await-glib-generator-in-thread,
await-glib-task-in-thread-pool and await-glib-generator-in-thread-pool
take a single argument (the thrown exception object) rather than
'catch'-handler style arguments.

2.  In guile-a-sync2 the thread-pool-set-non-blocking!,
thread-pool-stop!, thread-pool-add!, await-task-in-thread-pool!,
await-generator-in-thread-pool!, await-glib-task-in-thread-pool and
await-glib-generator-in-thread-pool procedures might throw a
'thread-pool-error exception in certain circumstances.  In
guile-a-sync3 these throw a compound exception object incorporating a
&thread-pool-error object with a thread-pool-error? predicate, and
which also incorporates &origin and &message exception objects.
