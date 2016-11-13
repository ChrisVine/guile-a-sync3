;; Copyright (C) 2016 Chris Vine

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


;; When using the scheme (gnome gtk) bindings of guile-gnome with
;; guile, in order to provide await semantics on gtk+ callbacks it
;; will normally be necessary to use the 'await' and 'resume'
;; procedures provided by the a-sync procedure in the (a-sync
;; coroutines) module directly (calling 'resume' in the gtk+ callback
;; when ready, and waiting on that callback using 'await').  However
;; when launching timeouts, file watches or idle events on the glib
;; main loop, convenience procedures are possible similar to those
;; provided for the event loop in the (a-sync event-loop) module.
;; These are set out below.
;;
;; Note that the g-idle-add procedure in guile-gnome is suspect -
;; there appears to be a garbage collection issue, and if you call the
;; procedure often enough in a single or multi-threaded program it
;; will eventually segfault.  g-io-add-watch is also broken in
;; guile-gnome, so this file uses its own glib-add-watch procedure
;; which is exported publicly in case it is useful to users.
;;
;; All the other scheme files provided by this library are by default
;; compiled by this library to bytecode.  That is not the case with
;; this file, so as not to create a hard dependency on guile-gnome.

(define-module (a-sync gnome-glib)
  #:use-module (gnome glib)            ;; for g-io-channel-* and g-source-*
  #:use-module (gnome gobject)         ;; for gclosure
  #:use-module (oop goops)             ;; for make
  #:use-module (ice-9 rdelim)          ;; for read-line
  #:use-module (ice-9 suspendable-ports)
  #:use-module (a-sync coroutines)     ;; for make-iterator
  #:export (await-glib-task-in-thread
	    await-glib-task
	    await-glib-generator-in-thread
	    await-glib-generator
	    await-glib-timeout
	    glib-add-watch
	    await-glib-read-suspendable
	    await-glib-write-suspendable
	    await-glib-getline))

(install-suspendable-ports!)

;; This is a convenience procedure which will run 'thunk' in its own
;; thread, and then post an event to the default glib main loop when
;; 'thunk' has finished.  This procedure calls 'await' in the default
;; glib main loop and will return the thunk's return value.  It is
;; intended to be called in a waitable procedure invoked by a-sync.
;; If the optional 'handler' argument is provided, then it will be run
;; in the default glib main loop if 'thunk' throws and its return
;; value will be the return value of this procedure; otherwise the
;; program will terminate if an unhandled exception propagates out of
;; 'thunk'.  'handler' should take the same arguments as a guile catch
;; handler (this is implemented using catch).
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs, where
;; the result of calling 'thunk' will be received.  As mentioned
;; above, the thunk itself will run in its own thread.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the worker thread starts), which
;; shouldn't happen unless memory is exhausted or pthread has run out
;; of resources.  Exceptions arising during execution of the task, if
;; not caught by a handler procedure, will terminate the program.
;; Exceptions thrown by the handler procedure will propagate out of
;; g-main-loop-run.
(define* (await-glib-task-in-thread await resume thunk #:optional handler)
  (if handler
      (call-with-new-thread
       (lambda ()
	 (catch
	  #t
	  (lambda ()
	    (let ((res (thunk)))
	      (g-idle-add (lambda ()
			    (resume res)
			    #f))))
	  (lambda args
	    (g-idle-add (lambda ()
			  (resume (apply handler args))
			  #f))))))
      (call-with-new-thread
       (lambda ()
	 (let ((res (thunk)))
	   (g-idle-add (lambda ()
			 (resume res)
			 #f))))))
  (await))

;; This is a convenience procedure for use with glib, which will run
;; 'thunk' in the default glib main loop.  This procedure calls
;; 'await' and will return the thunk's return value.  It is intended
;; to be called in a waitable procedure invoked by a-sync.  It is the
;; single-threaded corollary of await-glib-task-in-thread.  This means
;; that (unlike with await-glib-task-in-thread) while 'thunk' is
;; running other events in the main loop will not make progress, so
;; blocking calls should not be made in 'thunk'.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the task starts), which shouldn't
;; happen unless memory is exhausted.  Exceptions arising during
;; execution of the task, if not caught locally, will propagate out of
;; g-main-loop-run.
(define (await-glib-task await resume thunk)
  (g-idle-add (lambda ()
		(resume (thunk))
		#f))
  (await))

;; This is a convenience procedure for acting asynchronously on values
;; yielded by generator procedures.  The 'generator' argument is a
;; procedure taking one argument, namely a yield argument (see the
;; documentation on the make-iterator procedure for further details).
;; This await-glib-generator-in-thread procedure will run 'generator'
;; in its own worker thread, and whenever 'generator' yields a value
;; will cause 'proc' to execute in the default glib main loop.
;;
;; 'proc' should be a procedure taking a single argument, namely the
;; value yielded by the generator.  If the optional 'handler' argument
;; is provided, then that handler will be run in the default glib main
;; loop if 'generator' throws; otherwise the program will terminate if
;; an unhandled exception propagates out of 'generator'.  'handler'
;; should take the same arguments as a guile catch handler (this is
;; implemented using catch).
;;
;; This procedure calls 'await' and will return when the generator has
;; finished or, if 'handler' is provided, upon the generator throwing
;; an exception.  This procedure will return #f if the generator
;; completes normally, or 'guile-a-sync-thread-error if the generator
;; throws an exception and 'handler' is run.
;;
;; This procedure is intended to be called in a waitable procedure
;; invoked by a-sync.  It must (like the a-sync procedure) be called
;; in the same thread as that in which the default glib main loop
;; runs.  As mentioned above, the generator itself will run in its own
;; thread.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the worker thread starts), which
;; shouldn't happen unless memory is exhausted or pthread has run out
;; of resources.  Exceptions arising during execution of the
;; generator, if not caught by a handler procedure, will terminate the
;; program.  Exceptions thrown by the handler procedure will propagate
;; out of g-main-loop-run.  Exceptions thrown by 'proc', if not caught
;; locally, will also propagate out of g-main-loop-run!.
;;
;; This procedure is first available in version 0.4 of this library.
(define* (await-glib-generator-in-thread await resume generator proc #:optional handler)
  (if handler
      (call-with-new-thread
       (lambda ()
	 (catch
	   #t
	   (lambda ()
	     (let ((iter (make-iterator generator)))
	       (let next ((res (iter)))
		 (g-idle-add (lambda ()
			       (resume res)
			       #f))
		 (when (not (eq? res 'stop-iteration))
		   (next (iter))))))
	   (lambda args
	     (g-idle-add (lambda ()
			   (apply handler args)
			   (resume 'guile-a-sync-thread-error)
			   #f))))))
      (call-with-new-thread
       (lambda ()
	 (let ((iter (make-iterator generator)))
	   (let next ((res (iter)))
	     (g-idle-add (lambda ()
			   (resume res)
			   #f))
	     (when (not (eq? res 'stop-iteration))
	       (next (iter))))))))
  (let next ((res (await)))
    (cond
     ((eq? res 'guile-a-sync-thread-error)
      'guile-a-sync-thread-error)
     ((not (eq? res 'stop-iteration))
      (proc res)
      (next (await)))
     (else #f))))

;; This is a convenience procedure for acting asynchronously on values
;; yielded by generator procedures.  The 'generator' argument is a
;; procedure taking one argument, namely a yield argument (see the
;; documentation on the make-iterator procedure for further details).
;; This await-glib-generator procedure will run 'generator', and
;; whenever 'generator' yields a value will cause 'proc' to execute in
;; the default glib main loop - each time 'proc' runs it will do so as
;; a separate event in the main loop and so be multi-plexed with other
;; events.  'proc' should be a procedure taking a single argument,
;; namely the value yielded by the generator.
;;
;; This procedure is intended to be called in a waitable procedure
;; invoked by a-sync.  It is the single-threaded corollary of
;; await-glib-generator-in-thread.  This means that (unlike with
;; await-glib-generator-in-thread) while 'generator' is running other
;; events in the main loop will not make progress, so blocking calls
;; (other than to the yield procedure) should not be made in
;; 'generator'.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the task starts), which shouldn't
;; happen unless memory is exhausted.  Exceptions arising during
;; execution of the generator, if not caught locally, will propagate
;; out of await-glib-generator.  Exceptions thrown by 'proc', if not
;; caught locally, will propagate out of g-main-loop-run!.
;;
;; This procedure is first available in version 0.4 of this library.
(define (await-glib-generator await resume generator proc)
  (let ((iter (make-iterator generator)))
    (let next ((res (iter)))
      (g-idle-add (lambda ()
		    (resume res)
		    #f))
      (when (not (eq? res 'stop-iteration))
	(next (iter)))))
  (let next ((res (await)))
    (when (not (eq? res 'stop-iteration))
      (proc res)
      (next (await)))))

;; This is a convenience procedure for use with a glib main loop,
;; which will run 'thunk' in the default glib main loop when the
;; timeout expires.  This procedure calls 'await' and will return the
;; thunk's return value.  It is intended to be called in a waitable
;; procedure invoked by a-sync.  The timeout is single shot only - as
;; soon as 'thunk' has run once and completed, the timeout will be
;; removed from the event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the first call to 'await' is made),
;; which shouldn't happen unless memory is exhausted.  Exceptions
;; thrown by 'thunk', if not caught locally, will propagate out of
;; g-main-loop-run.
(define (await-glib-timeout await resume msecs thunk)
  (g-timeout-add msecs
		 (lambda ()
		   (resume (thunk))
		   #f))
  (await))

;; This procedure replaces guile-gnome's g-io-add-watch procedure,
;; which won't compile.  It attaches a watch on a g-io-channel object
;; to the main context provided, or if none is provided, to the
;; default glib main context (the main program loop).  It returns a
;; glib ID which can be passed subsequently to the g-source-remove
;; procedure.  It should be possible to call this procedure in any
;; thread.
;;
;; 'ioc' is the g-io-condition object to which the watch is to be
;; attached, and 'cond' is a list of symbols (being 'in, 'out, 'pri,
;; 'hup, 'err and 'nval) comprising the events for which the watch is
;; established.
;;
;; 'func' is executed when an event occurs, and takes two arguments:
;; first the g-io-channel object to which the watch has been attached,
;; and second a g-io-condition object indicating the watch condition
;; which has been met (note: the interface for g-io-condition objects
;; is broken in guile-gnome at present).  The watch is ended either by
;; 'func' returning #f, or by applying g-source-remove to the return
;; value of this procedure.  Otherwise the watch will continue.
;;
;; File watches in guile-gnome are implemented using a GIOChannel
;; object, and unfortunately GIOChannel support in guile-gnome is
;; decaying.  The only procedure that guile-gnome provides to read
;; from a GIOChannel object is g-io-channel-read-line, which does not
;; work.  One is therefore usually left with having to read from a
;; guile port (whose underlying file descriptor is 'fd') using guile's
;; port input procedures, but this has its own difficulties, which
;; means that one of the following approaches should be adopted (i)
;; the port has to be unbuffered (say by using the open-file or fdopen
;; procedure with the '0' mode option or the R6RS open-file-input-port
;; procedure with a buffer-mode of none, or by calling setvbuf), or
;; (ii) 'proc' must deal with everything in the port's buffer by
;; calling drain-input, or by looping on char-ready? before returning,
;; or (iii) the port's ordinary input procedures should be used with
;; suspendable ports using the await-glib-read-suspendable procedure.
;; This is because otherwise, if the port is buffered, once the port
;; is read from there may be further characters in the buffer to be
;; dealt with even though the GIOChannel watch does not trigger
;; because there is nothing new to make the file descriptor ready for
;; reading.
(define* (glib-add-watch ioc cond func #:optional context)
  (let ((s (g-io-create-watch ioc cond))
        (closure (make <gclosure>
                   #:return-type <gboolean>
                   #:func func)))
    (g-source-set-closure s closure)
    (g-source-attach s context)))

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when func is invoked.  'port' must be a suspendable
;; non-blocking port.  'proc' will be executed whenever there is
;; something available to read, and this procedure will return when
;; 'proc' returns, as if by a blocking read.  The glib event loop will
;; not be blocked by this procedure even if only individual characters
;; or bytes comprising part characters are available at any one time.
;; It is intended to be called in a waitable procedure invoked by
;; a-sync.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, because of port or conversion errors) will
;; propagate out of this procedure in the first instance, and if not
;; caught locally will then propagate out of g-main-loop-run.
(define (await-glib-read-suspendable await resume port proc)
  (define (read-waiter p)
    (await))
  (define id (glib-add-watch (g-io-channel-unix-new (port->fdes port))
			     '(in hup err)
			     (lambda (ioc status)
			       ;; accessing 'status' doesn't work.  The wrapper does not
			       ;; seem to provide any way of extracting GIOCondition
			       ;; enumeration values which actually works.  However, 'err
			       ;; or 'pri should cause a read of the port to return an
			       ;; eof-object
  			       (resume)
			       #t)))
  (let ((res
	 (parameterize ((current-read-waiter read-waiter))
	   (catch #t
	     (lambda ()
	       (proc port))
	     (lambda args
	       (g-source-remove id)
	       (release-port-handle port)
	       (apply throw args))))))
    (g-source-remove id)
    (release-port-handle port)
    res))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-glib-read-suspendable (and is implemented by
;; await-glib-read-suspendable).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and reads a line of text from a non-blocking suspendable
;; port and returns it (without the terminating '\n' character).  See
;; the documentation on the await-glib-read-suspendable procedure for
;; further particulars about this procedure.
(define (await-glib-getline await resume port)
  (await-read-suspendable! await resume port
			   (lambda (p)
			     (read-line p))))

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when func is invoked.  'port' must be a suspendable
;; non-blocking port.  'proc' will be executed whenever the port is
;; available to write to, and this procedure will return when 'proc'
;; returns, as if by a blocking write.  The glib event loop will not
;; be blocked by this procedure even if only individual characters or
;; bytes comprising part characters can be written at any one time.
;; It is intended to be called in a waitable procedure invoked by
;; a-sync.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, because of port or conversion errors) will
;; propagate out of this procedure in the first instance, and if not
;; caught locally will then propagate out of g-main-loop-run.
(define (await-glib-write-suspendable await resume port proc)
  (define (write-waiter p)
    (await))
  (define id (glib-add-watch (g-io-channel-unix-new (port->fdes port))
			     '(out err)
			     (lambda (ioc status)
			       ;; accessing 'status' doesn't work.  The wrapper does not
			       ;; seem to provide any way of extracting GIOCondition
			       ;; enumeration values which actually works.  However, 'err
			       ;; or 'pri should cause a read of the port to return an
			       ;; eof-object
  			       (resume)
			       #t)))
  (let ((res
	 (parameterize ((current-write-waiter write-waiter))
	   (catch #t
	     (lambda ()
	       (proc port))
	     (lambda args
	       (g-source-remove id)
	       (release-port-handle port)
	       (apply throw args))))))
    (g-source-remove id)
    (release-port-handle port)
    res))
