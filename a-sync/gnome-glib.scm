;; Copyright (C) 2016 and 2017 Chris Vine

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
  #:use-module (ice-9 textual-ports)   ;; for put-string
  #:use-module (ice-9 binary-ports)    ;; for get-u8
  #:use-module (ice-9 suspendable-ports)
  #:use-module (ice-9 exceptions)      ;; for raise-exception and with-exception-handler
  #:use-module (a-sync coroutines)     ;; for make-iterator
  #:use-module (rnrs bytevectors)      ;; for bytevectors
  #:export (await-glib-task-in-thread
	    await-glib-task
	    await-glib-yield
	    await-glib-generator-in-thread
	    await-glib-generator
	    await-glib-timeout
	    await-glib-sleep
	    glib-add-watch
	    await-glib-read-suspendable
	    await-glib-write-suspendable
	    await-glib-getline
	    await-glib-put-string))

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
;; 'thunk'.  'handler' should take a single argument, which will be
;; the thrown exception object.
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
	 (with-exception-handler
	   (lambda (exc)
	     (g-idle-add (lambda ()
			   (resume (handler exc))
			   #f)))
	   (lambda ()
	     (let ((res (thunk)))
	       (g-idle-add (lambda ()
			     (resume res)
			     #f))))
	   #:unwind? #t)))
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
;; When 'thunk' is executed, this procedure is waiting on 'await', so
;; 'await' and 'resume' cannot be used again in 'thunk' (although
;; 'thunk' can call a-sync to start another series of asynchronous
;; operations with a new await-resume pair).  For that reason,
;; await-glib-yield is usually more convenient for composing
;; asynchronous tasks.  In retrospect, this procedure offers little
;; over await-glib-yield, apart from symmetry with
;; await-glib-task-in-thread.
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

;; This is a convenience procedure for use with glib, which will
;; surrender execution to the relevant event loop, so that code in
;; other a-sync or compose-a-sync blocks can run.  The remainder of
;; the code after the call to await-yield! in the current a-sync or
;; compose-a-sync block will execute on the next iteration through the
;; loop.  It is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).  It's effect is similar to calling await-glib-task with
;; a task that does nothing.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the relevant event loop runs: for this
;; purpose "the relevant event loop" is the event loop given by the
;; 'loop' argument, or if no 'loop' argument is provided or #f is
;; provided as the 'loop' argument, then the default event loop.
;;
;; This procedure should not throw any exceptions unless memory is
;; exhausted.
(define (await-glib-yield await resume)
  (g-idle-add (lambda ()
		(resume)
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
;; should take a single argument, which will be the thrown exception
;; object.
;;
;; This procedure calls 'await' and will return when the generator has
;; finished or, if 'handler' is provided, upon the generator throwing
;; an exception.  This procedure will return #f if the generator
;; completes normally, or 'guile-a-sync-thread-error if the generator
;; throws an exception and 'handler' is run (the
;; 'guile-a-sync-thread-error symbol is reserved to the implementation
;; and should not be yielded by the generator).
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
;; locally, will also propagate out of g-main-loop-run.
(define* (await-glib-generator-in-thread await resume generator proc #:optional handler)
  (if handler
      (call-with-new-thread
       (lambda ()
	 (with-exception-handler
	   (lambda (exc)
	     (g-idle-add (lambda ()
			   (handler exc)
			   (resume 'guile-a-sync-thread-error)
			   #f)))
	   (lambda ()
	     (let ((iter (make-iterator generator)))
	       (let next ((res (iter)))
		 (g-idle-add (lambda ()
			       (resume res)
			       #f))
		 (when (not (eq? res 'stop-iteration))
		   (next (iter))))))
	   #:unwind? #t)))
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
     ((eq? res 'stop-iteration)
      #f)
     ((eq? res 'guile-a-sync-thread-error)
      'guile-a-sync-thread-error)
     (else 
      (proc res)
      (next (await))))))

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
;; When 'proc' executes, 'await' and 'resume' will still be in use by
;; this procedure, so they may not be reused by 'proc' (even though
;; 'proc' runs in the event loop thread).
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the task starts), which shouldn't
;; happen unless memory is exhausted.  Exceptions arising during
;; execution of the generator, if not caught locally, will propagate
;; out of await-glib-generator.  Exceptions thrown by 'proc', if not
;; caught locally, will propagate out of g-main-loop-run.
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
;; In practice, calling await-glib-sleep may often be more convenient
;; for composing asynchronous code than using this procedure.  That is
;; because, when 'thunk' is executed, this procedure is waiting on
;; 'await', so 'await' and 'resume' cannot be used again in 'thunk'
;; (although 'thunk' can call a-sync to start another series of
;; asynchronous operations with a new await-resume pair).  In
;; retrospect, this procedure offers little over await-glib-sleep.
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

;; This is a convenience procedure for use with a glib main loop,
;; which will suspend execution of code in the current a-sync or
;; compose-a-sync block for the duration of 'msecs' milliseconds.  The
;; event loop will not be blocked by the sleep - instead any other
;; events in the event loop (including any other a-sync or
;; compose-a-sync blocks) will be serviced.  It is intended to be
;; called within a waitable procedure invoked by a-sync (which
;; supplies the 'await' and 'resume' arguments).
;;
;; Calling this procedure is equivalent to calling await-glib-timeout
;; with a 'proc' argument comprising a lambda expression that does
;; nothing.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
;;
;; This procedure should not throw any exceptions unless memory is
;; exhausted.
(define (await-glib-sleep await resume msecs)
  (g-timeout-add msecs
		 (lambda ()
		   (resume)
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
;; will be passed when it is invoked.  The purpose of 'proc' is to
;; carry out i/o operations on 'port' using the port's normal read
;; procedures.  'port' must be a suspendable non-blocking port.  This
;; procedure will return when 'proc' returns, as if by blocking read
;; operations, with the value returned by 'proc'.  However, the glib
;; event loop will not be blocked by this procedure even if only
;; individual characters or bytes comprising part characters are
;; available at any one time.  It is intended to be called in a
;; waitable procedure invoked by a-sync (which supplies the 'await'
;; and 'resume' arguments).  'proc' must not itself explicitly apply
;; 'await' and 'resume' as those are potentially in use by the
;; suspendable port while 'proc' is executing.  'proc' may return any
;; number of values.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, from 'proc' because of port or conversion errors)
;; will propagate out of this procedure in the first instance, and if
;; not caught locally will then propagate out of g-main-loop-run.
;;
;; This procedure will not call 'await' if the read operation(s) in
;; 'proc' can be effected immediately without waiting: instead, after
;; reading this procedure would return straight away without invoking
;; the glib main loop.
(define (await-glib-read-suspendable await resume port proc)
  (define id #f)
  (define (read-waiter p)
    (when (not id)
      (set! id (glib-add-watch (g-io-channel-unix-new (port->fdes port))
			       '(in hup err)
			       (lambda (ioc status)
				 ;; accessing 'status' doesn't work.  The wrapper does not
				 ;; seem to provide any way of extracting GIOCondition
				 ;; enumeration values which actually works.  However, 'err
				 ;; or 'pri should cause a read of the port to return an
				 ;; eof-object
				 (resume)
				 #t))))
    (await))
  (call-with-values
    (lambda ()
      (parameterize ((current-read-waiter read-waiter))
	(with-exception-handler
	  (lambda (exc)
	    (when id
	      (g-source-remove id)
	      (release-port-handle port))
	    (raise-exception exc))
	  (lambda ()
	    (proc port))
	  #:unwind? true)))
    (lambda args
      (when id
	(g-source-remove id)
	(release-port-handle port))
      (apply values args))))

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
  (await-glib-read-suspendable await resume port
			       (lambda (p)
				 (read-line p))))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because an implementation is
;; trivial to implement with await-glib-read-suspendable (and is
;; implemented by await-glib-read-suspendable).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and reads a block of data, such as a binary record, of size
;; 'size' from a non-blocking suspendable port 'port'.  This procedure
;; and will return a pair, normally comprising as its car a bytevector
;; of length 'size' containing the data, and as its cdr the number of
;; bytes received and placed in the bytevector (which will be the same
;; as 'size' unless an end-of-file object was encountered part way
;; through receiving the data).  If an end-of-file object is
;; encountered without any bytes of data, a pair with eof-object as
;; car and #f as cdr will be returned.
;;
;; See the documentation on the await-glib-read-suspendable procedure
;; for further particulars about this procedure.
(define (await-glib-getblock await resume port size)
  (define bv (make-bytevector size))
  (define index 0)
  (await-glib-read-suspendable await resume port
			       (lambda (p)
				 (let next ((u8 (get-u8 p)))
				   (if (eof-object? u8)
				       (if (= index 0)
					   (cons u8 #f)
					   (cons bv index))
				       (begin
					 (bytevector-u8-set! bv index u8)
					 (set! index (1+ index))
					 (if (= index size)
					     (cons bv size)
					     (next (get-u8 p)))))))))

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when it is invoked.  The purpose of 'proc' is to
;; carry out i/o operations on 'port' using the port's normal write
;; procedures.  'port' must be a suspendable non-blocking port.  This
;; procedure will return when 'proc' returns, as if by blocking write
;; operations, with the value returned by 'proc'.  However, the glib
;; event loop will not be blocked by this procedure even if only
;; individual characters or bytes comprising part characters can be
;; written at any one time.  It is intended to be called in a waitable
;; procedure invoked by a-sync (which supplies the 'await' and
;; 'resume' arguments).  'proc' must not itself explicitly apply
;; 'await' and 'resume' as those are potentially in use by the
;; suspendable port while 'proc' is executing.  'proc' may return any
;; number of values.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, from 'proc' because of port or conversion errors)
;; will propagate out of this procedure in the first instance, and if
;; not caught locally will then propagate out of g-main-loop-run.
;;
;; This procedure will not call 'await' if the write operation(s) in
;; 'proc' can be effected immediately without waiting: instead, after
;; writing this procedure would return straight away without invoking
;; the glib main loop.
(define (await-glib-write-suspendable await resume port proc)
  (define id #f)
  (define (write-waiter p)
    (when (not id)
      (set! id (glib-add-watch (g-io-channel-unix-new (port->fdes port))
			       '(out err)
			       (lambda (ioc status)
				 ;; accessing 'status' doesn't work.  The wrapper does not
				 ;; seem to provide any way of extracting GIOCondition
				 ;; enumeration values which actually works.  However, 'err
				 ;; or 'pri should cause a read of the port to return an
				 ;; eof-object
				 (resume)
				 #t))))
    (await))
  (call-with-values
    (lambda ()
      (parameterize ((current-write-waiter write-waiter))
	(with-exception-handler
	  (lambda (exc)
	    (when id
	      (g-source-remove id)
	      (release-port-handle port))
	    (raise-exception exc))
	  (lambda ()
	    (proc port))
	  #:unwind? #t)))
    (lambda args
      (when id
	(g-source-remove id)
	(release-port-handle port))
      (apply values args))))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-glib-write-suspendable (and is implemented by
;; await-glib-write-suspendable).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and will write a bytevector to the port.
;;
;; See the documentation on the await-glib-write-suspendable procedure
;; for further particulars about this procedure.
(define (await-glib-put-bytevector await resume port bv)
  (await-glib-write-suspendable await resume port
				(lambda (p)
				  (put-bytevector p bv)
				  ;; enforce a flush when the current
				  ;; write-waiter is still in operation
				  (force-output p))))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-glib-write-suspendable (and is implemented by
;; await-glib-write-suspendable).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and will write a string to the port.
;;
;; If CR-LF line endings are to be written when outputting the string,
;; the '\r' character (as well as the '\n' character) must be embedded
;; in the string.
;;
;; See the documentation on the await-glib-write-suspendable procedure
;; for further particulars about this procedure.
(define (await-glib-put-string await resume port text)
  (await-glib-write-suspendable await resume port
				(lambda (p)
				  (put-string p text)
				  ;; enforce a flush when the current
				  ;; write-waiter is still in operation
				  (force-output p))))
