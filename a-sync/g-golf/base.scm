;; Copyright (C) 2016 to 2020 Chris Vine

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


;; When using the g-golf bindings for gtk+, in order to provide await
;; semantics on gtk+ callbacks it will normally be necessary to use
;; the 'await' and 'resume' procedures provided by the a-sync
;; procedure in the (a-sync coroutines) module directly (calling
;; 'resume' in the gtk+ callback when ready, and waiting on that
;; callback using 'await').  However when launching timeouts, file
;; watches or other events on the glib main loop using g-golf,
;; convenience procedures are possible similar to those provided for
;; the event loop in the (a-sync event-loop) module.  These are set
;; out in the files in this directory.
;;
;; Most of the scheme files provided by this library are by default
;; compiled by this library to bytecode.  That is not the case with
;; this file, so as not to create a hard dependency on g-golf.

(define-module (a-sync g-golf base)
  #:use-module (ice-9 threads)         ;; for with-mutex and call-with-new-thread
  #:use-module (g-golf hl-api glib)
  #:use-module (g-golf glib main-event-loop)
  #:use-module (a-sync coroutines)     ;; for make-iterator
  #:use-module (a-sync thread-pool)
  #:export (a-sync-glib-quit
	    await-glib-task-in-thread
	    await-glib-task
	    await-glib-yield
	    await-glib-generator-in-thread
	    await-glib-generator
	    await-glib-timeout
	    await-glib-sleep
	    await-glib-task-in-thread-pool
	    await-glib-generator-in-thread-pool))

;; Usually the default glib main loop runs throughout a program's
;; lifetime and only quits (say, via GTK) when the program is brought
;; to an end by the user.  Notwithstanding that, in a non-GUI program
;; sometimes you may need to quit the main loop programmatically (most
;; of the glib example code in this library does so).  To quit a main
;; loop using g-main-loop-quit, the loop must be running.  One problem
;; that may arise from this is that the suspendable port procedures in
;; the (a-sync g-golf await-ports) module do not suspend to the main
;; loop if they do not need to.  This procedure deals with the issue
;; by posting a g-main-loop-quit event to the main loop instead of
;; calling g-main-loop-quit directly, so ensuring that the main loop
;; must be running when g-main-loop-quit is called.
(define (a-sync-glib-quit loop)
  (g-idle-add (lambda () (g-main-loop-quit loop) #f) 'default))

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
			   #f)
			 'default))
	   (lambda ()
	     (let ((res (thunk)))
	       (g-idle-add (lambda ()
			     (resume res)
			     #f)
			   'default)))
	   #:unwind? #t)))
      (call-with-new-thread
       (lambda ()
	 (let ((res (thunk)))
	   (g-idle-add (lambda ()
			 (resume res)
			 #f)
		       'default)))))
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
		#f)
	      'default)
  (await))

;; This is a convenience procedure for use with glib, which will
;; surrender execution to the default glib main loop, so that code in
;; other a-sync or compose-a-sync blocks can run.  The remainder of
;; the code after the call to await-glib-yield in the current a-sync
;; or compose-a-sync block will execute on the next iteration through
;; the loop.  It is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).  It's effect is similar to calling await-glib-task with
;; a task that does nothing.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
;;
;; This procedure should not throw any exceptions unless memory is
;; exhausted.
;;
;; This procedure is first available in version 0.19 of this library.
(define (await-glib-yield await resume)
  (g-idle-add (lambda ()
		(resume)
		#f)
	      'default)
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
			   #f)
			 'default))
	   (lambda ()
	     (let ((iter (make-iterator generator)))
	       (let next ((res (iter)))
		 (g-idle-add (lambda ()
			       (resume res)
			       #f)
			     'default)
		 (when (not (eq? res 'stop-iteration))
		   (next (iter))))))
 	   #:unwind? #t)))
     (call-with-new-thread
       (lambda ()
	 (let ((iter (make-iterator generator)))
	   (let next ((res (iter)))
	     (g-idle-add (lambda ()
			   (resume res)
			   #f)
			 'default)
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
;; same thread as that in which the default glib main loop runs.
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
		    #f)
		  'default)
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

;; This is a convenience procedure for use with a glib main loop,
;; which will run 'thunk' in the thread pool specified by the 'pool'
;; argument.  The result of executing 'thunk' will then be posted to
;; the default glib main loop, and will comprise this procedure's
;; return value.  This procedure is intended to be called within a
;; waitable procedure invoked by a-sync (which supplies the 'await'
;; and 'resume' arguments).
;;
;; If the optional 'handler' argument is provided, then that handler
;; will run if 'thunk' throws, and the return value of the handler
;; would become the return value of this procedure; otherwise the
;; program will terminate if an unhandled exception propagates out of
;; 'thunk'.  'handler' should take a single argument, which will be
;; the thrown exception object.  Note that unlike a handler passed to
;; the thread-pool-add!  procedure, 'handler' will run in the default
;; glib main loop thread and not in a thread pool thread.  Exceptions
;; thrown by the handler procedure will propagate out of
;; g-main-loop-run.
;;
;; This procedure calls 'await' and must (like the a-sync procedure)
;; be called in the same thread as that in which the default glib main
;; loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up, which shouldn't happen unless the thread pool given by
;; the 'pool' argument has been closed (in which case a compound
;; &thread-pool-error exception, also incorporating &origin and
;; &message objects, will arise), the thread pool tries to start an
;; additional native thread which the operating system fails to supply
;; (which would cause a system exception to arise) or memory is
;; exhausted.
(define* (await-glib-task-in-thread-pool await resume pool thunk #:optional handler)
  (if handler
      (thread-pool-add! pool
			(lambda ()
			  (let ((res (thunk)))
			    (g-idle-add (lambda () (resume res) #f)
					'default)))
			(lambda (exc)
			  (g-idle-add (lambda () (resume (handler exc)) #f)
				      'default)))
      (thread-pool-add! pool
			(lambda ()
			  (let ((res (thunk)))
			    (g-idle-add (lambda () (resume res) #f)
					'default)))))
    (await))

;; The 'generator' argument is a procedure taking one argument, namely
;; a yield argument (see the documentation on the make-iterator
;; procedure for further details).  This
;; await-glib-generator-in-thread-pool procedure will cause
;; 'generator' to run as a task in the 'pool' thread pool, and
;; whenever 'generator' yields a value this will cause 'proc' to
;; execute in the default glib main loop.  'proc' should be a
;; procedure taking a single argument, namely the value yielded by the
;; generator.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; If the optional 'handler' argument is provided, then that handler
;; will run if 'generator' throws an exception; otherwise the program
;; will terminate if an unhandled exception propagates out of
;; 'generator'.  'handler' should take a single argument, which will
;; be the thrown exception object..  Note that unlike a handler passed
;; to the thread-pool-add! procedure, 'handler' will run in the
;; default glib main loop thread and not in a thread pool thread.
;; This procedure will return #f if the generator completes normally,
;; or 'guile-a-sync-thread-error if the generator throws an exception
;; and 'handler' is run (the 'guile-a-sync-thread-error symbol is
;; reserved to the implementation and should not be yielded by the
;; generator).  Exceptions thrown by the handler procedure will
;; propagate out of g-main-loop-run.
;;
;; This procedure calls 'await' and will return when the generator has
;; finished or, if 'handler' is provided, upon the generator raising
;; an exception.  This procedure must (like the a-sync procedure) be
;; called in the same thread as that in which the default glib main
;; loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up, which shouldn't happen unless the thread loop given by
;; the 'pool' argument has been closed (in which case a compound
;; &thread-pool-error exception, also incorporating &origin and
;; &message objects, will arise), the thread pool tries to start an
;; additional native thread which the operating system fails to supply
;; (which would cause a system exception to arise) or memory is
;; exhausted.  Exceptions arising during the execution of 'proc', if
;; not caught locally, will propagate out of g-main-loop-run.
(define* (await-glib-generator-in-thread-pool await resume pool generator proc #:optional handler)
  (if handler
      (thread-pool-add! pool
			(lambda ()
			  (let ((iter (make-iterator generator)))
			    (let next ((res (iter)))
			      (g-idle-add (lambda () (resume res) #f)
					  'default)
			      (when (not (eq? res 'stop-iteration))
				(next (iter))))))
			(lambda (exc)
			  (g-idle-add (lambda ()
					(handler exc)
					(resume 'guile-a-sync-thread-error)
					#f)
				      'default)))
      (thread-pool-add! pool
			(lambda ()
			  (let ((iter (make-iterator generator)))
			    (let next ((res (iter)))
			      (g-idle-add (lambda () (resume res) #f)
					  'default)
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
