;; Copyright Chris Vine 2016

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
  #:export (await-glib-task-in-thread
	    await-glib-task
	    await-glib-timeout
	    glib-add-watch
	    a-sync-glib-read-watch
	    a-sync-glib-write-watch
	    await-glib-getline))


;; This is a convenience procedure which will run 'thunk' in its own
;; thread, and then post an event to the default glib main loop when
;; 'thunk' has finished.  This procedure calls 'await' and will return
;; the thunk's return value.  It is intended to be called in a
;; waitable procedure invoked by a-sync.  If the optional 'handler'
;; argument is provided, then it will be run in the event loop thread
;; if 'thunk' throws and its return value will be the return value of
;; this procedure; otherwise the program will terminate if an
;; unhandled exception propagates out of 'thunk'.  'handler' should
;; take the same arguments as a guile catch handler (this is
;; implemented using catch).  If 'handler' throws, the exception will
;; propagate out of g-mail-loop-run.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs, where
;; the result of calling 'thunk' will be received.  As mentioned
;; above, the thunk itself will run in its own thread.
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
;; running other events in the main loop will not make progress.  This
;; is not particularly useful except when called by the main loop
;; thread for the purpose of bringing the loop to an end at its own
;; place in the event queue, or when called by a worker thread to
;; report a result expected by a waitable procedure running in the
;; main loop thread.  (For the latter case though,
;; await-glib-task-in-thread is generally a more convenient wrapper.)
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
(define (await-glib-task await resume thunk)
  (g-idle-add (lambda ()
		(resume (thunk))
		#f))
  (await))

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
(define* (glib-add-watch ioc cond func #:optional context)
  (let ((s (g-io-create-watch ioc cond))
        (closure (make <gclosure>
                   #:return-type <gboolean>
                   #:func func)))
    (g-source-set-closure s closure)
    (g-source-attach s context)))

;; This is a convenience procedure for use with a glib main loop,
;; which will run 'proc' in the default glib main loop whenever the
;; file descriptor 'fd' is ready for reading, and apply resume
;; (obtained from a call to a-sync) to the return value of 'proc'.
;; 'proc' should take two arguments, the first of which will be set by
;; glib to the g-io-channel object constructed for the watch and the
;; second of which will be set to the GIOCondition ('in, 'pri, 'hup or
;; 'err) provided by glib which caused the watch to activate.  It is
;; intended to be called in a waitable procedure invoked by a-sync.
;; The watch is multi-shot - it is for the user to bring it to an end
;; at the right time by calling g-source-remove in the waitable
;; procedure on the id tag returned by this procedure.  Any port for
;; the file descriptor 'fd' is not referenced for garbage collection
;; purposes - it must remain valid while the read watch is active.
;; This procedure is mainly intended as something from which
;; higher-level asynchronous file operations can be constructed, such
;; as the await-glib-getline procedure.
;;
;; Because this procedure takes a 'resume' argument derived from the
;; a-sync procedure, it must (like the a-sync procedure) in practice
;; be called in the same thread as that in which the default glib main
;; loop runs.
(define (a-sync-glib-read-watch resume fd proc)
  (glib-add-watch (g-io-channel-unix-new fd)
		  '(in pri hup err)
		  (lambda (a b)
		    (resume (proc a b))
		    #t)))
			   
;; This is a convenience procedure for use with a glib main loop,
;; which will start a read watch on 'port' and run 'thunk' in the
;; default glib main loop whenver an entire line of text has been
;; received.  This procedure calls 'await' while waiting for input and
;; will return the line of text received (without the terminating '\n'
;; character).  The event loop will not be blocked by this procedure
;; even if only individual characters are available at any one time.
;; It is intended to be called in a waitable procedure invoked by
;; a-sync.  This procedure is implemented using
;; a-sync-glib-read-watch.  If an exceptional condition ('pri) or an
;; error ('err) is encountered, #f will be returned.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the default glib main loop runs.
(define (await-glib-getline await resume port)
  (define text '())
  (define id (a-sync-glib-read-watch resume
				     (port->fdes port)
				     (lambda (ioc status)
				       (if (or (eq? status 'pri)
					       (eq? status 'err))
					   #f
					   (let next ()
					     (let ((ch (read-char port)))
					       (if (not (or (eof-object? ch)
							    (char=? ch #\newline)))
						   (begin
						     (set! text (cons ch text))
						     (if (char-ready? port)
							 (next)
							 'more))
						   (reverse-list->string text))))))))
  (let next ((res (await)))
    (if (eq? res 'more)
	(next (await))
	(begin
	  (g-source-remove id)
	  (release-port-handle port)
	  res))))
	
;; This is a convenience procedure for use with a glib main loop,
;; which will run 'proc' in the default glib main loop whenever the
;; file descriptor 'fd' is ready for writing, and apply resume
;; (obtained from a call to a-sync) to the return value of 'proc'.
;; 'proc' should take two arguments, the first of which will be set by
;; glib to the g-io-channel object constructed for the watch and the
;; second of which will be set to the GIOCondition ('out or 'err)
;; provided by glib which caused the watch to activate.  It is
;; intended to be called in a waitable procedure invoked by a-sync.
;; The watch is multi-shot - it is for the user to bring it to an end
;; at the right time by calling g-source-remove in the waitable
;; procedure on the id tag returned by this procedure.  Any port for
;; the file descriptor 'fd' is not referenced for garbage collection
;; purposes - it must remain valid while the read watch is active.
;; This procedure is mainly intended as something from which
;; higher-level asynchronous file operations can be constructed.
;;
;; Because this procedure takes a 'resume' argument derived from the
;; a-sync procedure, it must (like the a-sync procedure) in practice
;; be called in the same thread as that in which the default glib main
;; loop runs.
(define (a-sync-glib-write-watch resume fd proc)
  (glib-add-watch (g-io-channel-unix-new fd)
		  '(out err)
		  (lambda (a b)
		    (resume (proc a b))
		    #t)))
			   
