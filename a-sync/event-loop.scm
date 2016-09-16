;; Copyright (C) 2014 and 2016 Chris Vine

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

(define-module (a-sync event-loop)
  #:use-module (ice-9 q)               ;; for make-q, etc
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)         ;; for with-mutex and call-with-new-thread
  #:use-module (srfi srfi-1)           ;; for reduce, delete!, member and delete-duplicates
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)      ;; for make-bytevector, bytevector-copy!
  #:use-module (rnrs hashtables)       ;; for make-eqv-hashtable, etc
  #:use-module (a-sync monotonic-time) ;; for get-time
  #:export (set-default-event-loop!
	    get-default-event-loop
	    make-event-loop
	    event-loop-run!
	    event-loop-add-read-watch!
	    event-loop-add-write-watch!
	    event-loop-remove-read-watch!
	    event-loop-remove-write-watch!
	    event-loop-block!
	    event-loop-quit!
	    event-post!
	    timeout-post!
	    timeout-remove!
	    event-loop?
	    await-task!
	    await-task-in-thread!
	    await-task-in-event-loop!
	    await-timeout!))


;; this variable is not exported - use the accessors below
(define ***default-event-loop*** #f)

;; The 'el' (event loop) argument is optional.  This procedure sets
;; the default event loop for the procedures in this module to the one
;; passed in (which must have been constructed by the make-event-loop
;; procedure), or if no argument is passed (or #f is passed), a new
;; event loop will be constructed for you as the default, which can be
;; accessed via the get-default-event-loop procedure.  The default
;; loop variable is not a fluid or a parameter - it is intended that
;; the default event loop is the same for every thread in the program,
;; and that the default event loop would normally run in the thread
;; with which the program started.  This procedure is not thread safe
;; - if it might be called by a different thread from others which
;; might access the default event loop, then external synchronization
;; may be required.  However, that should not normally be an issue.
;; The normal course would be to call this procedure once only on
;; program start up, before other threads have started.  It is usually
;; a mistake to call this procedure twice: if there are asynchronous
;; events pending (that is, if event-loop-run!  has not returned) you
;; will probably not get the results you expect.
(define* (set-default-event-loop! #:optional el)
  (if el
      (set! ***default-event-loop*** el)
      (set! ***default-event-loop*** (make-event-loop))))

;; This returns the default loop set by the set-default-event-loop!
;; procedure, or #f if none has been set.
(define (get-default-event-loop)
  ***default-event-loop***)

(define-record-type <event-loop>
  (_make-event-loop mutex q done event-in event-out read-files read-files-actions
                    write-files write-files-actions timeouts current-timeout block
		    num-tasks threshold delay loop-thread)
  event-loop?
  (mutex _mutex-get)
  (q _q-get)
  (done _done-get _done-set!)
  (event-in _event-in-get _event-in-set!)
  (event-out _event-out-get _event-out-set!)
  (read-files _read-files-get _read-files-set!)
  (read-files-actions _read-files-actions-get)
  (write-files _write-files-get _write-files-set!)
  (write-files-actions _write-files-actions-get)
  (timeouts _timeouts-get _timeouts-set!)
  (current-timeout _current-timeout-get _current-timeout-set!)
  (block _block-get _block-set!)
  (num-tasks _num-tasks-get _num-tasks-set!)
  (threshold _threshold-get)
  (delay _delay-get)
  (loop-thread _loop-thread-get _loop-thread-set!))

;; This procedure constructs an event loop object.  From version 0.2,
;; this procedure optionally takes two throttling arguments for
;; backpressure when applying the event-post! procedure to the event
;; loop.  The 'throttle-threshold' argument specifies the number of
;; unexecuted tasks queued for execution, by virtue of calls to
;; event-post!, at which throttling will first be applied.  Where the
;; threshold is exceeded, throttling proceeds by adding a wait to any
;; thread which calls the event-post! procedure, equal to the cube of
;; the number of times (if any) by which the number of queued tasks
;; exceeds the threshold multiplied by the value of 'threshold-delay'.
;; The value of 'threshold-delay' should be given in microseconds.
;; Throttling is only applied where the call to event-post! is made in
;; a thread other than the one in which the event loop runs.
;;
;; So if the threshold given is 10000 tasks and the delay given is
;; 1000 microseconds, upon 10000 unexecuted tasks accumulating a delay
;; of 1000 microseconds will be applied to callers of event-post!
;; which are not in the event loop thread, at 20000 unexecuted tasks a
;; delay of 8000 microseconds will be applied, and at 30000 unexecuted
;; tasks a delay of 27000 microseconds will be applied, and so on.
;;
;; If throttle-threshold and throttle-delay arguments are not provided
;; (or #f is passed for them), then no throttling takes place.
(define make-event-loop
  (case-lambda
    (() (make-event-loop #f #f))
    ((throttle-threshold throttle-delay)
     (when (and throttle-threshold
		(or (not (number? throttle-threshold))
		    (not (number? throttle-delay))
		    (< throttle-threshold 1)
		    (< throttle-delay 1)))
       (error "invalid arguments passed to make-event-loop"))
     (let* ((event-pipe (pipe))
	    (in (car event-pipe))
	    (out (cdr event-pipe)))
       ;; the write end of the pipe needs to be set non-blocking so
       ;; that if the pipe fills and the event loop thread is also
       ;; putting a new event in the queue, an exception is thrown
       ;; rather than a deadlock arising
       (fcntl out F_SETFL (logior O_NONBLOCK
				  (fcntl out F_GETFL)))
       (_make-event-loop (make-mutex)
			 (make-q)
			 #f
			 in
			 out
			 '()
			 (make-eqv-hashtable)
			 '()
			 (make-eqv-hashtable)
			 '()
			 #f
			 #f
			 0
			 throttle-threshold
			 throttle-delay
			 #f)))))

;; timeouts are kept as an unsorted list of timeout items.  Each
;; timeout item is a vector of four elements.  First, an absolute time
;; pair (say as generated by _get-abstime) representing the time it is
;; next due to fire; second, a gensym value representing its tag;
;; third, the timeout value in milliseconds from which it was
;; generated (so it can be used again if it is a repeating timeout);
;; and fourth, a callback thunk representing the action to be executed
;; when the timeout is due.  This procedure finds the next timeout
;; item due to fire, or #f if the list passed in is empty.
(define (_next-timeout timeouts)
  (reduce (lambda (elt previous)
	    (if (< (_time-remaining (vector-ref elt 0))
		   (_time-remaining (vector-ref previous 0)))
		elt
		previous))
	  #f
	  timeouts))

;; for an absolute time pair (secs . usecs), say as generated by
;; _get-abstime, this returns the number of seconds remaining to
;; current time as a real number.  If the absolute time is in the
;; past, it returns a negative number: this is useful behaviour as it
;; will help the _next-timeout procedure pick the oldest timeout
;; amongst any which have already expired.  However, normalize a
;; negative value to 0 before passing it to select.
(define (_time-remaining abstime)
  (let ((curr (get-time)))
   (let ((secs (- (car abstime) (car curr)))
	  (usecs (- (cdr abstime) (cdr curr))))
      (+ (exact->inexact secs) (/ (exact->inexact usecs) 1000000)))))

;; takes a timeout value in milliseconds and returns a (secs . usecs)
;; pair representing current time plus the timeout value as an
;; absolute time
(define (_get-abstime msecs)
  (let* ((curr (get-time))
	 (usec-tmp (round (+ (* msecs 1000) (cdr curr)))))
    (let ((secs (+ (car curr) (quotient usec-tmp 1000000)))
	  (usecs (remainder usec-tmp 1000000)))
    (cons secs usecs))))

(define (_process-timeouts el)
  ;; we don't need any mutexes here as we only access the timeouts and
  ;; current-timeout fields of an event loop object, and any
  ;; individual timeout item vectors, in the event loop thread
  (let ((current-timeout (_current-timeout-get el)))
    (when (and current-timeout
	       (<= (_time-remaining (vector-ref current-timeout 0)) 0))
      (if ((vector-ref current-timeout 3))
	  (vector-set! current-timeout 0
		       (_get-abstime (vector-ref current-timeout 2)))
	  (_timeouts-set! el (_filter-timeout (_timeouts-get el) (vector-ref current-timeout 1))))
      (_current-timeout-set! el (_next-timeout (_timeouts-get el))))))

;; This returns a list of timeouts, with the tagged timeout removed
;; from the timeout list passed in
(define (_filter-timeout timeouts tag)
  (let loop ((remaining timeouts)
	     (checked '()))
    (if (null? remaining)
	(begin
	  ;; it is probably too extreme to error here if the user
	  ;; requests to remove a timeout which no longer exists
	  (simple-format (current-error-port)
			 "Warning: timeout tag ~A not found in timeouts list in procedure _filter-timeout\n"
			 tag)
	  (force-output (current-error-port))
	  timeouts)
	(let ((first (car remaining)))
	  (if (eq? (vector-ref first 1) tag)
	      (append (reverse checked) (cdr remaining))
	      (loop (cdr remaining) (cons first checked)))))))

;; this takes a port or a file descriptor as an argument and returns a
;; file descriptor (being the port's underlying file descriptor in the
;; case of a port).  It is used by _file-equal? to test for file
;; equality and to make keys for the file watch actions hashtables.
(define-syntax-rule (_fd-or-port->fd file)
  (let ((f file))
    (if (port? f) (fileno f) f)))

;; for the purposes of the event loop, two files compare equal if
;; their file descriptors are the same, even if one is a port and one
;; is a file descriptor (or both are ports)
(define (_file-equal? file1 file2)
  (let ((fd1 (_fd-or-port->fd file1))
	(fd2 (_fd-or-port->fd file2)))
    (= fd1 fd2)))

;; we don't need any mutexes here as we only access any of the
;; read-files, read-files-actions, write-files and write-files-actions
;; fields in the event loop thread.  This removes a given read file
;; watch and its action from an event loop object.  A file descriptor
;; and a port with the same underlying file descriptor, or two ports
;; with the same underlying file descriptor, compare equal for the
;; purposes of removal.
(define (_remove-read-watch-impl! file el)
  (_read-files-set! el (delete! file (_read-files-get el) _file-equal?))
  (hashtable-delete! (_read-files-actions-get el)
		     (_fd-or-port->fd file)))

;; we don't need any mutexes here as we only access any of the
;; read-files, read-files-actions, write-files and write-files-actions
;; fields in the event loop thread.  This removes a given write file
;; watch and its action from an event loop object.  A file descriptor
;; and a port with the same underlying file descriptor, or two ports
;; with the same underlying file descriptor, compare equal for the
;; purposes of removal.
(define (_remove-write-watch-impl! file el)
  (_write-files-set! el (delete! file (_write-files-get el) _file-equal?))
  (hashtable-delete! (_write-files-actions-get el)
		     (_fd-or-port->fd file)))

;; this procedure is only called by event-post!, and tests for the
;; number of tasks pending which have been added with that procedure,
;; and adds a wait to the calling thread if necessary.  See comments
;; on make-event-loop for further information.
(define (_check-for-throttle el)
  (let ((threshold (_threshold-get el)))
    (when threshold
      (let ((loop-thread (with-mutex (_mutex-get el) (_loop-thread-get el))))
	(when (and threshold loop-thread (not (eqv? loop-thread (current-thread))))
	  (let* ((tasks (with-mutex (_mutex-get el) (_num-tasks-get el)))
		 (excess (/ tasks threshold)))
	    (when (not (zero? (truncate excess)))
	      (let loop ((remaining (inexact->exact (round (* excess excess excess (_delay-get el))))))
		(unless (zero? remaining) (loop (usleep remaining)))))))))))
   
;; the 'el' (event loop) argument is optional.  This procedure starts
;; the event loop passed in as an argument, or if none is passed (or
;; #f is passed) it starts the default event loop.  The event loop
;; will run in the thread which calls this procedure.  If this is
;; different from the thread which called make-event-loop, external
;; synchronization is required to ensure visibility.  If this
;; procedure has returned, including after a call to event-loop-quit!,
;; this procedure may be called again to restart the event loop.  If a
;; callback throws, or something else throws in the implementation,
;; then this procedure will clean up the event loop as if
;; event-loop-quit! had been called, and the exception will be
;; rethrown out of this procedure.
(define* (event-loop-run! #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-run!"))
    (_event-loop-run-impl! el)))

(define (_event-loop-run-impl! el)
  (define mutex (_mutex-get el))
  (define q (_q-get el))
  (define event-in (_event-in-get el))
  (define event-fd (fileno event-in))

  (with-mutex mutex (_loop-thread-set! el (current-thread)))

  (catch
   #t
   (lambda ()
     ;; we don't need to use the mutex in this procedure except in
     ;; relation to q, _done-get and _block-get, as we only access the
     ;; current-timeout field of an event loop object, any individual
     ;; timeout item vectors, and any of the read-files,
     ;; read-files-actions, write-files and write-files-actions fields
     ;; in the event loop thread
     (let loop1 ()
       (_process-timeouts el)
       (when (not (and (null? (_read-files-get el))
		       (null? (_write-files-get el))
		       (null? (_timeouts-get el))
		       (with-mutex mutex (and (q-empty? q)
					      (not (_block-get el))))))
         ;; we provide local versions in order to take a consistent view
         ;; on each run, since we might remove items from the lists after
         ;; executing the callbacks
         (let ((read-files (_read-files-get el))
	       (read-files-actions (_read-files-actions-get el))
	       (write-files (_write-files-get el))
	       (write-files-actions (_write-files-actions-get el))
	       (current-timeout (_current-timeout-get el)))
	   (let ((res (catch 'system-error
		        (lambda ()
			  (select (cons event-fd read-files)
				  write-files
				  (delete-duplicates (append read-files write-files) _file-equal?)
				  (and current-timeout
				       (let ((secs (_time-remaining (vector-ref current-timeout 0))))
					 (if (< secs 0) 0 secs)))))
			(lambda args
			  (if (= EINTR (system-error-errno args))
			      '(() () ())
			      (apply throw args))))))
	     (for-each (lambda (elt)
			 (let ((action
				(hashtable-ref read-files-actions
					       (_fd-or-port->fd elt)
					       #f)))
			   (if action
			       (when (not (action 'in))
				 (_remove-read-watch-impl! elt el))
			       (error "No action in event loop for read file: " elt))))
		       (delv event-fd (car res)))
	     (for-each (lambda (elt)
			 (let ((action
				(hashtable-ref write-files-actions
					       (_fd-or-port->fd elt)
					       #f)))
			   (if action
			       (when (not (action 'out))
			         (_remove-write-watch-impl! elt el))
			       (error "No action in event loop for write file: " elt))))
		       (cadr res))
	     (for-each (lambda (elt)
			 (let ((action
				(or (hashtable-ref read-files-actions
						   (_fd-or-port->fd elt)
						   #f)
				    (hashtable-ref write-files-actions
						   (_fd-or-port->fd elt)
						   #f))))
			   (if action
			       (when (not (action 'excpt))
				 (if (member elt read-files _file-equal?)
				     (_remove-read-watch-impl! elt el)
				     (_remove-write-watch-impl! elt el)))
			       (error "No action in event loop for file: " elt))))
		       (caddr res))
	     ;; the strategy with posted events is first to empty the
	     ;; event pipe (the only purpose of which is to cause the
	     ;; event loop to unblock) and then run any events queued
	     ;; in the queue.  This (i) eliminates any concerns that
	     ;; events might go missing if the pipe fills up, and (ii)
	     ;; ensures that if a timeout or watch callback has posted
	     ;; an event (say ending the timeout or watch), that will
	     ;; have been acted on by the time the event loop begins
	     ;; its next iteration.
	     (when (memv event-fd (car res))
	       (let loop2 ()
		 (let ((c (read-char event-in)))
		   (when (and (char-ready? event-in)
			      (not (eof-object? c))) ;; this shouldn't ever happen
		     (loop2)))))
	     (let loop3 ()
	       (let ((action (with-mutex mutex
			       (if (q-empty? q)
				   (begin
				     ;; num-tasks should be 0 with an
				     ;; empty queue anyway, but ...
				     (_num-tasks-set! el 0)
				     #f)
				   (begin
				     (_num-tasks-set! el (1- (_num-tasks-get el)))
				     (deq! q))))))
		 (when action
		   (action)
		   ;; one of the posted events may have called
		   ;; event-loop-quit!, so test for it
		   (when (not (with-mutex mutex (_done-get el)))
		     (loop3))))))
	   (if (not (with-mutex mutex (_done-get el)))
	       (loop1)
	       ;; clear out any stale events before returning and
	       ;; unblocking
	       (_event-loop-reset! el))))))
   (lambda args
     ;; something threw, probably a callback.  Put the event loop in a
     ;; valid state and rethrow
     (_event-loop-reset! el)
     (apply throw args))))

;; This procedure is only called in the event loop thread, by
;; event-loop-run!  The only things requiring protection by a mutex
;; are the q, done-set, event-out, num-tasks and loop-thread fields of
;; the event loop object.  However, for consistency we deal with all
;; operations on the event pipe below via the mutex.
(define (_event-loop-reset! el)
  ;; the only foolproof way of vacating a unix pipe is to close it and
  ;; then create another one
  (with-mutex (_mutex-get el)
    ;; discard any EAGAIN exception when flushing the output buffer
    ;; of a fully filled pipe on closing
    (catch 'system-error
      (lambda ()
	(close-port (_event-in-get el))
	(close-port (_event-out-get el)))
      (lambda args
	(unless (= EAGAIN (system-error-errno args))
	  (apply throw args))))
      
    (let* ((event-pipe (pipe))
	   (in (car event-pipe))
	   (out (cdr event-pipe)))
      (fcntl out F_SETFL (logior O_NONBLOCK
				 (fcntl out F_GETFL)))
      (_event-in-set! el in)
      (_event-out-set! el out))
    (let ((q (_q-get el)))
      (let loop ()
	(when (not (q-empty? q))
	  (deq! q)
	  (loop))))
    (_done-set! el #f)
    (_num-tasks-set! el 0)
    (_loop-thread-set! el #f))
  (_read-files-set! el '())
  (hashtable-clear! (_read-files-actions-get el))
  (_write-files-set! el '())
  (hashtable-clear! (_write-files-actions-get el))
  (_timeouts-set! el '())
  (_current-timeout-set! el #f))

;; The 'el' (event loop) argument is optional.  This procedure will
;; start a read watch in the event loop passed in as an argument, or
;; if none is passed (or #f is passed), in the default event loop.
;; The 'proc' callback should take a single argument, and when called
;; this will be set to 'in or 'excpt.  The same port or file
;; descriptor can also be passed to event-loop-add-write-watch, and if
;; so and the descriptor is also available for writing, the write
;; callback will also be called with its argument set to 'out.  If
;; there is already a read watch for the file passed, the old one will
;; be replaced by the new one.  If 'proc' returns #f, the read watch
;; will be removed from the event loop, otherwise the watch will
;; continue.  This is thread safe - any thread may add a watch, and
;; the callback will execute in the event loop thread.  The file
;; argument can be either a port or a file descriptor.  If 'file' is a
;; file descriptor, any port for the descriptor is not referenced for
;; garbage collection purposes - it must remain valid while operations
;; are carried out on the descriptor.  If 'file' is a buffered port,
;; buffering will be taken into account in indicating whether a read
;; can be made without blocking (but on a buffered port, for
;; efficiency purposes each read operation in response to this watch
;; should usually exhaust the buffer by calling drain-input or by
;; looping on char-ready?).
;;
;; This procedure should not throw an exception unless memory is
;; exhausted.  If 'proc' throws, say because of port errors, and the
;; exception is not caught locally, it will propagate out of
;; event-loop-run!.
(define* (event-loop-add-read-watch! file proc #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-add-read-watch!"))
    (event-post! (lambda ()
		   (_read-files-set!
		    el
		    (cons file
			  (delete! file (_read-files-get el) _file-equal?)))
		   (hashtable-set! (_read-files-actions-get el)
				   (_fd-or-port->fd file)
				   proc))
		 el)))

;; The 'el' (event loop) argument is optional.  This procedure will
;; start a write watch in the event loop passed in as an argument, or
;; if none is passed (or #f is passed), in the default event loop.
;; The 'proc' callback should take a single argument, and when called
;; this will be set to 'out or 'excpt.  The same port or file
;; descriptor can also be passed to event-loop-add-read-watch, and if
;; so and the descriptor is also available for reading or in
;; exceptional condition, the read callback will also be called with
;; its argument set to 'in or 'excpt (if both a read and a write watch
;; have been set for the same file argument, and there is an
;; exceptional condition, it is the read watch procedure which will be
;; called with 'excpt rather than the write watch procedure, so if
;; that procedure returns #f only the read watch will be removed).  If
;; there is already a write watch for the file passed, the old one
;; will be replaced by the new one.  If 'proc' returns #f, the write
;; watch will be removed from the event loop, otherwise the watch will
;; continue.  This is thread safe - any thread may add a watch, and
;; the callback will execute in the event loop thread.  The file
;; argument can be either a port or a file descriptor.  If 'file' is a
;; file descriptor, any port for the descriptor is not referenced for
;; garbage collection purposes - it must remain valid while operations
;; are carried out on the descriptor.
;;
;; If 'file' is a buffered port, buffering will be taken into account
;; in indicating whether a write can be made without blocking, either
;; because there is room in the buffer for a character, or because the
;; underlying file descriptor is ready for a character.  This can have
;; unintended consequences: if the buffer is full but the underlying
;; file descriptor is ready for a character, the next write will cause
;; a buffer flush, and if the size of the buffer is greater than the
;; number of characters that the file can receive without blocking,
;; blocking might still occur.  Unless the port will carry out a
;; partial flush in such a case, this procedure will therefore
;; generally work best with unbuffered ports (say by using the
;; open-file or fdopen procedure with the '0' mode option or the R6RS
;; open-file-input-port procedure with a buffer-mode of none, or by
;; calling setvbuf).
;;
;; This procedure should not throw an exception unless memory is
;; exhausted.  If 'proc' throws, say because of port errors, and the
;; exception is not caught locally, it will propagate out of
;; event-loop-run!.
(define* (event-loop-add-write-watch! file proc #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-add-write-watch!"))
    (event-post! (lambda ()
		   (_write-files-set!
		    el
		    (cons file
			  (delete! file (_write-files-get el) _file-equal?)))
		   (hashtable-set! (_write-files-actions-get el)
				   (_fd-or-port->fd file)
				   proc))
		 el)))

;; The 'el' (event loop) argument is optional.  This procedure will
;; remove a read watch from the event loop passed in as an argument,
;; or if none is passed (or #f is passed), from the default event
;; loop.  The file argument may be a port or a file descriptor.  This
;; is thread safe - any thread may remove a watch.  A file descriptor
;; and a port with the same underlying file descriptor compare equal
;; for the purposes of removal.
(define* (event-loop-remove-read-watch! file #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-remove-read-watch!"))
    (event-post! (lambda ()
		   (_remove-read-watch-impl! file el))
		 el)))

;; The 'el' (event loop) argument is optional.  This procedure will
;; remove a write watch from the event loop passed in as an argument,
;; or if none is passed (or #f is passed), from the default event
;; loop.  The file argument may be a port or a file descriptor.  This
;; is thread safe - any thread may remove a watch.  A file descriptor
;; and a port with the same underlying file descriptor compare equal
;; for the purposes of removal.
(define* (event-loop-remove-write-watch! file #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-remove-write-watch!"))
    (event-post! (lambda ()
		   (_remove-write-watch-impl! file el))
		 el)))

;; The 'el' (event loop) argument is optional.  This procedure will
;; post a callback for execution in the event loop passed in as an
;; argument, or if none is passed (or #f is passed), in the default
;; event loop.  The 'action' callback is a thunk.  This is thread safe
;; - any thread may post an event (that is its main purpose), and the
;; action callback will execute in the event loop thread.  Actions
;; execute in the order in which they were posted.  If an event is
;; posted from a worker thread, it will normally be necessary to call
;; event-loop-block! beforehand.
;;
;; This procedure should not throw an exception unless memory is
;; exhausted.  If the 'action' callback throws, and the exception is
;; not caught locally, it will propagate out of event-loop-run!.
;;
;; Where this procedure is called by other than the event loop thread,
;; throttling may take place if the number of posted callbacks waiting
;; to execute exceeds the threshold set for the event loop - see the
;; documentation on make-event-loop for further details.
(define* (event-post! action #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-post!"))
    (with-mutex (_mutex-get el)
      (enq! (_q-get el) action)
      (_num-tasks-set! el (1+ (_num-tasks-get el)))
      (let ((out (_event-out-get el)))
	;; if the event pipe is full and an EAGAIN error arises, we
	;; can just swallow it.  The only purpose of writing #\x is to
	;; cause the select procedure to return and reloop to pick up
	;; any new entries in the event queue.
	(catch 'system-error
	  (lambda ()
	    (write-char #\x out)
	    (force-output out))
	  (lambda args
	    (unless (= EAGAIN (system-error-errno args))
	      (apply throw args))))))
    (_check-for-throttle el)))

;; The 'el' (event loop) argument is optional.  This procedure adds a
;; timeout to the event loop passed in as an argument, or if none is
;; passed (or #f is passed), to the default event loop.  The timeout
;; will repeat unless and until the passed-in callback returns #f or
;; timeout-remove! is called.  The passed-in callback must be a thunk.
;; This procedure returns a tag symbol to which timeout-remove! can be
;; applied.  It may be called by any thread, and the timeout callback
;; will execute in the event loop thread.
;;
;; This procedure should not throw an exception unless memory is
;; exhausted.  If the 'action' callback throws, and the exception is
;; not caught locally, it will propagate out of event-loop-run!.
(define* (timeout-post! msecs action #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to timeout-post!"))
    (let ((tag (gensym "timeout-"))
	  (abstime (_get-abstime msecs)))
      (event-post! (lambda ()
		     (let ((new-timeouts (cons (vector abstime
						       tag
						       msecs
						       action)
					       (_timeouts-get el))))
		       (_timeouts-set! el new-timeouts)
		       (_current-timeout-set! el (_next-timeout new-timeouts))))
		   el)
      tag)))

;; The 'el' (event loop) argument is optional.  This procedure stops
;; the timeout with the given tag from executing in the event loop
;; passed in as an argument, or if none is passed (or #f is passed),
;; in the default event loop.  It may be called by any thread.
(define* (timeout-remove! tag #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to timeout-remove!"))
    (event-post! (lambda ()
		   (_timeouts-set! el (_filter-timeout (_timeouts-get el) tag))
		   ;; the timeout removed might be the current-timeout
		   ;; - if so, reset current-timeout
		   (when (eq? tag (vector-ref (_current-timeout-get el) 1))
		     (_current-timeout-set! el (_next-timeout (_timeouts-get el)))))
		 el)))
      
;; By default, upon there being no more watches, timeouts and posted
;; events for an event loop, event-loop-run! will return, which is
;; normally what you want with a single threaded program.  However,
;; this is undesirable where a worker thread is intended to post an
;; event to the main loop after it has reached a result, say via
;; await-task-in-thread!, because the main loop may have ended before
;; it posts.  Passing #t to the val argument of this procedure will
;; prevent that from happening, so that the event loop can only be
;; ended by calling event-loop-quit!, or by calling event-loop-block!
;; again with a #f argument (to switch the event loop back to
;; non-blocking mode, pass #f).  This is thread safe - any thread may
;; call this procedure.  The 'el' (event loop) argument is optional:
;; this procedure operates on the event loop passed in as an argument,
;; or if none is passed (or #f is passed), on the default event loop.
(define* (event-loop-block! val #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-block!"))
    (with-mutex (_mutex-get el)
      (let ((old-val (_block-get el)))
	(_block-set! el (not (not val)))
	(when (and old-val (not val))
	  ;; if the event pipe is full and an EAGAIN error arises, we
	  ;; can just swallow it.  The only purpose of writing #\x is
	  ;; to cause the select procedure to return and reloop and
	  ;; then exit the event loop if there are no further events.
	  (let ((out (_event-out-get el)))
	    (catch 'system-error
	      (lambda ()
		(write-char #\x out)
		(force-output out))
	      (lambda args
		(unless (= EAGAIN (system-error-errno args))
		  (apply throw args))))))))))

;; This procedure causes an event loop to unblock.  Any events
;; remaining in the event loop will be discarded.  New events may
;; subsequently be added after event-loop-run! has unblocked and
;; event-loop-run! then called for them.  This is thread safe - any
;; thread may call this procedure.  The 'el' (event loop) argument is
;; optional: this procedure operates on the event loop passed in as an
;; argument, or if none is passed (or #f is passed), on the default
;; event loop.
(define* (event-loop-quit! #:optional el)
  (let ((el (or el (get-default-event-loop))))
    (when (not el) 
      (error "No default event loop set for call to event-loop-quit!"))
    (with-mutex (_mutex-get el)
      (_done-set! el #t)
      ;; if the event pipe is full and an EAGAIN error arises, we can
      ;; just swallow it.  The only purpose of writing #\x is to cause
      ;; the select procedure to return
      (let ((out (_event-out-get el)))
	(catch 'system-error
	  (lambda ()
	    (write-char #\x out)
	    (force-output out))
	  (lambda args
	    (unless (= EAGAIN (system-error-errno args))
	      (apply throw args))))))))

;; This is a convenience procedure whose signature is:
;;
;;   (await-task-in-thread! await resume [loop] thunk [handler])
;;
;; The loop and handler arguments are optional.  The procedure will
;; run 'thunk' in its own thread, and then post an event to the event
;; loop specified by the 'loop' argument when 'thunk' has finished, or
;; to the default event loop if no 'loop' argument is provided or if
;; #f is provided as the 'loop' argument (pattern matching is used to
;; detect the type of the third argument).  This procedure calls
;; 'await' and will return the thunk's return value.  It is intended
;; to be called in a waitable procedure invoked by a-sync.  It will
;; normally be necessary to call event-loop-block! before invoking
;; this procedure.  If the optional 'handler' argument is provided,
;; then that handler will be run in the event loop thread if 'thunk'
;; throws and the return value of the handler would become the return
;; value of this procedure; otherwise the program will terminate if an
;; unhandled exception propagates out of 'thunk'.  'handler' should
;; take the same arguments as a guile catch handler (this is
;; implemented using catch).
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs, where the result
;; of calling 'thunk' will be received.  As mentioned above, the thunk
;; itself will run in its own thread.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the worker thread starts), which
;; shouldn't happen unless memory is exhausted or pthread has run out
;; of resources.  Exceptions arising during execution of the task, if
;; not caught by a handler procedure, will terminate the program.
;; Exceptions thrown by the handler procedure will propagate out of
;; event-loop-run!.
(define (await-task-in-thread! await resume . rest)
  (match rest
    ((loop thunk handler)
     (_await-task-in-thread-impl! await resume loop thunk handler))
    ((#f thunk)
     (_await-task-in-thread-impl! await resume #f thunk #f))
    ((($ <event-loop>) thunk)
     (_await-task-in-thread-impl! await resume (car rest) thunk #f))
    ((thunk handler)
     (_await-task-in-thread-impl! await resume #f thunk handler))
    ((thunk)
     (_await-task-in-thread-impl! await resume #f thunk #f))
    (_
     (error "Wrong number of arguments passed to await-task-in-thread!" await resume rest))))

(define (_await-task-in-thread-impl! await resume loop thunk handler)
  ;; set the event loop value here so that if the default event
  ;; loop is chosen, it is the one pertaining at the time this
  ;; procedure is called
  (let ((loop (or loop (get-default-event-loop))))
    (when (not loop) 
      (error "No default event loop set for call to await-task-in-thread!"))
    (if handler
	(call-with-new-thread
	 (lambda ()
	   (catch
	     #t
	     (lambda ()
	       (let ((res (thunk)))
		 (event-post! (lambda () (resume res))
			      loop)))
	     (lambda args
	       (event-post! (lambda () (resume (apply handler args)))
			    loop)))))
	(call-with-new-thread
	 (lambda ()
	   (let ((res (thunk)))
	     (event-post! (lambda () (resume res))
			   loop)))))
    (await)))

;; This is a convenience procedure whose signature is:
;;
;;   (await-task-in-event-loop! await resume [waiter] worker thunk)
;;
;; The 'waiter' argument is optional.  The 'worker' argument is an
;; event loop running in a different thread than the one in which this
;; procedure is called, and is the one in which 'thunk' will be
;; executed by posting an event to that loop.  The result of executing
;; 'thunk' will then be posted to the event loop specified by the
;; 'waiter' argument, or to the default event loop if no 'waiter'
;; argument is provided or if #f is provided as the 'waiter' argument,
;; and will comprise this procedure's return value.  This procedure is
;; intended to be called in a waitable procedure invoked by a-sync.
;; It will normally be necessary to call event-loop-block! on 'waiter'
;; (or on the default event loop) before invoking this procedure.
;;
;; This procedure calls 'await' and must (like the a-sync procedure)
;; be called in the same thread as that in which the 'waiter' or
;; default event loop runs (as the case may be).
;;
;; This procedure acts as a form of channel through which two
;; different event loops may communicate.  It also offers a means by
;; which a master event loop (the waiter or default event loop) may
;; allocate work to worker event loops for execution.  It would be
;; nice to have a pool of worker event loops for the purpose, but that
;; is a work for the future.
;;
;; Depending on the circumstances, it may be desirable to provide
;; throttling arguments when constructing the 'worker' event loop, in
;; order to enable backpressure to be supplied if the 'worker' event
;; loop becomes overloaded: see the documentation on the
;; make-event-loop procedure for further information about that.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up, which shouldn't happen unless memory is exhausted or
;; pthread has run out of resources.  Exceptions arising during
;; execution of the task, if not caught locally, will propagate out of
;; the event-loop-run! procedure called for the 'worker' event loop.
;;
;; This procedure is first available in version 0.2 of this library.
(define await-task-in-event-loop!
  (case-lambda
    ((await resume worker thunk)
     (await-task-in-event-loop! await resume #f worker thunk))
    ((await resume waiter worker thunk)
     (let ((waiter (or waiter (get-default-event-loop))))
       (when (not waiter) 
	 (error "No default event loop set for call to await-task-in-event-loop!"))
       (event-post! (lambda ()
		      (let ((res (thunk)))
			(event-post! (lambda () (resume res))
				     waiter)))
		    worker)
       (await)))))

;; This is a convenience procedure whose signature is:
;;
;;   (await-task! await resume [loop] thunk)
;;
;; The 'loop' argument is optional.  This will run 'thunk' in the
;; event loop specified by the 'loop' argument, or in the default
;; event loop if no 'loop' argument is provided or #f is provided as
;; the 'loop' argument.  This procedure calls 'await' and will return
;; the thunk's return value.  It is intended to be called in a
;; waitable procedure invoked by a-sync.  It is the single-threaded
;; corollary of await-task-in-thread!.  This means that (unlike with
;; await-task-in-thread!) while 'thunk' is running other events in the
;; event loop will not make progress, so blocking calls should not be
;; made in 'thunk'.  This procedure can be useful for the purpose of
;; implementing co-operative multi-tasking, say by composing tasks
;; with compose-a-sync (see compose.scm).
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the task starts), which shouldn't
;; happen unless memory is exhausted.  Exceptions arising during
;; execution of the task, if not caught locally, will propagate out of
;; event-loop-run!.
(define await-task!
  (case-lambda
    ((await resume thunk)
     (await-task! await resume #f thunk))
    ((await resume loop thunk)
     (event-post! (lambda ()
		    (resume (thunk)))
		  loop)
     (await))))

;; This is a convenience procedure whose signature is:
;;
;;   (await-timeout! await resume [loop] msecs thunk)
;;
;; This procedure will run 'thunk' in the event loop thread when the
;; timeout expires.  It calls 'await' and will return the thunk's
;; return value.  It is intended to be called in a waitable procedure
;; invoked by a-sync.  The timeout is single shot only - as soon as
;; 'thunk' has run once and completed, the timeout will be removed
;; from the event loop.  The 'loop' argument is optional: this
;; procedure operates on the event loop passed in as an argument, or
;; if none is passed (or #f is passed), on the default event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions may propagate out of this procedure if they arise while
;; setting up (that is, before the first call to 'await' is made),
;; which shouldn't happen unless memory is exhausted.  Exceptions
;; thrown by 'thunk', if not caught locally, will propagate out of
;; event-loop-run!.
(define await-timeout!
  (case-lambda
    ((await resume msecs thunk)
     (await-timeout! await resume #f msecs thunk))
    ((await resume loop msecs thunk)
     (timeout-post! msecs
		    (lambda ()
		      (resume (thunk))
		      #f)
		    loop)
     (await))))
