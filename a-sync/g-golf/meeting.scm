;; Copyright (C) 2017 and 2020 Chris Vine

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

(define-module (a-sync g-golf meeting)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)  ;; for call/ec
  #:use-module (srfi srfi-9)
  #:use-module (g-golf hl-api glib)
  #:export (make-g-meeting
	    g-meeting?
	    g-meeting-close
	    g-meeting-ready?
	    g-meeting-send
	    g-meeting-receive))

(define-record-type <g-meeting>
  (_make-g-meeting resumptions status)
  g-meeting?
  (resumptions resumptions-get)
  (status status-get status-set!))

;; This procedure makes and returns a g-meeting object.  g-meetings
;; are objects on which a-sync or compose-a-sync blocks running on the
;; default glib main loop can synchronize by one passing a datum to
;; another.
;;
;; Strictly speaking this procedure can be called in any native OS
;; thread, but since it carries out no synchronization of native
;; threads the user would have to provide her own synchronization if
;; called in other than the thread of the event loop with respect to
;; which the meeting will be held; so it is best if this procedure is
;; called in the thread of that event loop.
;;
;; This procedure is first available in version 0.19 of this library.
(define (make-g-meeting)
  (_make-g-meeting (make-q) 'unset))

;; This closes a g-meeting object.  It's purpose is to wake up any
;; "pseudo-thread" (that is, any a-sync or compose-a-sync block)
;; waiting in g-meeting-send or g-meeting-receive by causing either
;; procedure to return with a 'stop-iteration value.
;;
;; Where that is not necessary (say, the receiver already knows how
;; many items are to be sent), then this procedure does not need to be
;; applied.  It is not needed in order to release resources.
;;
;; This procedure is first available in version 0.19 of this library.
(define (g-meeting-close m)
  (let ((res (resumptions-get m)))
    (when (not (q-empty? res))
      (let lp ((elt (deq! res)))
	(g-idle-add (lambda () ((vector-ref elt 0) 'stop-iteration) #f)
		    'default)
	;; clean up all other g-meeting objects on which an affected
	;; waiter is selecting
	((vector-ref elt 2))
	(when (not (q-empty? res))
	  (lp (deq! res)))))
    (status-set! m 'closed)))

;; This indicates whether applying g-meeting-send or g-meeting-receive
;; (as the case may be) to the g-meeting object 'm' will return
;; immediately: in other words, this procedure will return #t if
;; another a-sync or compose-a-sync block is already waiting on the
;; object or the g-meeting object has been closed, otherwise #f.
;;
;; This procedure is first available in version 0.19 of this library.
(define (g-meeting-ready? m)
  (or (not (q-empty? (resumptions-get m)))
      (eq? (status-get m) 'closed)))

;; The signature of this procedure is:
;;
;;   (g-meeting-send await resume m0 [m1 ...] datum)
;;
;; This sends a datum to a receiver via one or more g-meeting objects
;; 'm0 m1 ...'.  If no receiver is waiting for the datum, this
;; procedure waits until a receiver calls g-meeting-receive on one of
;; the g-meeting objects to request the datum.  If a receiver is
;; already waiting, this procedure passes on the datum and returns
;; immediately.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; Multiple senders may wait on a g-meeting object to permit fan in.
;; The provided datum of each sender will be passed to a receiver (as
;; and when a receiver becomes available) in the order in which this
;; procedure was invoked.  In addition, this procedure has
;; 'select'-like behavior: multiple g-meeting objects may be passed
;; and this procedure will send to the first one which becomes
;; available to receive the datum.
;;
;; Once a datum exchange has taken place, the g-meeting object(s) can
;; be reused for making another exchange (provided the g-meeting
;; objects have not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; default glib main loop runs.  To have other native OS threads
;; communicate with that loop, use await-g-task-in-thread,
;; await-g-generator-in-thread, await-g-task-in-thread-pool or
;; await-g-generator-in-thread-pool.
;;
;; This procedure always returns #f unless g-meeting-close has been
;; applied to a g-meeting object, in which case 'stop-iteration is
;; returned.  Note that if multiple g-meeting objects are passed to
;; this procedure and one of them is then closed, this procedure will
;; return 'stop-iteration and any wait will be abandonned.  It is
;; usually a bad idea to close a g-meeting object on which this
;; procedure is waiting where this procedure is selecting on more than
;; one g-meeting object.
;;
;; This procedure is first available in version 0.19 of this library.
(define (g-meeting-send await resume . rest)
  (when (null? rest)
    (error "arity error for g-meeting-send procedure"))
  (when (not (g-meeting? (car rest)))
    (error "no g-meeting objects passed to g-meeting-send procedure"))
  (when (null? (cdr rest))
    (error "no datum passed to g-meeting-send procedure"))
  (let ((split (- (length rest) 1)))
    (let ((meetings (list-head rest split))
	  (datum (car (list-tail rest split))))
      ;; If resumptions is not empty for one or more g-meetings whose
      ;; status is not at 'set then it must contain resumption
      ;; iterators for one or more waiting receivers so we can proceed
      ;; immediately.  Otherwise all the g-meeting objects must either
      ;; have status already at 'set so another sender is already
      ;; waiting on that g-meeting for a receiver, or the g-meeting's
      ;; resumptions are empty so nothing is waiting: in either case
      ;; we should also add ourselves to all the g-meetings' queues
      ;; and wait for a receiver.
      (let ((meeting (call/ec
		      (lambda (k)
			(for-each (lambda (m)
				    (let ((status (status-get m)))
				      (when (and (eq? status 'unset)
						 (not (q-empty? (resumptions-get m))))
					(k m))
				      (when (eq? status 'closed)
					(k 'closed))))
				  meetings)
			#f))))
	(cond
	 ((eq? meeting 'closed)
	  'stop-iteration)
	 (meeting
	  ;; if a g-meeting is both unset and has resumption
	  ;; iterators, send the datum to a waiting receiver and
	  ;; return
	  (let ((elt (deq! (resumptions-get meeting))))
	    (g-idle-add (lambda ()
			  ((vector-ref elt 0) datum) #f)
			'default)
	    ((vector-ref elt 2))
	    #f))
	 (else
	  ;; if no g-meeting is unset with resumption iterators,
	  ;; insert the resume iterator in each g-meeting object and
	  ;; await a receiver
	  (letrec ((item (vector resume
				 datum
				 (lambda ()
				   (for-each (lambda (m)
					       (let ((res (resumptions-get m)))
						 (q-remove! res item)
						 (when (q-empty? res) (status-set! m 'unset))))
					     meetings)))))
	    (for-each (lambda (m)
			(status-set! m 'set)
			(enq! (resumptions-get m) item))
		      meetings)
	    (await))))))))

;; The signature of this procedure is:
;;
;;   (g-meeting-receive await resume m0 [m1 ...])
;;
;; This receives a datum from a sender via one or more g-meeting
;; objects 'm0 m1 ...'.  If no sender is waiting to pass the datum,
;; this procedure waits until a sender calls g-meeting-send on one of
;; the g-meeting objects to provide the datum.  If a sender is already
;; waiting, this procedure returns immediately with the datum
;; supplied.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; Multiple receivers may wait on a g-meeting object to permit fan
;; out.  The waiting receivers will be released (as and when a sender
;; provides a datum) in the order in which this procedure was invoked.
;; In addition, this procedure has 'select'-like behavior: multiple
;; g-meeting objects may be passed and this procedure will receive
;; from the first one which sends a datum.
;;
;; Once a datum exchange has taken place, the g-meeting object(s) can be
;; reused for making another exchange (provided the g-meeting objects
;; have not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; default glib main loop runs.  To have other native OS threads
;; communicate with that loop, use await-g-task-in-thread,
;; await-g-generator-in-thread, await-g-task-in-thread-pool or
;; await-g-generator-in-thread-pool.
;;
;; This procedure always returns the datum value supplied by
;; g-meeting-send unless g-meeting-close has been applied to a
;; g-meeting object, in which case 'stop-iteration is returned.  Note
;; that if multiple g-meeting objects are passed to this procedure and
;; one of them is then closed, this procedure will return
;; 'stop-iteration and any wait will be abandonned.  It is usually a
;; bad idea to close a g-meeting object on which this procedure is
;; waiting where this procedure is selecting on more than one
;; g-meeting object.
;;
;; This procedure is first available in version 0.19 of this library.
(define (g-meeting-receive await resume . meetings)
  (when (null? meetings)
    (error "no g-meeting objects passed to g-meeting-receive procedure"))
  ;; We can only enter this procedure with repect to a g-meeting
  ;; object under two circumstances: either the status for at least
  ;; one g-meeting is 'set and resumptions for it is not empty, in
  ;; which case a sender is waiting and we can proceed, or status is
  ;; 'unset for all of them, which means that no sender is waiting and
  ;; we must add ourselves to all the g-meetings' queues and wait for
  ;; a sender.
  (let ((meeting (call/ec
		    (lambda (k)
		      (for-each (lambda (m)
				  (let ((status (status-get m)))
				    (when (eq? status 'set)
				      (k m))
				    (when (eq? status 'closed)
				      (k 'closed))))
				meetings)
		      #f))))
      (cond
       ((eq? meeting 'closed)
	'stop-iteration)
       (meeting
	;; if a g-meeting is set, extract the datum and return
	(let ((res (resumptions-get meeting)))
	  (when (q-empty? res)
	    (error "g-meeting-receive encountered a set g-meeting object with no sender resumption iterator"))
	  (let ((elt (deq! res)))
	    (g-idle-add (lambda ()
			  ((vector-ref elt 0) #f) #f)
			'default)
	    ((vector-ref elt 2))
	    (vector-ref elt 1))))
       (else
	;; if no g-meeting is set, insert the resume iterator in each
	;; g-meeting object and await a datum
	(letrec ((item (vector resume
			       #f
			       (lambda ()
				 (for-each (lambda (m)
					     (q-remove! (resumptions-get m) item))
					   meetings)))))
	  (for-each (lambda (m)
		      (enq! (resumptions-get m) item))
		    meetings)
	  (await))))))
