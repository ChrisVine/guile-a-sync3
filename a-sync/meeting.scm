;; Copyright (C) 2017 Chris Vine

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

(define-module (a-sync meeting)
  #:use-module (a-sync event-loop)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)  ;; for call/ec
  #:use-module (srfi srfi-9)
  #:export (make-meeting
	    meeting?
	    meeting-close
	    meeting-ready?
	    meeting-send
	    meeting-receive))


(define-record-type <meeting>
  (_make-meeting resumptions loop status)
  meeting?
  (resumptions resumptions-get)
  (loop loop-get)
  (status status-get status-set!))

;; This procedure makes and returns a meeting object.  Meetings are
;; objects on which a-sync or compose-a-sync blocks running on the
;; same event loop can synchronize by one passing a datum to another.
;; The 'loop' argument specifies the event loop (as constructed by
;; make-event-loop in the (a-sync event-loop module)) with respect to
;; which the meeting will be held: it is an error if the meeting-send
;; or meeting-receive procedures are passed a different event loop as
;; an argument.  The 'loop' argument is optional - if none is passed,
;; or #f is passed, then the default event loop will be chosen.
;;
;; Strictly speaking this procedure can be called in any native OS
;; thread, but since it carries out no synchronization of native
;; threads the user would have to provide her own synchronization if
;; called in other than the thread of the event loop with respect to
;; which the meeting will be held; so it is best if this procedure is
;; called in the thread of that event loop.
(define* (make-meeting #:optional loop)
  (let ((loop (or loop (get-default-event-loop))))
    (when (not loop)
      (error "No default event loop set for call to make-meeting"))
    (_make-meeting (make-q) loop 'unset)))

;; This closes a meeting object.  It's purpose is to wake up any
;; "pseudo-thread" (that is, any a-sync or compose-a-sync block)
;; waiting in meeting-send or meeting-receive by causing either
;; procedure to return with a 'stop-iteration value.
;;
;; Where that is not necessary (say, the receiver already knows how
;; many items are to be sent), then this procedure does not need to be
;; applied.  It is not needed in order to release resources.
(define (meeting-close m)
  (let ((res (resumptions-get m))
	(loop (loop-get m)))
    (when (not (q-empty? res))
      (let lp ((elt (deq! res)))
	(event-post! (lambda () ((vector-ref elt 0) 'stop-iteration))
		     loop)
	((vector-ref elt 2)) ;; clean up all other meeting objects waiting with this one
	(when (not (q-empty? res))
	  (lp (deq! res)))))
    (status-set! m 'closed)))

;; This indicates whether applying message-send or message-receive (as
;; the case may be) to the meeting object 'm' will return immediately:
;; in other words, this procedure will return #t if another a-sync or
;; compose-a-sync block is already waiting on the object or the
;; meeting object has been closed, otherwise #f.
(define (meeting-ready? m)
  (or (not (q-empty? (resumptions-get m)))
      (eq? (status-get m) 'closed)))

;; The signature of this procedure is:
;;
;;   (meeting-send await resume [loop] m0 [m1 ...] datum)
;;
;; This sends a datum to a receiver which is running on the same event
;; loop as the sender, via one or more meeting objects 'm0 m1 ...'.
;; If no receiver is waiting for the datum, this procedure waits until
;; a receiver calls meeting-receive on one of the meeting objects to
;; request the datum.  If a receiver is already waiting, this
;; procedure passes on the datum and returns immediately.
;;
;; The 'loop' argument is optional.  If not supplied, or #f is passed,
;; this procedure will use the default event loop.  It is an error if
;; this procedure is given a different event loop than the one which
;; was passed to make-meeting on constructing the 'meeting' objects.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; Multiple senders may wait on a meeting object to permit fan in.
;; The provided datum of each sender will be passed to a receiver (as
;; and when a receiver becomes available) in the order in which this
;; procedure was invoked.
;;
;; In addition, this procedure has 'select'-like behavior: multiple
;; meeting objects may be passed and this procedure will send to the
;; first one which becomes available to receive the datum.
;;
;; Once a datum exchange has taken place, the meeting object(s) can be
;; reused for making another exchange (provided the meeting objects
;; have not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; event loop concerned runs.  To have other native OS threads
;; communicate with an event-loop, use await-task-in-thread!,
;; await-task-in-event-loop!, await-generator-in-thread! or
;; await-generator-in-event-loop!.
;;
;; This procedure always returns #f unless meeting-close has been
;; applied to a meeting object, in which case 'stop-iteration is
;; returned.  Note that if multiple meeting objects are passed to this
;; procedure and one of them is then closed, this procedure will
;; return 'stop-iteration and any wait will be abandonned.  It is
;; usually a bad idea to close a meeting object on which this
;; procedure is waiting where this procedure is selecting on more than
;; one meeting object.
(define (meeting-send await resume . rest)
  (cond
   ((null? rest)
    (error "arity error for meeting-send procedure"))
   ((event-loop? (car rest))
    (cond
     ((or (null? (cdr rest))
	  (not (meeting? (cadr rest))))
      (error "no meeting objects passed to meeting-send procedure"))
     ((null? (cddr rest))
      (error "no datum passed to meeting-send procedure"))
     (else
      (apply meeting-send-impl await resume rest))))
   (else
    (cond
     ((not (meeting? (car rest)))
      (error "no meeting objects passed to meeting-send procedure"))
     ((null? (cdr rest))
      (error "no datum passed to meeting-send procedure"))
     (else
      (apply meeting-send-impl await resume #f rest))))))
  
(define (meeting-send-impl await resume loop . rest)
  (let ((loop (or loop (get-default-event-loop))))
    (when (not loop) 
      (error "No default event loop set for call to meeting-send"))
    (let ((split (- (length rest) 1)))
      (let ((meetings (list-head rest split))
	    (datum (car (list-tail rest split))))
	;; If resumptions is not empty for one or more meetings whose
	;; status is not at 'set then it must contain resumption
	;; iterators for one or more waiting receivers so we can
	;; proceed immediately.  Otherwise all the meeting objects
	;; must either have status already at 'set so another sender
	;; is already waiting on that meeting for a receiver, or the
	;; meeting's resumptions are empty so nothing is waiting: in
	;; either case we should also add ourselves to all the
	;; meetings' queues and wait for a receiver.
	(let ((meeting (call/ec
			(lambda (k)
			  (for-each (lambda (m)
				      (let ((status (status-get m)))
					(when (not (eq? loop (loop-get m))) 
					  (error "meeting-send passed an event loop object for which the meeting was not constructed"))
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
	    ;; if a meeting is both unset and has resumption
	    ;; iterators, send the datum to a waiting receiver and
	    ;; return
	    (let ((elt (deq! (resumptions-get meeting))))
	      (event-post! (lambda ()
			     ((vector-ref elt 0) datum))
			   loop)
	      ((vector-ref elt 2))
	      #f))
	   (else
	    ;; if no meeting is unset with resumption iterators,
	    ;; insert the resume iterator in each meeting object and
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
	      (await)))))))))

;; The signature of this procedure is:
;;
;;   (meeting-receive await resume [loop] m0 [m1 ...])
;;
;; This receives a datum from a sender running on the same event loop
;; as the receiver, via one or more meeting objects 'm0 m1 ...'.  If
;; no sender is waiting to pass the datum, this procedure waits until
;; a sender calls meeting-send on one of the meeting objects to
;; provide the datum.  If a sender is already waiting, this procedure
;; returns immediately with the datum supplied.
;;
;; The 'loop' argument is optional.  If not supplied, or #f is passed,
;; this procedure will use the default event loop.  It is an error if
;; this procedure is given a different event loop than the one which
;; was passed to make-meeting on constructing the 'meeting' objects.
;;
;; This procedure is intended to be called within a waitable procedure
;; invoked by a-sync (which supplies the 'await' and 'resume'
;; arguments).
;;
;; Multiple receivers may wait on a meeting object to permit fan out.
;; The waiting receivers will be released (as and when a sender
;; provides a datum) in the order in which this procedure was invoked.
;;
;; In addition, this procedure has 'select'-like behavior: multiple
;; meeting objects may be passed and this procedure will receive from
;; the first one which sends a datum.
;;
;; Once a datum exchange has taken place, the meeting object(s) can be
;; reused for making another exchange (provided the meeting objects
;; have not been closed).
;;
;; This procedure must be called in the native OS thread in which the
;; event loop concerned runs.  To have other native OS threads
;; communicate with an event-loop, use await-task-in-thread!,
;; await-task-in-event-loop!, await-generator-in-thread! or
;; await-generator-in-event-loop!.
;;
;; This procedure always returns the datum value supplied by
;; meeting-send unless meeting-close has been applied to a meeting
;; object, in which case 'stop-iteration is returned.  Note that if
;; multiple meeting objects are passed to this procedure and one of
;; them is then closed, this procedure will return 'stop-iteration and
;; any wait will be abandonned.  It is usually a bad idea to close a
;; meeting object on which this procedure is waiting where this
;; procedure is selecting on more than one meeting object.
(define (meeting-receive await resume . rest)
  (cond
   ((null? rest)
    (error "no meeting objects passed to meeting-receive procedure"))
   ((event-loop? (car rest))
    (if (null? (cdr rest))
	(error "no meeting objects passed to meeting-receive procedure")
	(apply meeting-receive-impl await resume rest)))
   (else
    (apply meeting-receive-impl await resume #f rest))))
  
(define (meeting-receive-impl await resume loop . meetings)
  (let ((loop (or loop (get-default-event-loop))))
    (when (not loop) 
      (error "No default event loop set for call to meeting-receive"))
    ;; We can only enter this procedure with repect to a meeting
    ;; object under two circumstances: either the status for at leasts
    ;; one meeting is 'set and resumptions for it is not empty, in
    ;; which case a sender is waiting and we can proceed, or status is
    ;; 'unset for all of them, which means that no sender is waiting
    ;; and we must add ourselves to all the meetings' queues and wait
    ;; for a sender.
    (let ((meeting (call/ec
		    (lambda (k)
		      (for-each (lambda (m)
				  (let ((status (status-get m)))
				    (when (not (eq? loop (loop-get m))) 
				      (error "meeting-receive passed an event loop object for which the meeting was not constructed"))
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
	;; if a meeting is set, extract the datum and return
	(let ((res (resumptions-get meeting)))
	  (when (q-empty? res)
	    (error "meeting-receive encountered a set meeting object with no sender resumption iterator"))
	  (let ((elt (deq! res)))
	    (event-post! (lambda ()
			   ((vector-ref elt 0) #f))
			 loop)
	    ((vector-ref elt 2))
	    (vector-ref elt 1))))
       (else
	;; if no meeting is set, insert the resume iterator in each
	;; meeting object and await a datum
	(letrec ((item (vector resume
			       #f
			       (lambda ()
				 (for-each (lambda (m)
					     (q-remove! (resumptions-get m) item))
					   meetings)))))
	  (for-each (lambda (m)
		      (enq! (resumptions-get m) item))
		    meetings)
	  (await)))))))
