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

(define-module (a-sync await-ports)
  #:use-module (ice-9 rdelim)          ;; for read-line
  #:use-module (ice-9 control)         ;; for call/ec
  #:use-module (ice-9 suspendable-ports)
  #:use-module (a-sync event-loop)     ;; for event loop
  #:export (await-read-suspendable!
	    await-write-suspendable!
	    await-getline!
	    await-geteveryline!
	    await-getsomelines!))

(install-suspendable-ports!)

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when func is invoked.  'port' must be a suspendable
;; non-blocking port.  'proc' will be executed whenever there is
;; something available to read, and this procedure will return when
;; 'proc' returns, as if by a blocking read.  The event loop will not
;; be blocked by this procedure even if only individual characters or
;; bytes comprising part characters are available at any one time.  It
;; is intended to be called in a waitable procedure invoked by a-sync.
;; If an exceptional condition ('excpt) is encountered by the
;; implementation, #f will be returned by this procedure and the read
;; operations to be performed by 'proc' will be abandonned; there is
;; however no guarantee that any exceptional condition that does arise
;; will be encountered by the implementation - the user procedure
;; 'proc' may get there first and deal with it, or it may not.
;; However exceptional conditions are very rare, usually comprising
;; only out-of-band data on a TCP socket, or a pseudoterminal master
;; in packet mode seeing state change in a slave.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, because of port or conversion errors) will
;; propagate out of this procedure in the first instance, and if not
;; caught locally will then propagate out of event-loop-run!.
(define await-read-suspendable!
  (case-lambda
    ((await resume port proc)
     (await-read-suspendable! await resume #f port proc))
    ((await resume loop port proc)
     (define (read-waiter p)
       ;;(display "Awaiting\n")
       (when (eq? (await) 'except)
	 (throw 'except)))
     (event-loop-add-read-watch! port
				 (lambda (status)
				   (if (eq? status 'except)
				       (resume 'except)
				       (resume))
				   #t)
				 loop)
     (let ((res
	    (parameterize ((current-read-waiter read-waiter))
	      (catch #t
		(lambda ()
		  (proc port))
		(lambda args
		  (if (eq? (car args) 'except)
		      #f
		      (begin
			(event-loop-remove-read-watch! port loop)
			(apply throw args))))))))
       (event-loop-remove-read-watch! port loop)
       res))))
     
;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-read-suspendable! (and is implemented by
;; await-read-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and reads a line of text from a non-blocking suspendable
;; port and returns it (without the terminating '\n' character).  The
;; 'loop' argument is optional: this procedure operates on the event
;; loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.  If an exceptional condition
;; ('excpt) is encountered by the implementation, #f will be returned
;; by this procedure and the read operations to be performed by 'proc'
;; will be abandonned.  See the documentation on the
;; await-read-suspendable!  procedure for further particulars about
;; this procedure.
(define await-getline!
  (case-lambda
    ((await resume port)
     (await-getline! await resume #f port))
    ((await resume loop port)
     (await-read-suspendable! await resume loop port
			      (lambda (p)
				(read-line p))))))
	 
;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-read-suspendable! (and is implemented by
;; await-read-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and will apply 'proc' to every complete line of text
;; received (without the terminating '\n' character).  The watch will
;; not end until end-of-file or an exceptional condition ('excpt) is
;; reached.  In the event of that happening, this procedure will end
;; and return an end-of-file object or #f respectively.  The 'loop'
;; argument is optional: this procedure operates on the event loop
;; passed in as an argument, or if none is passed (or #f is passed),
;; on the default event loop.  See the documentation on the
;; await-read-suspendable! procedure for further particulars about
;; this procedure.
(define await-geteveryline!
  (case-lambda
    ((await resume port proc)
     (await-geteveryline! await resume #f port proc))
    ((await resume loop port proc)
     (await-read-suspendable! await resume loop port
			      (lambda (p)
				(let next ((line (read-line p)))
				  (if (or (eof-object? line)
					  (not line))
				      line
				      (begin
					(proc line)
					(next (read-line p))))))))))

;; This procedure is intended to be called in a waitable procedure
;; invoked by a-sync, and does the same as await-geteveryline!, except
;; that it provides a second argument to 'proc', namely an escape
;; continuation which can be invoked by 'proc' to cause the procedure to
;; return before end-of-file is reached.  Behavior is identical to
;; await-geteveryline! if the continuation is not invoked.
;;
;; This procedure will apply 'proc' to every complete line of text
;; received (without the terminating '\n' character).  The watch will
;; not end until end-of-file or an exceptional condition ('excpt) is
;; reached, which would cause this procedure to end and return an
;; end-of-file object or #f respectively, or until the escape
;; continuation is invoked, in which case the value passed to the
;; escape continuation will be returned.  The 'loop' argument is
;; optional: this procedure operates on the event loop passed in as an
;; argument, or if none is passed (or #f is passed), on the default
;; event loop.  See the documentation on the await-read-suspendable!
;; procedure for further particulars about this procedure.
(define await-getsomelines!
  (case-lambda
    ((await resume port proc)
     (await-getsomelines! await resume #f port proc))
    ((await resume loop port proc)
     (await-read-suspendable! await resume loop port
			      (lambda (p)
				(call/ec
				 (lambda (k)
				   (let next ((line (read-line p)))
				     (if (or (eof-object? line)
					     (not line))
					 line
					 (begin
					   (proc line k)
					   (next (read-line p))))))))))))

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when func is invoked, and is intended to use the
;; port's normal write procedures.  'port' must be a suspendable
;; non-blocking port.  'proc' will be executed whenever the port is
;; available to write to, and this procedure will return when 'proc'
;; returns, as if by a blocking write.  The event loop will not be
;; blocked by this procedure even if only individual characters or
;; bytes comprising part characters can be written at any one time.
;; It is intended to be called in a waitable procedure invoked by
;; a-sync.  If an exceptional condition ('excpt) is encountered by the
;; implementation, #f will be returned by this procedure and the write
;; operations to be performed by 'proc' will be abandonned; there is
;; however no guarantee that any exceptional condition that does arise
;; will be encountered by the implementation - the user procedure
;; 'proc' may get there first and deal with it, or it may not.
;; However exceptional conditions on write ports cannot normally
;; occur.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, because of port or conversion errors) will
;; propagate out of this procedure in the first instance, and if not
;; caught locally will then propagate out of event-loop-run!.
(define await-write-suspendable!
  (case-lambda
    ((await resume port proc)
     (await-write-suspendable! await resume #f port proc))
    ((await resume loop port proc)
     (define (write-waiter p)
       ;;(display "Awaiting\n")
       (when (eq? (await) 'except)
	 (throw 'except)))
     (event-loop-add-write-watch! port
				  (lambda (status)
				    (if (eq? status 'except)
					(resume 'except)
					(resume))
				    #t)
				  loop)
     (let ((res
	    (parameterize ((current-write-waiter write-waiter))
	      (catch #t
		(lambda ()
		  (proc port))
		(lambda args
		  (if (eq? (car args) 'except)
		      #f
		      (begin
			(event-loop-remove-write-watch! port loop)
			(apply throw args))))))))
       (event-loop-remove-write-watch! port loop)
       res))))
