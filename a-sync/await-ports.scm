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
  #:use-module (ice-9 textual-ports)   ;; for put-string
  #:use-module (ice-9 binary-ports)    ;; for get-u8
  #:use-module (ice-9 control)         ;; for call/ec
  #:use-module (ice-9 suspendable-ports)
  #:use-module (a-sync event-loop)     ;; for event loop
  #:use-module (rnrs bytevectors)      ;; for bytevectors
  #:export (await-read-suspendable!
	    await-write-suspendable!
	    await-getline!
	    await-geteveryline!
	    await-getsomelines!
	    await-getblock!
	    await-geteveryblock!
	    await-getsomeblocks!
	    await-put-bytevector!
	    await-put-string!))

(install-suspendable-ports!)

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when it is invoked, and is intended to use the
;; port's normal read procedures.  'port' must be a suspendable
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
;; in packet mode seeing state change in a slave.  In the absence of
;; an exceptional condition, the value returned by 'proc' will be
;; returned.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, from 'proc' because of port or conversion errors)
;; will propagate out of this procedure in the first instance, and if
;; not caught locally will then propagate out of event-loop-run!.
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
;; by this procedure and the read operation will be abandonned.  See
;; the documentation on the await-read-suspendable!  procedure for
;; further particulars about this procedure.
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

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement this kind of functionality with await-read-suspendable!
;; (and is implemented by await-read-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and reads a block of data, such as a binary record, of size
;; 'size' from a non-blocking suspendable port 'port'.  This procedure
;; and will return a pair, normally comprising as its car a bytevector
;; of length 'size' containing the data, and as its cdr the number of
;; bytes received and placed in the bytevector (which will be the same
;; as 'size' unless an end-of-file object was encountered part way
;; through receiving the data).  If an exceptional condition ('excpt)
;; is encountered, a pair comprising (#f . #f) will be returned.  If
;; an end-of-file object is encountered without any bytes of data, a
;; pair with eof-object as car and #f as cdr will be returned.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; See the documentation on the await-read-suspendable! procedure for
;; further particulars about this procedure.
;;
;; This procedure is first available in version 0.6 of this library.
(define await-getblock!
  (case-lambda
    ((await resume port size)
     (await-getblock! await resume #f port size))
    ((await resume loop port size)
     ;; we cannot use get-bytevector-n! because it is not async-safe
     ;; in suspendable ports, so build the bytevector by hand
     (define bv (make-bytevector size))
     (define index 0)
     (await-read-suspendable! await resume loop port
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
					    (next (get-u8 p)))))))))))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement this kind of functionality with await-read-suspendable!
;; (and is implemented by await-read-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and and will apply 'proc' to any block of data received,
;; such as a binary record.  'proc' should be a procedure taking two
;; arguments, first a bytevector of length 'size' containing the block
;; of data read and second the size of the block of data placed in the
;; bytevector.  The value passed as the size of the block of data
;; placed in the bytevector will always be the same as 'size' unless
;; end-of-file has been encountered after receiving only a partial
;; block of data.  The watch will not end until end-of-file or an
;; exceptional condition ('excpt) is reached.  In the event of that
;; happening, this procedure will end and return an end-of-file object
;; or #f respectively.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; See the documentation on the await-read-suspendable! procedure for
;; further particulars about this procedure.
;;
;; This procedure is first available in version 0.6 of this library.
(define await-geteveryblock!
  (case-lambda
    ((await resume port size proc) (await-geteveryblock! await resume #f port size proc))
    ((await resume loop port size proc)
     ;; we cannot use get-bytevector-n! because it is not async-safe
     ;; in suspendable ports, so build the bytevector by hand
     (define bv (make-bytevector size))
     (define index 0)
     (await-read-suspendable! await resume loop port
			      (lambda (p)
				(let next ((u8 (get-u8 p)))
				  (if (eof-object? u8)
				      (begin
					(when (> index 0)
					  (proc bv index))
					u8)
				      (begin
					(bytevector-u8-set! bv index u8)
					(set! index (1+ index))
					(when (= index size)
					  (set! index 0)
					  (proc bv size))
					(next (get-u8 p))))))))))

;; This procedure is intended to be called in a waitable procedure
;; invoked by a-sync, and does the same as await-geteveryblock!,
;; except that it provides a third argument to 'proc', namely an
;; escape continuation which can be invoked by 'proc' to cause the
;; procedure to return before end-of-file is reached.  Behavior is
;; identical to await-geteveryblock! if the continuation is not
;; invoked.
;;
;; This procedure will apply 'proc' to any block of data received,
;; such as a binary record.  'proc' should be a procedure taking three
;; arguments, first a bytevector of length 'size' containing the block
;; of data read, second the size of the block of data placed in the
;; bytevector and third an escape continuation.  The value passed as
;; the size of the block of data placed in the bytevector will always
;; be the same as 'size' unless end-of-file has been encountered after
;; receiving only a partial block of data.  The watch will not end
;; until end-of-file or an exceptional condition ('excpt) is reached,
;; which would cause this procedure to end and return an end-of-file
;; object or #f respectively, or until the escape continuation is
;; invoked, in which case the value passed to the escape continuation
;; will be returned.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; See the documentation on the await-read-suspendable! procedure for
;; further particulars about this procedure.
;;
;; This procedure is first available in version 0.6 of this library.
(define await-getsomeblocks!
  (case-lambda
    ((await resume port size proc) (await-getsomeblocks! await resume #f port size proc))
    ((await resume loop port size proc)
     ;; we cannot use get-bytevector-n! because it is not async-safe
     ;; in suspendable ports, so build the bytevector by hand
     (define bv (make-bytevector size))
     (define index 0)
     (await-read-suspendable! await resume loop port
			      (lambda (p)
				(call/ec
				 (lambda (k)
				   (let next ((u8 (get-u8 p)))
				     (if (eof-object? u8)
					 (begin
					   (when (> index 0)
					     (proc bv index k))
					   u8)
					 (begin
					   (bytevector-u8-set! bv index u8)
					   (set! index (1+ index))
					   (when (= index size)
					     (set! index 0)
					     (proc bv size k))
					   (next (get-u8 p))))))))))))

;; 'proc' is a procedure taking a single argument, to which the port
;; will be passed when it is invoked, and is intended to use the
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
;; occur.  In the absence of an exceptional condition, the value
;; returned by 'proc' will be returned.
;;
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.
;;
;; This procedure must (like the a-sync procedure) be called in the
;; same thread as that in which the event loop runs.
;;
;; Exceptions (say, from 'proc' because of port or conversion errors)
;; will propagate out of this procedure in the first instance, and if
;; not caught locally will then propagate out of event-loop-run!.
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

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-write-suspendable! (and is implemented by
;; await-write-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and will write the contents of bytevector 'bv' to 'port'.
;; The 'loop' argument is optional: this procedure operates on the
;; event loop passed in as an argument, or if none is passed (or #f is
;; passed), on the default event loop.  If an exceptional condition
;; ('excpt) is encountered by the implementation, #f will be returned
;; by this procedure and the write operation will be abandonned,
;; otherwise #t will be returned.  However exceptional conditions on
;; write ports cannot normally occur.
;;
;; The port will be flushed by this procedure upon conclusion of the
;; writing of the string.
;;
;; See the documentation on the await-write-suspendable! procedure for
;; further particulars about this procedure.
;;
;; This procedure is first available in version 0.6 of this library.
(define await-put-bytevector!
  (case-lambda
    ((await resume port bv) (await-put-bytevector! await resume #f port bv))
    ((await resume loop port bv)
     (await-write-suspendable! await resume loop port
			       (lambda (p)
				 (put-bytevector p bv)
				 ;; enforce a flush when the current
				 ;; write-waiter is still in operation
				 (force-output p)
				 #t)))))

;; This procedure is provided mainly to retain compatibility with the
;; guile-a-sync library for guile-2.0, because it is trivial to
;; implement with await-write-suspendable! (and is implemented by
;; await-write-suspendable!).
;;
;; It is intended to be called in a waitable procedure invoked by
;; a-sync, and will write the string 'text' to 'port'.  The 'loop'
;; argument is optional: this procedure operates on the event loop
;; passed in as an argument, or if none is passed (or #f is passed),
;; on the default event loop.  If an exceptional condition ('excpt) is
;; encountered by the implementation, #f will be returned by this
;; procedure and the write operation will be abandonned, otherwise #t
;; will be returned.  However exceptional conditions on write ports
;; cannot normally occur.
;;
;; The port will be flushed by this procedure upon conclusion of the
;; writing of the string.
;;
;; If CR-LF line endings are to be written when outputting the string,
;; the '\r' character (as well as the '\n' character) must be embedded
;; in the string.
;;
;; See the documentation on the await-write-suspendable! procedure for
;; further particulars about this procedure.
;;
;; This procedure is first available in version 0.5 of this library.
(define await-put-string!
  (case-lambda
    ((await resume port text) (await-put-string! await resume #f port text))
    ((await resume loop port text)
     (await-write-suspendable! await resume loop port
			       (lambda (p)
				 (put-string p text)
				 ;; enforce a flush when the current
				 ;; write-waiter is still in operation
				 (force-output p)
				 #t)))))
