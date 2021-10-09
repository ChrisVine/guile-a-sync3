#!/usr/bin/env guile
!#

;; Copyright (C) 2016 to 2021 Chris Vine
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this file (the "Software"), to deal in the
;; Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute,
;; sublicense, and/or sell copies of the Software, and to permit
;; persons to whom the Software is furnished to do so, subject to the
;; following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is an example file for using asynchronous reads and writes on
;; sockets.  It will provide the caller's IPv4 internet address from
;; checkip.dyndns.com.  Normally if you wanted to do this from a
;; utility script, you would do it synchronously.  However in a
;; program using an event loop, you would need to do it
;; asynchronously.  This does so.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-modules
 (a-sync coroutines)
 (a-sync event-loop)
 (a-sync await-ports)
 (ice-9 textual-ports)
 (ice-9 regex)
 (web request)
 (web uri))

(define check-ip "checkip.dyndns.com")

;; unfortunately the read-response-body procedure in guile's (web
;; response) module calls get-bytevector-all, which is not
;; suspendable, so we have to write our own waitable procedure to read
;; the response instead - get-line and get-string-all are suspendable.
;; This procedure returns two values, first the header and second the
;; body.
(define (await-read-response await resume sock)
  (await-read-suspendable! await resume sock
			   (lambda (s)
			     (let lp ((header ""))
			       (let ((line (get-line s)))
				 (if (eof-object? line)
				     (values header "")
				     (let ((line (string-trim-right line #\cr)))
				       (if (string=? line "")
					   (let ((body (get-string-all s)))
					     (values header body))
					   (lp (if (string=? header "")
						   line
						   (string-append header "\n" line)))))))))))

(set-default-event-loop!)
(sigaction SIGPIPE SIG_IGN) ;; we want EPIPE, not SIGPIPE
(event-loop-block! #t)  ;; we invoke await-task-in-thread!

(a-sync
 (lambda (await resume)
   (let ((sock (socket PF_INET SOCK_STREAM 0))
	 (request (build-request (build-uri 'http 
					    #:host check-ip
					    #:port 80
					    #:path "/")
				 #:method 'GET
				 #:version '(1 . 1)
				 #:headers '((Connection . "close"))))
	 ;; getaddrinfo is not suspendable, so call it up with
	 ;; await-task-in-thread!, await-task-in-event-loop! or
	 ;; await-task-in-thread-pool!
	 (addr (await-task-in-thread! await resume
				      (lambda ()
					(addrinfo:addr (car (getaddrinfo check-ip "http")))))))

     ;; make socket non-blocking
     (fcntl sock F_SETFL (logior O_NONBLOCK
				 (fcntl sock F_GETFL)))
     ;; socket ports are unbuffered by default, so make the socket
     ;; buffered (as this is a socket, with no file position pointer,
     ;; keeping port buffers synchronized is not an issue, and we
     ;; flush the buffer when sending)
     (setvbuf sock 'block)

     ;; the connect and force-output procedures are suspendable in
     ;; guile-2.2, as is write-request if no custom header writers
     ;; are imported which invoke non-suspendable i/o
     (await-write-suspendable! await resume sock
			       (lambda (s)
				 (connect s addr)
				 (write-request request s)
				 (force-output s)))
     (call-with-values
	 (lambda ()
	   (await-read-response await resume sock))
       (lambda (header body)
	 (let ((ip (match:substring 
		    (string-match "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" body))))
	   (display ip)
	   (newline))))
     (event-loop-block! #f))))

(event-loop-run!)
