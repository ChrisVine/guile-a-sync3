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

(use-modules (a-sync coroutines)
	     (a-sync event-loop)
	     (a-sync compose)
	     (a-sync await-ports)
	     (ice-9 threads)      ;; for call-with-new-thread
	     (ice-9 rdelim)       ;; for write-line and read-string
	     (ice-9 control)      ;; for call/ec
	     (ice-9 binary-ports) ;; for get-bytevector-all and put-bytevector
	     (ice-9 exceptions)   ;; for raise-exception
	     (rnrs base)          ;; for assert
	     (rnrs bytevectors))  ;; for bytevectors

;; helpers

(define-syntax-rule (test-result expected res)
  (assert (eqv? expected res)))

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       ;; the current-filename procedure does not work in guile-2.1
       ;; when looking up a file via the GUILE_LOAD_PATH environmental
       ;; variable, so state the file name explicitly
       (simple-format #t "~A: Test ~A OK\n" "test-helpers-await.scm" count)
       (set! count (1+ count))))))

;; bv1 must be smaller than or equal to bv2 to obtain a #t result
(define (test-bytevector bv1 bv2)
  (if (> (bytevector-length bv1) (bytevector-length bv2))
      #f
      (let ((sz (bytevector-length bv1)))
	(call/ec (lambda (k)
		   (let loop ((index 0))
		     (cond
		      ((= index sz)
		       #t)
		      ((= (bytevector-s8-ref bv1 index)
			  (bytevector-s8-ref bv2 index))
		       (loop (1+ index)))
		      ((k #f)))))))))

;; Test 1: await-task!

(define main-loop (make-event-loop))

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! await resume main-loop
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)

;; Test 2: await-task-in-thread! without handler

(event-loop-block! #t main-loop)
(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume main-loop
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    ;; we can apply event-loop-quit! here as we know the loop is running
	    (event-loop-quit! main-loop))))
(event-loop-run! main-loop)
  
;; Test 3: await-task-in-thread! with handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread!
		  await resume main-loop
		  (lambda () (raise-exception 'test-exception))
		  (lambda (obj)
		    (test-result 'test-exception obj)
		    5))))
	    (test-result 5 res)
	    (print-result)
	    ;; we can apply event-loop-quit! here as we know the loop is running
	    (event-loop-quit! main-loop))))
(event-loop-run! main-loop)

;; Test 4: await-task-in-event-loop!

(let ()
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t main-loop)
  (event-loop-block! #t worker)

  (call-with-new-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (let ((res
		   (await-task-in-event-loop! await resume main-loop worker
					      (lambda ()
						(+ 5 10)))))
	      (test-result 15 res)
	      (print-result)
	      (event-loop-block! #f main-loop)
	      (event-loop-block! #f worker))))
  (event-loop-run! main-loop))

;; Test 5: await-yield!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-yield! await resume main-loop)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 6: await-generator!

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (await-generator! await resume main-loop
			      (lambda (yield)
				(let loop ((count 0))
				  (when (< count 5)
				    (yield (* 2 count))
				    (loop (1+ count)))))
			      (lambda (val)
				(set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)))
  (event-loop-run! main-loop))

;; Test 7: await-generator-in-thread! without handler

(let ()
  (define lst '())
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread! await resume main-loop
					       (lambda (yield)
						 (let loop ((count 0))
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (1+ count)))))
					       (lambda (val)
						 (set! lst (cons val lst))))))
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop))

;; Test 8: await-generator-in-thread! with handler

(let ()
  (define lst '())
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread! await resume main-loop
					       (lambda (yield)
						 (let loop ((count 0))
						   (cond
						    ((< count 5)
						     (yield (* 2 count))
						     (loop (1+ count)))
						    ((= count 5)
						     (raise-exception 'my-exception)
						     ;; we never reach here
						     (yield (* 2 count))
						     (loop (1+ count)))
						    (else
						     (assert #f))))) ;; we should never reach here
					       (lambda (val)
						 (set! lst (cons val lst)))
					       (lambda (obj)
						 (test-result obj 'my-exception)
						 (set! lst (cons 100 lst))))))
	      (test-result (car lst) 100)
	      (test-result (length lst) 6)
	      (test-result res 'guile-a-sync-thread-error)
	      (print-result)
	      (event-loop-block! #f main-loop))))
  (event-loop-run! main-loop))

;; Test 9: await-generator-in-event-loop!

(let ()
  (define lst '())
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t main-loop)
  (event-loop-block! #t worker)

  (call-with-new-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (await-generator-in-event-loop! await resume main-loop worker
					    (lambda (yield)
					      (let loop ((count 0))
						(when (< count 5)
						  (yield (* 2 count))
						  (loop (1+ count)))))
					    (lambda (val)
					      (set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)
	    (event-loop-block! #f main-loop)))
  (event-loop-run! main-loop))

;; Test 10: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume main-loop 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)
  
;; Test 11: await-sleep!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-sleep! await resume main-loop 100)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 12: await-getline! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getline! await resume
				       main-loop
				       in)))
	      (assert (string=? res "test-string"))
	      (print-result))))
  (write-line "test-string" out)
  (force-output out)
  (event-loop-run! main-loop))

;; Test 13: await-geteveryline! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryline! await resume
					    main-loop
					    in
					    (lambda (line)
					      (set! count (1+ count))
					      (when (= count 1)
						(assert (string=? line "test-string1")))
					      (when (= count 2)
						(assert (string=? line "test-string2"))
						(close out))))))
	      (assert (eof-object? res))
	      (test-result 2 count)
	      (print-result))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (force-output out)
  (event-loop-run! main-loop))

;; Test 14: await-getsomelines! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomelines! await resume
					    main-loop
					    in
					    (lambda (line k)
					      (set! count (1+ count))
					      (when (= count 1)
						    (assert (string=? line "test-string1")))
					      (when (= count 2)
						    (assert (string=? line "test-string2")))
					      (when (= count 3)
						    (assert (string=? line "test-string3"))
						    (k 'exit-await))))))
	      (assert (eq? res 'exit-await))
	      (test-result 3 count)
	      (print-result))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (event-loop-run! main-loop)
  (close out))

;; Test 15: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception propagates out of event-loop-run!
(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (await-getsomelines! await resume
				 main-loop
				 in
				 (lambda (line k)
				   (set! count (1+ count))
				   (when (= count 1)
					 (assert (string=? line "test-string1")))
				   (when (= count 2)
					 (throw 'exit-exception))
				   (when (= count 3)
					 (assert #f)))) ;; we should never reach here
	    (assert #f))) ;; we should never reach here
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (catch #t
	 (lambda ()
	   (event-loop-run! main-loop)
	   (assert #f)) ;; we should never reach here
	 (lambda args
	   (assert (eq? (car args) 'exit-exception))))
  (close out)
  (test-result 2 count)
  (print-result))

;; Test 16: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception caught within a-sync block
(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (catch #t
		   (lambda ()
		     (await-getsomelines! await resume
					  main-loop
					  in
					  (lambda (line k)
					    (set! count (1+ count))
					    (when (= count 1)
						  (assert (string=? line "test-string1")))
					    (when (= count 2)
						  (throw 'exit-exception))
					    (when (= count 3)
						  (assert #f))))   ;; we should never reach here
		     (assert #f))   ;; we should never reach here
		   (lambda args
		     (assert (eq? (car args) 'exit-exception))))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (event-loop-run! main-loop)
  (close out)
  (test-result 2 count)
  (print-result))

;; Test 17: await-getblock! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getblock! await resume
					main-loop
					in
					10)))
	      (test-result 10 (cdr res))
	      (assert (test-bytevector bv1 (car res))))
	    (let ((res (await-getblock! await resume
					main-loop
					in
					20)))
	      (test-result 10 (cdr res))
	      (test-result 20 (bytevector-length (car res)))
	      (assert (test-bytevector bv2 (car res))))
	    (let ((res (await-getblock! await resume
					main-loop
					in
					10)))
	      (assert (eof-object? (car res)))
	      (test-result #f (cdr res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
            (close-port out)))
  (event-loop-run! main-loop)
  (close-port in))

;; Test 18: await-geteveryblock! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryblock! await resume
					     main-loop
					     in
					     20
					     (lambda (bv-in len)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 20 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv3 bv-in)))))))
	      (assert (eof-object? res)))
	    (let ((res (await-geteveryblock! await resume
					     main-loop
					     in
					     10
					     (lambda (bv-in len)
					       (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (event-loop-run! main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 19: await-getsomeblocks! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomeblocks! await resume
					     main-loop
					     in
					     10
					     (lambda (bv-in len k)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 10 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv2 bv-in))
						     (k 'test))))))
	      (test-result 'test res))
	    (let ((res (await-getsomeblocks! await resume
	    				     main-loop
	    				     in
					     20
	    				     (lambda (bv-in len k)
					       (test-result 10 len)
					       (assert (test-bytevector bv3 bv-in))))))
	      (assert (eof-object? res)))
	    (let ((res (await-getsomeblocks! await resume
	    				     main-loop
	    				     in
					     10
	    				     (lambda (bv-in len k)
	    				       (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-timeout! await resume main-loop 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (event-loop-run! main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 20: await-put-bytevector! (also tests await-write-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (define bv (make-bytevector 10 55))
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume main-loop
						       (lambda ()
							 (get-bytevector-all in))))
		      (event-loop-block! #f main-loop)))
	    (await-put-bytevector! await resume main-loop out bv)
	    (close-port out)))
  (event-loop-run! main-loop)
  (test-result (bytevector-length res) 10)
  (test-bytevector res bv)
  (close-port in)
  (print-result))

;; Test 21: await-put-string! (also tests await-write-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (event-loop-block! #t main-loop)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume main-loop
						       (lambda ()
							 (read-string in))))
		      (event-loop-block! #f main-loop)))
	    (await-put-string! await resume main-loop out (string #\a #\b #\c))
	    (close-port out)))
  (event-loop-run! main-loop)
  (test-result (string-length res) 3)
  (test-result (string-ref res 0) #\a)
  (test-result (string-ref res 1) #\b)
  (test-result (string-ref res 2) #\c)
  (close-port in)
  (print-result))

;; Test 22: compose-a-sync and no-await

(event-loop-block! #t main-loop)
(compose-a-sync main-loop ((res (await-task-in-thread! (lambda ()
							 (+ 5 10)))))
	      ((no-await (test-result 15 res)
			 (print-result)
			 ;; we can apply event-loop-quit! here as we know the loop is running
			 (event-loop-quit! main-loop))))
(event-loop-run! main-loop)

;;;;;;;;;; now the same tests with a default event loop ;;;;;;;;;;

(event-loop-block! #f main-loop)
(set-default-event-loop! main-loop)

;; Test 23: await-task!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! await resume
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)

;; Test 24: await-task-in-thread! without handler

;; set a new default event loop
(set-default-event-loop!)

(event-loop-block! #t)
(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    ;; we can apply event-loop-quit! here as we know the loop is running
	    (event-loop-quit!))))
(event-loop-run!)
  
;; Test 25: await-task-in-thread! without handler (explicit loop argument)

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! await resume #f
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    ;; we can apply event-loop-quit! here as we know the loop is running
	    (event-loop-quit!))))
(event-loop-run!)

;; Test 26: await-task-in-thread! with handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread!
		  await resume
		  (lambda () (raise-exception 'test-exception))
		  (lambda (obj)
		    (test-result 'test-exception obj)
		    5))))
	    (test-result 5 res)
	    (print-result)
	    ;; we can apply event-loop-quit! here as we know the loop is running
	    (event-loop-quit!))))
(event-loop-run!)

;; Test 27: await-task-in-event-loop!

(let ()
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t)
  (event-loop-block! #t worker)

  (call-with-new-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (let ((res
		   (await-task-in-event-loop! await resume worker
					      (lambda ()
						(+ 5 10)))))
	      (test-result 15 res)
	      (print-result)
	      (event-loop-block! #f)
	      (event-loop-block! #f worker))))
  (event-loop-run!))

;; Test 28: await-yield!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-yield! await resume)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run!))

;; Test 29: await-generator!

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (await-generator! await resume
			      (lambda (yield)
				(let loop ((count 0))
				  (when (< count 5)
				    (yield (* 2 count))
				    (loop (1+ count)))))
			      (lambda (val)
				(set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)))
  (event-loop-run!))

;; Test 30: await-generator-in-thread! without handler

(let ()
  (define lst '())
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread! await resume
					       (lambda (yield)
						 (let loop ((count 0))
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (1+ count)))))
					       (lambda (val)
						 (set! lst (cons val lst))))))
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-run!))

;; Test 31: await-generator-in-thread! without handler (explicit loop argument)

(let ()
  (define lst '())
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread! await resume #f
					       (lambda (yield)
						 (let loop ((count 0))
						   (when (< count 5)
						     (yield (* 2 count))
						     (loop (1+ count)))))
					       (lambda (val)
						 (set! lst (cons val lst))))))
	      (test-result (car lst) 8)
	      (test-result (length lst) 5)
	      (test-result res #f)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-run!))

;; Test 32: await-generator-in-thread! with handler

(let ()
  (define lst '())
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-generator-in-thread! await resume
					       (lambda (yield)
						 (let loop ((count 0))
						   (cond
						    ((< count 5)
						     (yield (* 2 count))
						     (loop (1+ count)))
						    ((= count 5)
						     (raise-exception 'my-exception)
						     ;; we never reach here
						     (yield (* 2 count))
						     (loop (1+ count)))
						    (else
						     (assert #f))))) ;; we should never reach here
					       (lambda (val)
						 (set! lst (cons val lst)))
					       (lambda (obj)
						 (test-result obj 'my-exception)
						 (set! lst (cons 100 lst))))))
	      (test-result (car lst) 100)
	      (test-result (length lst) 6)
	      (test-result res 'guile-a-sync-thread-error)
	      (print-result)
	      (event-loop-block! #f))))
  (event-loop-run!))

;; Test 33: await-generator-in-event-loop!

(let ()
  (define lst '())
  (define worker (make-event-loop 10 100000))

  (event-loop-block! #t)
  (event-loop-block! #t worker)

  (call-with-new-thread
   (lambda ()
     (event-loop-run! worker)))

  (a-sync (lambda (await resume)
	    (await-generator-in-event-loop! await resume worker
					    (lambda (yield)
					      (let loop ((count 0))
						(when (< count 5)
						  (yield (* 2 count))
						  (loop (1+ count)))))
					    (lambda (val)
					      (set! lst (cons val lst))))
	    (test-result (car lst) 8)
	    (test-result (length lst) 5)
	    (print-result)
	    (event-loop-block! #f)))
  (event-loop-run!))

;; Test 34: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! await resume 10
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run!)
  
;; Test 35: await-sleep!
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-sleep! await resume 100)
	    (test-result 1 val)
	    (print-result)))
  (set! val 1)
  (event-loop-run! main-loop))

;; Test 36: await-getline! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getline! await resume
				       in)))
	      (assert (string=? res "test-string"))
	      (print-result))))
  (write-line "test-string" out)
  (force-output out)
  (event-loop-run!))

;; Test 37: await-geteveryline! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryline! await resume
					    in
					    (lambda (line)
					      (set! count (1+ count))
					      (when (= count 1)
						(assert (string=? line "test-string1")))
					      (when (= count 2)
					      	(assert (string=? line "test-string2"))
						(close out))))))
	      (assert (eof-object? res))
	      (test-result 2 count)
	      (print-result))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (force-output out)
  (event-loop-run!))

;; Test 38: await-getsomelines! (also tests await-read-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomelines! await resume
					    in
					    (lambda (line k)
					      (set! count (1+ count))
					      (when (= count 1)
						    (assert (string=? line "test-string1")))
					      (when (= count 2)
						    (assert (string=? line "test-string2")))
					      (when (= count 3)
						    (assert (string=? line "test-string3"))
						    (k 'exit-await))))))
	      (assert (eq? res 'exit-await))
	      (test-result 3 count)
	      (print-result))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (event-loop-run!)
  (close out))

;; Test 39: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception propagates out of event-loop-run!
(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (await-getsomelines! await resume
				 in
				 (lambda (line k)
				   (set! count (1+ count))
				   (when (= count 1)
					 (assert (string=? line "test-string1")))
				   (when (= count 2)
					 (throw 'exit-exception))
				   (when (= count 3)
					 (assert #f)))) ;; we should never reach here
	    (assert #f))) ;; we should never reach here
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (catch #t
	 (lambda ()
	   (event-loop-run!)
	   (assert #f)) ;; we should never reach here
	 (lambda args
	   (assert (eq? (car args) 'exit-exception))))
  (close out)
  (test-result 2 count)
  (print-result))

;; Test 40: await-getsomelines! exception handling (also tests strategy for await-geteveryline!)
;; exception caught within a-sync block
(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (catch #t
		   (lambda ()
		     (await-getsomelines! await resume
					  in
					  (lambda (line k)
					    (set! count (1+ count))
					    (when (= count 1)
						  (assert (string=? line "test-string1")))
					    (when (= count 2)
						  (throw 'exit-exception))
					    (when (= count 3)
						  (assert #f))))   ;; we should never reach here
		     (assert #f))   ;; we should never reach here
		   (lambda args
		     (assert (eq? (car args) 'exit-exception))))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (event-loop-run!)
  (close out)
  (test-result 2 count)
  (print-result))

;; Test 41: await-getblock! (also tests a-sync-read-watch!)
(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getblock! await resume
					in
					10)))
	      (test-result 10 (cdr res))
	      (assert (test-bytevector bv1 (car res))))
	    (let ((res (await-getblock! await resume
					in
					20)))
	      (test-result 10 (cdr res))
	      (test-result 20 (bytevector-length (car res)))
	      (assert (test-bytevector bv2 (car res))))
	    (let ((res (await-getblock! await resume
					in
					10)))
	      (assert (eof-object? (car res)))
	      (test-result #f (cdr res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
            (close-port out)))
  (event-loop-run!)
  (close-port in))

;; Test 42: await-geteveryblock! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-geteveryblock! await resume
					     in
					     20
					     (lambda (bv-in len)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 20 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv3 bv-in)))))))
	      (assert (eof-object? res)))
	    (let ((res (await-geteveryblock! await resume
					     in
					     10
					     (lambda (bv-in len)
					       (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (event-loop-run!)
  (test-result 2 count)
  (close-port in))

;; Test 43: await-getsomeblocks! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (define bv3 (make-bytevector 10 22))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-getsomeblocks! await resume
					     in
					     10
					     (lambda (bv-in len k)
					       (set! count (1+ count))
					       (when (= count 1)
						     (test-result 10 len)
						     (assert (test-bytevector bv1 bv-in)))
					       (when (= count 2)
						     (test-result 10 len)
						     (assert (test-bytevector bv2 bv-in))
						     (k 'test))))))
	      (test-result 'test res))
	    (let ((res (await-getsomeblocks! await resume
	    				     in
					     20
	    				     (lambda (bv-in len k)
					       (test-result 10 len)
					       (assert (test-bytevector bv3 bv-in))))))
	      (assert (eof-object? res)))
	    (let ((res (await-getsomeblocks! await resume
	    				     in
					     10
	    				     (lambda (bv-in len k)
	    				       (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)))
  (a-sync (lambda (await resume)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-timeout! await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (event-loop-run!)
  (test-result 2 count)
  (close-port in))

;; Test 44: await-put-bytevector! (also tests await-write-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (define bv (make-bytevector 10 55))
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume
						       (lambda ()
							 (get-bytevector-all in))))
		      (event-loop-block! #f)))
	    (await-put-bytevector! await resume out bv)
	    (close-port out)))
  (event-loop-run!)
  (test-result (bytevector-length res) 10)
  (test-bytevector res bv)
  (close-port in)
  (print-result))

;; Test 45: await-put-string! (also tests await-write-suspendable!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (event-loop-block! #t)
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-task-in-thread! await resume
						       (lambda ()
							 (read-string in))))
		      (event-loop-block! #f)))
	    (await-put-string! await resume out (string #\a #\b #\c))
	    (close-port out)))
  (event-loop-run!)
  (test-result (string-length res) 3)
  (test-result (string-ref res 0) #\a)
  (test-result (string-ref res 1) #\b)
  (test-result (string-ref res 2) #\c)
  (close-port in)
  (print-result))

;; Test 46: compose-a-sync and no-await

(event-loop-block! #t)
(compose-a-sync ((res (await-task-in-thread! (lambda ()
					       (+ 5 10)))))
	      ((no-await (test-result 15 res)
			 (print-result)
			 ;; we can apply event-loop-quit! here as we know the loop is running
			 (event-loop-quit!))))
(event-loop-run!)
