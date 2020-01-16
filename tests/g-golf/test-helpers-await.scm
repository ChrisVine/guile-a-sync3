;; Copyright (C) 2016 and 2020 Chris Vine

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
	     (a-sync compose)
	     (a-sync thread-pool)
	     (a-sync g-golf base)
	     (a-sync g-golf await-ports)
	     (ice-9 threads)      ;; for call-with-new-thread
	     (ice-9 rdelim)       ;; for write-line and read-string
	     (ice-9 control)      ;; for call/ec
	     (ice-9 binary-ports) ;; for get-bytevector-all and put-bytevector
	     (rnrs base)          ;; for assert
	     (rnrs bytevectors)   ;; for bytevectors
             (g-golf glib main-event-loop))

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

;; Test 1: await-glib-task

(define main-loop (g-main-loop-new #f #f))

(a-sync (lambda (await resume)
	  (let ((res
		 (await-glib-task await resume
				  (lambda ()
				    (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (a-sync-glib-quit main-loop))))
(g-main-loop-run main-loop)

;; Test 2: await-glib-task-in-thread without handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-glib-task-in-thread await resume
					    (lambda ()
					      (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (a-sync-glib-quit main-loop))))
(g-main-loop-run main-loop)
  
;; Test 3: await-glib-task-in-thread with handler

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-glib-task-in-thread
		  await resume
		  (lambda () (raise-exception 'test-exception))
		  (lambda (obj)
		    (test-result 'test-exception obj)
		    5))))
	    (test-result 5 res)
	    (print-result)
	    (a-sync-glib-quit main-loop))))
(g-main-loop-run main-loop)

;; Test 4: await-glib-yield
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-glib-yield await resume)
	    (test-result 1 val)
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (set! val 1)
  (g-main-loop-run main-loop))

;; Test 5: await-glib-generator

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (await-glib-generator await resume
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
	    (a-sync-glib-quit main-loop)))
  (g-main-loop-run main-loop))

;; Test 6: await-glib-generator-in-thread without handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-generator-in-thread await resume
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
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop))

;; Test 7: await-glib-generator-in-thread with handler

(let ()
  (define lst '())
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-generator-in-thread await resume
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
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop))

;; Test 8: await-glib-timeout

(a-sync (lambda (await resume)
	  (let ((res
		 (await-glib-timeout await resume 10
				     (lambda ()
				       (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (a-sync-glib-quit main-loop))))
(g-main-loop-run main-loop)
  
;; Test 9: await-glib-sleep
(let ()
  (define val 0)
  (a-sync (lambda (await resume)
	    (await-glib-sleep await resume 100)
	    (test-result 1 val)
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (set! val 1)
  (g-main-loop-run main-loop))

;; Test 10: await-glib-getline (also tests await-glib-read-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-glib-getline await resume
					   in)))
	      (assert (string=? res "test-string"))
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (write-line "test-string" out)
  (force-output out)
  (g-main-loop-run main-loop))

;; Test 11: await-glib-geteveryline (also tests await-glib-read-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-glib-geteveryline await resume
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
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (force-output out)
  (g-main-loop-run main-loop))

;; Test 12: await-glib-getsomelines (also tests await-glib-read-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-glib-getsomelines await resume
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
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (g-main-loop-run main-loop)
  (close out))

;; Test 13: await-glib-getsomelines exception handling (also tests strategy for await-glib-geteveryline)
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
		     (await-glib-getsomelines await resume
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
		     (a-sync-glib-quit main-loop)
		     (assert (eq? (car args) 'exit-exception))))))
  (write-line "test-string1" out)
  (write-line "test-string2" out)
  (write-line "test-string3" out)
  (force-output out)
  (g-main-loop-run main-loop)
  (close out)
  (test-result 2 count)
  (print-result))

;; Test 14: await-glib-getblock (also tests await-glib-read-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define bv1 (make-bytevector 10 20))
  (define bv2 (make-bytevector 10 21))
  (fcntl in F_SETFL (logior O_NONBLOCK
                            (fcntl in F_GETFL)))
  (a-sync (lambda (await resume)
	    (let ((res (await-glib-getblock await resume
					    in
					    10)))
	      (test-result 10 (cdr res))
	      (assert (test-bytevector bv1 (car res))))
	    (let ((res (await-glib-getblock await resume
					    in
					    20)))
	      (test-result 10 (cdr res))
	      (test-result 20 (bytevector-length (car res)))
	      (assert (test-bytevector bv2 (car res))))
	    (let ((res (await-glib-getblock await resume
					    in
					    10)))
	      (assert (eof-object? (car res)))
	      (test-result #f (cdr res)))
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (a-sync (lambda (await resume)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
            (close-port out)))
  (g-main-loop-run main-loop)
  (close-port in))

;; Test 15: await-glib-geteveryblock (also tests await-glib-read-suspendable)

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
	    (let ((res (await-glib-geteveryblock await resume
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
	    (let ((res (await-glib-geteveryblock await resume
						 in
						 10
						 (lambda (bv-in len)
						   (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (a-sync (lambda (await resume)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (g-main-loop-run main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 16: await-glib-getsomeblocks (also tests await-glib-read-suspendable)

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
	    (let ((res (await-glib-getsomeblocks await resume
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
	    (let ((res (await-glib-getsomeblocks await resume
						 in
						 20
						 (lambda (bv-in len k)
						   (test-result 10 len)
						   (assert (test-bytevector bv3 bv-in))))))
	      (assert (eof-object? res)))
	    (let ((res (await-glib-getsomeblocks await resume
						 in
						 10
						 (lambda (bv-in len k)
						   (assert #f))))) ;; we should never reach here
	      (assert (eof-object? res)))
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (a-sync (lambda (await resume)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv1)
	    (force-output out)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv2)
	    (force-output out)
	    (await-glib-timeout await resume 50 (lambda () #f))
	    (put-bytevector out bv3)
	    (force-output out)
            (close-port out)))
  (g-main-loop-run main-loop)
  (test-result 2 count)
  (close-port in))

;; Test 17: await-glib-put-bytevector (also tests await-glib-write-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (define bv (make-bytevector 10 55))
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-glib-task-in-thread await resume
							   (lambda ()
							     (get-bytevector-all in))))
		      (a-sync-glib-quit main-loop)))
	    (await-glib-put-bytevector await resume out bv)
	    (close-port out)))
  (g-main-loop-run main-loop)
  (test-result (bytevector-length res) 10)
  (test-bytevector res bv)
  (close-port in)
  (print-result))

;; Test 18: await-put-string! (also tests await-glib-write-suspendable)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define res #f)
  (fcntl out F_SETFL (logior O_NONBLOCK
			     (fcntl out F_GETFL)))
  (a-sync (lambda (await resume)
	    (a-sync (lambda (await resume)
		      (set! res (await-glib-task-in-thread await resume
							   (lambda ()
							     (read-string in))))
		      (a-sync-glib-quit main-loop)))
	    (await-glib-put-string await resume out (string #\a #\b #\c))
	    (close-port out)))
  (g-main-loop-run main-loop)
  (test-result (string-length res) 3)
  (test-result (string-ref res 0) #\a)
  (test-result (string-ref res 1) #\b)
  (test-result (string-ref res 2) #\c)
  (close-port in)
  (print-result))

;; Test 19: await-glib-task-in-thread-pool without handler

(let ((pool (make-thread-pool)))
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-task-in-thread-pool await resume pool
						   (lambda ()
						     (+ 5 10)))))
	      (test-result 15 res)
	      (test-result 1 (thread-pool-get-num-threads pool))
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop)
  (thread-pool-stop! pool))
		    
;; Test 20: await-glib-task-in-thread-pool with handler

(let ((pool (make-thread-pool)))
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-task-in-thread-pool
		    await resume pool
		    (lambda () (raise-exception 'test-exception))
		    (lambda (obj)
		      (test-result 'test-exception obj)
		      5))))
	      (test-result 5 res)
	      (test-result 1 (thread-pool-get-num-threads pool))
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop)
  (thread-pool-stop! pool))

;; Test 21: await-glib-generator-in-thread-pool without handler

(let ((pool (make-thread-pool))
      (lst '()))
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-generator-in-thread-pool await resume pool
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
	      (test-result 1 (thread-pool-get-num-threads pool))
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop)
  (thread-pool-stop! pool))

;; Test 22: await-glib-generator-in-thread-pool with handler

(let ((pool (make-thread-pool))
      (lst '()))
  (a-sync (lambda (await resume)
	    (let ((res
		   (await-glib-generator-in-thread-pool await resume pool
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
	      (test-result 1 (thread-pool-get-num-threads pool))
	      (print-result)
	      (a-sync-glib-quit main-loop))))
  (g-main-loop-run main-loop)
  (thread-pool-stop! pool))
