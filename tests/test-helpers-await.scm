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

(use-modules (a-sync coroutines)
	     (a-sync event-loop)
	     (ice-9 rdelim) ;; for write-line
	     (rnrs base))   ;; for assert

;; helpers

(define-syntax-rule (test-result expected res)
  (assert (eqv? expected res)))

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       (simple-format #t "~A: Test ~A OK\n" (basename (current-filename)) count)
       (set! count (1+ count))))))

;; Test 1: await-task!

(define main-loop (make-event-loop))

(a-sync (lambda (await resume)
	  (let ((res
		 (await-task! main-loop await resume
			      (lambda ()
				(+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)
  
;; Test 2: await-task-in-thread!

(a-sync (lambda (await resume)
	  (let ((res
	  	 (await-task-in-thread! main-loop await resume
	  				(lambda ()
	  				  (+ 5 10)))))
	    (test-result 15 res)
	    (print-result)
	    (event-loop-quit! main-loop))))
(event-loop-block! main-loop #t)
(event-loop-run! main-loop)
(event-loop-block! main-loop #f)
  
;; Test 3: await-timeout!

(a-sync (lambda (await resume)
	  (let ((res
		 (await-timeout! main-loop 10 await resume
				 (lambda ()
				   (+ 5 10)))))
	    (test-result 15 res)
	    (print-result))))
(event-loop-run! main-loop)
  
;; Test 4: await-getline! (also tests a-sync-read-watch!)

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (a-sync (lambda (await resume)
	    (let ((res (await-getline! main-loop
				       in
				       await resume)))
	      (assert (string=? res "test-string"))
	      (print-result))))
  (write-line "test-string" out)
  (force-output out)
  (event-loop-run! main-loop))

;; Test 5: a-sync-write-watch!

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (a-sync (lambda (await resume)
	    (a-sync-write-watch! main-loop out resume
				 (lambda (status)
				   (test-result 'out status)
				   (if (< count 3)
				       (begin
					 (set! count (1+ count))
					 (write-char #\a out)
					 (force-output out)
					 'more)
				       (begin
					 (write-char #\x out)
					 (force-output out)
					 (event-loop-remove-watch! main-loop out)
					 'done))))
	    (let loop ((res (await)))
	      (let ((ch (read-char in)))
		(if (not (char=? ch #\x))
		    (begin
		      (test-result 'more res)
		      (test-result #\a ch)
		      (loop (await)))
		    (test-result 'done res))))
	    (test-result 3 count)
	    (print-result)))
  (event-loop-run! main-loop))
