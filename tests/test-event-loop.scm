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

(use-modules (a-sync event-loop)
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

;; Test 1: event-post!

(define main-loop (make-event-loop))
(let ()
  (define count 0)
  (event-post! main-loop
	       (lambda ()
		 (set! count (1+ count))))
  (event-loop-run! main-loop)
  (test-result 1 count)
  (print-result))
  
;; Test 2: timeout-post! and timeout-remove!

(let ()
  (define count1 0)
  (define count2 0)
  (define tag (timeout-post! main-loop 60
			     (lambda ()
			       (if (< count1 3)
				   (set! count1 (1+ count1))
				   (timeout-remove! main-loop tag))
			       #t)))
  (timeout-post! main-loop 100
		 (lambda ()
		   (if (< count2 3)
		       (begin
			 (set! count2 (1+ count2))
			 #t)
		       #f)))
  (event-loop-run! main-loop)
  (test-result 3 count1)
  (test-result 3 count2)
  (print-result))

;; Test 3: event-loop-block! and event-loop-quit!

(let ()
  (define count 0)
  (call-with-new-thread (lambda ()
			  ;; we don't need mutex here as the main
			  ;; thread only access count before the
			  ;; thread starts and after it ends
			  (set! count (1+ count))
			  (event-post! main-loop
				       (lambda ()
					 (event-loop-quit! main-loop)))))
  (event-loop-block! main-loop #t)
  (event-loop-run! main-loop)
  (test-result 1 count)
  (event-loop-block! main-loop #f)
  (set! count (1+ count))
  ;; this should return immediately with the loop set not to block
  ;; again
  (event-loop-run! main-loop)
  (test-result 2 count)
  (print-result))

;; Test 4: event-loop-add-read-watch!

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (event-loop-add-read-watch! main-loop in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count))
					#t)
				      #f))))
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (force-output out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (force-output out)))))
  (event-loop-run! main-loop)
  (test-result 3 count)
  (print-result))

;; Test 5: event-loop-add-read-watch! and event-loop-remove-watch!

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (event-loop-add-read-watch! main-loop in
			      (lambda (status)
				(test-result 'in status)
				(let ([ch (read-char in)])
				  (if (not (char=? ch #\x))
				      (begin
					(test-result #\a ch)
					(set! count (1+ count)))
				      (event-loop-remove-watch! main-loop in))
				  #t)))
  (let loop ((count 0))
    (if (< count 3)
	(begin
	  (write-char #\a out)
	  (force-output out)
	  (loop (1+ count))
	(begin
	  (write-char #\x out)
	  (force-output out)))))
  (event-loop-run! main-loop)
  (test-result 3 count)
  (print-result))

;; Test 6: event-loop-add-write-watch!

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (event-loop-add-write-watch! main-loop out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (force-output out)
				       #t)
				     (begin
				       (write-char #\x out)
				       (force-output out)
				       #f))))
  (event-loop-run! main-loop)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (print-result))

;; Test 7: event-loop-add-write-watch! and event-loop-remove-watch!

(let ()
  (define test-pipe (pipe))
  (define in (car test-pipe))
  (define out (cdr test-pipe))
  (define count 0)
  (event-loop-add-write-watch! main-loop out
			       (lambda (status)
				 (test-result 'out status)
				 (if (< count 3)
				     (begin
				       (set! count (1+ count))
				       (write-char #\a out)
				       (force-output out))
				     (begin
				       (write-char #\x out)
				       (force-output out)
				       (event-loop-remove-watch! main-loop out)))
				 #t))
  (event-loop-run! main-loop)
  (let loop ((ch (read-char in))
	     (count 0))
    (if (not (char=? ch #\x))
	(begin
	  (test-result #\a ch)
	  (loop (read-char in) (1+ count)))
	(test-result 3 count)))
  (test-result 3 count)
  (print-result))
