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
