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
	     (rnrs base))   ;; for assert

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
       (simple-format #t "~A: Test ~A OK\n" "test-coroutines.scm" count)
       (set! count (1+ count))))))

;; Test 1: make-iterator

(define iter (make-iterator (lambda (yield)
			      (let loop ((val 0))
				(when (< val 3)
				  (loop (yield val)))))))
(test-result 0 (iter))
(test-result 1 (iter 1))
(test-result 2 (iter 2))
(test-result 'stop-iteration (iter 3))
(test-result 'stop-iteration (iter 0))
(print-result)

;; Test 2: make-coroutine

(define cor (make-coroutine (lambda (yield)
			      (let loop ((val 0))
				(if (< val 3)
				    (loop (yield val))
				    'end)))))

(let-values ([(resume arg) (cor)])
  (set! cor resume)
  (test-result 0 arg))
(let-values ([(resume arg) (cor 1)])
  (set! cor resume)
  (test-result 1 arg))
(let-values ([(resume arg) (cor 2)])
  (set! cor resume)
  (test-result 2 arg))
(let-values ([(resume arg) (cor 3)])
  (test-result #f resume)
  (test-result 'end arg))
(print-result)

;; Test 3: a-sync

(define main-loop (make-event-loop))
(a-sync (lambda (await resume)
	  (event-post! (lambda ()
			 (resume 5))
		       main-loop)
	  (test-result 5 (await))
	  (print-result)))
(event-loop-run! main-loop)
