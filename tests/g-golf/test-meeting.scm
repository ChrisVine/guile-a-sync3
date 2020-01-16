;; Copyright (C) 2017 and 2020 Chris Vine

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
	     (a-sync g-golf base)
	     (a-sync g-golf meeting)
	     (rnrs base)   ;; for assert
	     (g-golf glib main-event-loop))

;; helpers

(define-syntax-rule (test-result expected res)
  (assert (eqv? expected res)))

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       (simple-format #t "~A: Test ~A OK\n" "test-meeting.scm" count)
       (set! count (1+ count))))))

;; Test 1: start receiving before sending, iteratively on single meeting object

(define main-loop (g-main-loop-new #f #f))

(let ()
  (define m1 (make-glib-meeting))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ((datum (glib-meeting-receive await resume m1)))
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (glib-meeting-receive await resume m1))))
	    (a-sync-glib-quit main-loop)))

  (a-sync (lambda (await resume)
	    (let next ((count 0))
	      (if (< count 4)
		  (begin
		    (glib-meeting-send await resume m1 count)
		    (next (1+ count)))
		  (glib-meeting-close m1)))))

  (g-main-loop-run main-loop)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

;; Test 2: start sending before receiving, iteratively on single meeting object

(let ()
  (define m1 (make-glib-meeting))
  (define res '())

  (a-sync (lambda (await resume)
	    (let next ((count 0))
	      (if (< count 4)
		  (begin
		    (glib-meeting-send await resume m1 count)
		    (next (1+ count)))
		  (glib-meeting-close m1)))))

  (a-sync (lambda (await resume)
	    (let next ((datum (glib-meeting-receive await resume m1)))
	      (when (not (eq? datum 'stop-iteration))
		(set! res (cons datum res))
		(next (glib-meeting-receive await resume m1))))
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (assert (equal? res '(3 2 1 0)))
  (print-result))

;;;;;;;;;;;;;;;;;; additional tests ;;;;;;;;;;;;;;;;;;

;; Test 3: meeting-ready?

(let ()
  (define m1 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (assert (not (glib-meeting-ready? m1)))
	    (glib-meeting-send await resume m1 0)))

  (a-sync (lambda (await resume)
	    (assert (glib-meeting-ready? m1))
	    (glib-meeting-receive await resume m1)
	    (assert (not (glib-meeting-ready? m1)))
	    (glib-meeting-close m1)
	    (assert (glib-meeting-ready? m1))
	    (print-result)
	    (a-sync-glib-quit main-loop)))
  (g-main-loop-run main-loop))

;; Test 4: multiple readers on single meeting object (fan out)

(let ()
  (define m1 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 0)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 1)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 2)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 'stop-iteration)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 'stop-iteration)))

  (a-sync (lambda (await resume)
	    (let next ((count 0))
	      (if (< count 3)
		  (begin
		    (glib-meeting-send await resume m1 count)
		    (next (1+ count)))
		  (glib-meeting-close m1)))))

  (a-sync (lambda (await resume)
  	    (test-result (glib-meeting-receive await resume m1) 'stop-iteration)
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 5: multiple senders on single meeting object (fan in)

(let ()
  (define m1 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 0) #f)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 1) #f)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 2) #f)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 3) 'stop-iteration)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 4) 'stop-iteration)))

  (a-sync (lambda (await resume)
	    (let next ((count 0))
	      (if (< count 3)
		  (let ((datum (glib-meeting-receive await resume m1)))
		    (test-result datum count)
		    (next (1+ count)))
		  (glib-meeting-close m1)))))
  
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 5) 'stop-iteration)
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 6: waiting to receive from multiple meeting objects (1)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))
  (define m3 (make-glib-meeting))
  (define m4 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (let next ((count 1))
	      (when (< count 4)
		(test-result (glib-meeting-receive await resume m1 m2 m3 m4) count)
		(next (1+ count))))
	    (a-sync-glib-quit main-loop)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 1) #f)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m2 2) #f)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m3 3) #f)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 7: waiting to receive from multiple meeting objects (2)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))
  (define m3 (make-glib-meeting))
  (define m4 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (let next ((count 1))
	      (when (< count 4)
		(test-result (glib-meeting-receive await resume m1 m2 m3 m4) count)
		(next (1+ count))))
	    (a-sync-glib-quit main-loop)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 1) #f)
	    (test-result (glib-meeting-send await resume m2 2) #f)
	    (test-result (glib-meeting-send await resume m3 3) #f)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 8: waiting to receive from multiple meeting objects (3)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))
  (define m3 (make-glib-meeting))
  (define m4 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 1) #f)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m2 2) #f)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m3 3) #f)))

  (a-sync (lambda (await resume)
	    (let next ((count 1))
	      (when (< count 4)
		(test-result (glib-meeting-receive await resume m1 m2 m3 m4) count)
		(next (1+ count))))
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 9: waiting to receive from multiple meeting objects (4)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))
  (define m3 (make-glib-meeting))
  (define m4 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 1) #f)
	    (test-result (glib-meeting-send await resume m2 2) #f)
	    (test-result (glib-meeting-send await resume m3 3) #f)))

  (a-sync (lambda (await resume)
	    (let next ((count 1))
	      (when (< count 4)
		(test-result (glib-meeting-receive await resume m1 m2 m3 m4) count)
		(next (1+ count))))
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 10: waiting to send to multiple meeting objects (1)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 m2 1) #f)
	    (test-result (glib-meeting-send await resume m1 m2 2) #f)
	    (test-result (glib-meeting-send await resume m1 m2 3) #f)
	    (test-result (glib-meeting-send await resume m1 m2 4) #f)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 1)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 2)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 3)
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 11: waiting to send to multiple meeting objects (2)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 m2 1) #f)
	    (test-result (glib-meeting-send await resume m1 m2 2) #f)
	    (test-result (glib-meeting-send await resume m1 m2 3) #f)
	    (test-result (glib-meeting-send await resume m1 m2 4) #f)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 1)
  	    (test-result (glib-meeting-receive await resume m1) 2)
  	    (test-result (glib-meeting-receive await resume m2) 3)
  	    (test-result (glib-meeting-receive await resume m1) 4)
	    (a-sync-glib-quit main-loop)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 12: waiting to send to multiple meeting objects (3)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 2)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m1) 1)))
  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 3)
	    (a-sync-glib-quit main-loop)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 m2 1) #f)
	    (test-result (glib-meeting-send await resume m1 m2 2) #f)
	    (test-result (glib-meeting-send await resume m1 m2 3) #f)
	    (test-result (glib-meeting-send await resume m1 m2 4) #f)))

  (g-main-loop-run main-loop)
  (print-result))

;; Test 13: waiting to send to multiple meeting objects (4)

(let ()
  (define m1 (make-glib-meeting))
  (define m2 (make-glib-meeting))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-receive await resume m2) 1)
  	    (test-result (glib-meeting-receive await resume m1) 2)
  	    (test-result (glib-meeting-receive await resume m2) 3)
  	    (test-result (glib-meeting-receive await resume m1) 4)
	    (a-sync-glib-quit main-loop)))

  (a-sync (lambda (await resume)
	    (test-result (glib-meeting-send await resume m1 m2 1) #f)
	    (test-result (glib-meeting-send await resume m1 m2 2) #f)
	    (test-result (glib-meeting-send await resume m1 m2 3) #f)
	    (test-result (glib-meeting-send await resume m1 m2 4) #f)))

  (g-main-loop-run main-loop)
  (print-result))
