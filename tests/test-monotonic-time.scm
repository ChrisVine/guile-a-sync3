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

(use-modules (a-sync monotonic-time)
	     (rnrs base))   ;; for assert

;; helpers

(define-syntax-rule (test-result expected res)
  (assert (eqv? expected res)))

(define print-result
  ((lambda ()
     (define count 1)
     (lambda ()
       ;; the current-filename procedure is broken in guile-2.1.4 when
       ;; looking up a file via the GUILE_LOAD_PATH environmental
       ;; variable - this is a work-around until it is fixed
       ;;(simple-format #t "~A: Test ~A OK\n" (basename (current-filename)) count)
       (simple-format #t "~A: Test ~A OK\n" "test-monotonic-time.scm" count)
       (set! count (1+ count))))))

;; Test 1: have-monotonic-time?

;; all we have to do here is see that 'have-monotonic-time?' returns,
;; as we are not interested in what it returns.  'get-time' is tested
;; via test-event-loop.scm.
(have-monotonic-time?)
(print-result)
