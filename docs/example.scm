#!/usr/bin/env guile
!#

;; Copyright Chris Vine 2014 and 2016
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


(use-modules (a-sync event-loop) (a-sync coroutines) (ice-9 threads))

(define main-loop (make-event-loop))
(a-sync (lambda (await resume)
	  (display "In waitable procedure\n")
	  ;; launch asynchronous task: let's pretend its time
	  ;; consuming so we need to run it in a worker thread
	  ;; to avoid blocking any other events in the main loop
	  ;; (there aren't any in this example)
	  (a-sync-run-task-in-thread! main-loop
				      resume
				      (lambda ()
					;; do some work
					(usleep 500000)
					(display "In first async callback, work done\n")
					;; this is the result of our extensive computation
					"Hello via async\n"))
	  (display "About to make first wait\n")
	  (display (string-append "Back in waitable procedure, and the callback says: " (await)))
	  
	  ;; launch another asynchronous task, this time in the event loop thread
	  (a-sync-run-task! main-loop
			    resume
			    (lambda ()
			      (display "In second async callback\n")))
	  (display "About to make second wait\n")
	  (await)

	  (display "\nStarting watch on /dev/tty; press 'x' to finish\n")
	  (system* "stty" "--file=/dev/tty" "cbreak" "-echo")
	  (let ([keyboard (open "/dev/tty" O_RDONLY)])
	    ;; we don't need to run a-sync again here to obtain
	    ;; another await-resume pair, because as coded above the
	    ;; timeout does not run until the preceding awaits have
	    ;; returned, but it is OK to do so (and we would need to
	    ;; do so if we were to interleave the events)
	    (a-sync (lambda (await resume)
		      (a-sync-read-watch! main-loop
					  keyboard
					  resume
					  (lambda (status)
					    (read-char keyboard)))
		      (let loop ([ch (await)])
			(if (not (char=? ch #\x))
			    (begin
			      (simple-format #t "~A~A" ch (if (char=? ch #\lf) "" " "))
			      (loop (await)))
			    (begin
			      (newline)
			      (event-loop-block! main-loop #f)
			      (event-loop-remove-watch! main-loop keyboard)))))))))

;; because the first task runs in another thread
(event-loop-block! main-loop #t)
(event-loop-run! main-loop)

(system* "stty" "--file=/dev/tty" "-cbreak" "echo")
