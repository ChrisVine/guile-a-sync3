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

(define-module (a-sync let-a-sync)
  #:use-module (a-sync coroutines) ;; for a-sync
  #:export (let-a-sync*
	    no-await))

;; This file provides the let-a-sync* macro, which enables
;; asynchronous tasks to be composed on an event loop using let* type
;; syntax.
;;
;; The first argument to let-a-sync* is the main loop on which the
;; tasks are to be composed.  This is followed by bindings which are
;; optional (there need not be any), each of which must be initialised
;; by a 'let-a-sync*'-capable procedure, and following the bindings
;; there must be a body of 'let-a-sync*'-capable procedures executed
;; solely for the purpose of asynchronous side effects (this macro
;; does not, and cannot, return a value because as soon as the first
;; await is made control is passed to the event loop).  As in the case
;; of let*, unlike the bindings the body cannot be empty - there must
;; be at least one 'let-a-sync*'-capable procedure in the body.
;;
;; As in the case of let*, each 'let-a-sync*'-capable procedure
;; initializing a binding can see the values of the initializations
;; made prior to it.
;;
;; A 'let-a-sync*'-capable procedure is one which takes an 'await' and
;; 'yield' procedure from a-sync as its first and second arguments,
;; and an event loop as it third argument, followed by such further
;; arguments as it requires.  All the the await-task!,
;; await-task-in-thread!, await-timeout! and await-getline! procedures
;; provided by the (a-sync event-loop) module are
;; 'let-a-sync*'-capable.  In addition, to make an ordinary body of
;; code which does not block (and which does not need to invoke
;; a-sync's await procedure) usable by let-a-sync*, the no-await macro
;; can be used to generate a 'let-a-sync*'-capable procedure for it
;; (see below).
;;
;; Each binding is initialized as if sequentially (although it is done
;; asynchronously on the relevant event loop).  An initialization does
;; not begin until an earlier one has completed.  In addition, each
;; clause in the body is executed sequentially in turn, but does so
;; asynchronously on the event loop using 'await' semantics.
;;
;; When calling a 'let-a-sync*'-capable procedure within a
;; 'let-a-sync*' block (including when initializing its bindings), the
;; 'await' and 'yield' and event-loop arguments are not explicitly
;; passed to it.  The let-a-sync* macro will do it for you.  See the
;; example.scm file with the distribution for further particulars.
(define-syntax let-a-sync*
  (syntax-rules ()
    ((_ loop ((val (await-proc0 arg0 ...)) ...)
	(await-proc1 arg1 ...)
	(await-proc2 arg2 ...) ...)
     (a-sync (lambda (await resume)
	       (let* ((val (await-proc0 await resume loop arg0 ...)) ...)
		 (await-proc1 await resume loop arg1 ...)
		 (await-proc2 await resume loop arg2 ...) ...))))))

;; This macro will generate a 'let-a-sync*'-capable procedure from a
;; body of code which does not block.  See the example.scm file with
;; the distribution for further particulars.
(define-syntax no-await
  (syntax-rules ()
    ((_ body0 body1 ...)
     (lambda (ignore0 ignore1 ignore2)
	body0 body1 ...))))
