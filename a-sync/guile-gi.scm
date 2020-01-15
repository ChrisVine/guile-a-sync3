;; Copyright (C) 2020 Chris Vine

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


;; Most of the scheme files provided by this library are by default
;; compiled by this library to bytecode.  That is not the case with
;; this file, so as not to create a hard dependency on guile-gi.

(define-module (a-sync guile-gi)
  #:use-module (a-sync guile-gi base)
  #:use-module (a-sync guile-gi await-ports)
  #:use-module (a-sync guile-gi meeting)
  #:re-export (await-glib-task-in-thread
	       await-glib-task
	       await-glib-yield
	       await-glib-generator-in-thread
	       await-glib-generator
	       await-glib-timeout
	       await-glib-sleep
	       await-glib-task-in-thread-pool
	       await-glib-generator-in-thread-pool
	       await-glib-read-suspendable
	       await-glib-getline
	       await-glib-geteveryline
	       await-glib-getsomelines
	       await-glib-getblock
	       await-glib-geteveryblock
	       await-glib-getsomeblocks
	       await-glib-write-suspendable
	       await-glib-put-bytevector
	       await-glib-put-string
	       await-glib-accept
	       await-glib-connect
	       make-glib-meeting
	       glib-meeting?
	       glib-meeting-close
	       glib-meeting-ready?
	       glib-meeting-send
	       glib-meeting-receive)
  #:duplicates (warn last))
