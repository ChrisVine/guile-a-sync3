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


;; When using the g-golf bindings for gtk+, in order to provide await
;; semantics on gtk+ callbacks it will normally be necessary to use
;; the 'await' and 'resume' procedures provided by the a-sync
;; procedure in the (a-sync coroutines) module directly (calling
;; 'resume' in the gtk+ callback when ready, and waiting on that
;; callback using 'await').  However when launching timeouts, file
;; watches or other events on the glib main loop using g-golf,
;; convenience procedures are possible similar to those provided for
;; the event loop in the (a-sync event-loop) module.  These are set
;; out in the files in this directory.
;;
;; Most of the scheme files provided by this library are by default
;; compiled by this library to bytecode.  That is not the case with
;; this file, so as not to create a hard dependency on g-golf.
