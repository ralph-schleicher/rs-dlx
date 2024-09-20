;;; packages.lisp --- package definitions

;; Copyright (C) 2024 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.dlx
  (:nicknames :rs-dlx)
  (:use :common-lisp
	:iterate)
  (:shadow #:search)
  (:export #:make-matrix
           #:matrix-rows
           #:matrix-columns
           #:matrix-element
           #:matrix-elements
           #:matrix-from-array
           #:array-from-matrix
           #:add-matrix-row
           #:add-matrix-column
           #:remove-matrix-row
           #:remove-matrix-column
           #:remove-matrix-row-by-name
           #:remove-matrix-column-by-name
           #:remove-matrix-rows
           #:remove-matrix-columns
           #:remove-matrix-rows-by-name
           #:remove-matrix-columns-by-name
           #:map-matrix-row
           #:map-matrix-column
           #:row-index
           #:column-index
           #:row-name
           #:column-name
           #:row-names
           #:column-names
           ;; dlx.lisp
           #:solve)
  (:documentation
   "Knuth's Algorithm X using the dancing links technique.

The algorithm itself is executed by the ‘solve’ function.  The
remaining functions manage the incidence matrix, i.e. the sparse
matrix data structure based on circular doubly linked lists as
described in Knuth's “Dancing Links” paper.

Donald E. Knuth: “Dancing Links”, ‹https://arxiv.org/abs/cs/0011047›."))

;;; packages.lisp ends here
