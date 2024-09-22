;;; tests.lisp --- Knuth's Algorithm X with dancing links test suite

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

(defpackage #:de.ralph-schleicher.dlx-tests
  (:nicknames :rs-dlx-tests)
  (:use :common-lisp
	:iterate
	:lisp-unit
	:rs-dlx)
  (:export
   #:main))

(in-package :rs-dlx-tests)

(defparameter *prefix* (asdf:system-source-directory "rs-dlx"))

(defvar a5x6 #2A((11 12 13 14 15 16)
                 (21 22 23 24 25 26)
                 (31 32 33 34 35 36)
                 (41 42 43 44 45 46)
                 (51 52 53 54 55 56)))

(define-test empty-matrix
  (let (a)
    (setf a (make-matrix 0 0))
    (assert-true (= 0 (matrix-rows a)) a)
    (assert-true (= 0 (matrix-columns a)) a)
    (setf a (make-matrix 4 0))
    (assert-true (= 4 (matrix-rows a)) a)
    (assert-true (= 0 (matrix-columns a)) a)
    (setf a (make-matrix 0 3))
    (assert-true (= 0 (matrix-rows a)) a)
    (assert-true (= 3 (matrix-columns a)) a)
    ()))

(define-test null-matrix
  (let (a (m 4) (n 3))
    (setf a (make-matrix m n))
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (= 0 (matrix-element a i j)) a)))
    (setf a (make-matrix m n :element-type 'single-float))
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (= 0 (matrix-element a i j)) a)))
    (setf a (make-matrix m n :element-type t))
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (null (matrix-element a i j)) a)))
    ()))

(define-test full-matrix
  (let* ((a a5x6)
         (m (array-dimension a 0))
         (n (array-dimension a 1))
         (b nil))
    (setf b (make-matrix m n :element-type t))
    (setf (matrix-elements b) a)
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (= (aref a i j) (matrix-element b i j)) a b)))
    (setf b (matrix-from-array a :element-type t))
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (= (aref a i j) (matrix-element b i j)) a b)))
    (setf a (array-from-matrix b))
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (assert-true (= (aref a i j) (matrix-element b i j)) a b)))
    ()))

(define-test knuth
  (let ((a (matrix-from-array #2A((0 0 1 0 1 1 0)
                                  (1 0 0 1 0 0 1)
                                  (0 1 1 0 0 1 0)
                                  (1 0 0 1 0 0 0)
                                  (0 1 0 0 0 0 1)
                                  (0 0 0 1 1 0 1)))))
    (multiple-value-bind (solutions search-tree)
        (solve a :maximum-number-of-solutions nil :search-tree t)
      (assert-equal solutions '((0 3 4)))
      (assert-equal search-tree '((1 (2)) (3 (0 (4))))))))

(defun main (&optional (tests :all))
  (let ((lisp-unit:*print-errors* t)
	(lisp-unit:*print-failures* t)
	(lisp-unit:*print-summary* t))
    (run-tests tests :rs-dlx-tests)))
#+()
(main)

;;; tests.lisp ends here
