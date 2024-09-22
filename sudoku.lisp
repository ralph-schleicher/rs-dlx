;;; sudoku.lisp --- Sudoku solver

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

(defpackage #:de.ralph-schleicher.dlx-sudoku
  (:nicknames :rs-dlx-sudoku)
  (:use :common-lisp
        :iterate)
  (:export #:solve)
  (:documentation
   "A Sudoku solver based on RS-DLX."))

(in-package :rs-dlx-sudoku)

(defvar *board* nil
  "The current Sudoku board.")

(defun make-board ()
  "Create a new Sudoku board."
  (make-array '(9 9) :initial-element ()))

(defun make-marks ()
  "Create a fresh list of pencil marks."
  (list 1 2 3 4 5 6 7 8 9))

(defun not-given-p (value)
  "Return true if cell value VALUE is not given."
  (listp value))

(defun givenp (value)
  "Return true if cell value VALUE is given."
  (integerp value))

(defun eliminate (mark row-index column-index)
  "Remove pencil mark MARK from all cells in ROW-INDEX and COLUMN-INDEX."
  (iter (for column :from 0 :below 9)
        (for value = (aref *board* row-index column))
        (when (not-given-p value)
          (setf (aref *board* row-index column) (delete mark value))))
  (iter (for row :from 0 :below 9)
        (for value = (aref *board* row column-index))
        (when (not-given-p value)
          (setf (aref *board* row column-index) (delete mark value))))
  ;; Likewise for the cell's block.
  (let ((start-row (* (truncate row-index 3) 3))
        (start-column (* (truncate column-index 3) 3)))
    (iter (for row :from start-row :below (+ start-row 3))
          (iter (for column :from start-column :below (+ start-column 3))
                (for value = (aref *board* row column))
                (when (not-given-p value)
                  (setf (aref *board* row column) (delete mark value))))))
  ())

(defvar name-package (find-package :rs-dlx-sudoku)
  "The home package for row and column names.")

(defun encode-name (row column number &optional raw)
  "Encode the name of a possibility.

If optional argument RAW is true, arguments ROW and COLUMN are already
 one-based row and column indices.

Value is a symbol in the ‘name-package’ package."
  (when (not raw)
    (incf row)
    (incf column))
  (intern (format nil "R~AC~AN~A" row column number) name-package))

(defun decode-name (name &optional raw)
  "Decode the name of a possibility.

First argument NAME is a row name as returned by the ‘encode-name’
 function.
If optional second argument RAW is true, return the row index and
 column index as one-based indices.

Return the row index, column index, and number as multiple values."
  (let ((name (symbol-name name)))
    (let ((row    (digit-char-p (aref name 1)))
          (column (digit-char-p (aref name 3)))
          (number (digit-char-p (aref name 5))))
      (when (not raw)
        (decf row)
        (decf column))
      (values row column number))))

(defvar all-possibilities
  (let (result)
    (iter (for row :from 1 :to 9)
          (iter (for column :from 1 :to 9)
                (iter (for number :from 1 :to 9)
                      (push (encode-name row column number t) result))))
    (nreverse result))
  "A flat list of all possibilities.")

(defvar sudoku-matrix-elements ()
  "A list of non-null incidence matrix elements for Sudoku.
List elements are cons cells of the form ‘(ROW . COLUMNS)’ where ROW
is a row name and COLUMNS is a list of column indices of all non-null
matrix elements of this row.")

(defun initialize-sudoku-matrix (a)
  "Fill in the incidence matrix A from scratch.
Argument A is a 729×324 null matrix."
  (unless (and (= (rs-dlx:matrix-rows a) 729)
               (= (rs-dlx:matrix-columns a) 324))
    (error 'program-error))
  (let ((e (make-array '(9 9)
                       :element-type 'bit
                       :initial-contents '((1 0 0 0 0 0 0 0 0)
                                           (0 1 0 0 0 0 0 0 0)
                                           (0 0 1 0 0 0 0 0 0)
                                           (0 0 0 1 0 0 0 0 0)
                                           (0 0 0 0 1 0 0 0 0)
                                           (0 0 0 0 0 1 0 0 0)
                                           (0 0 0 0 0 0 1 0 0)
                                           (0 0 0 0 0 0 0 1 0)
                                           (0 0 0 0 0 0 0 0 1))))
        (i 0)
        (j 0))
    ;; Reset the row names.
    (setf (rs-dlx:row-names a) all-possibilities)
    ;; Cell constraints.  All possible numbers for a cell.
    ;;
    ;; R1C1 = {R1C1N1, R1C1N2, R1C1N3, R1C1N4, R1C1N5, R1C1N6, R1C1N7, R1C1N8, R1C1N9}
    (setf i 0)
    (iter (repeat 81)
          (iter (repeat 9)
                (setf (rs-dlx:matrix-element a i j) 1)
                (incf i))
          (incf j))
    ;; Row constraints.  All possibilities for a number in a row.
    ;;
    ;; R1N1 = {R1C1N1, R1C2N1, R1C3N1, R1C4N1, R1C5N1, R1C6N1, R1C7N1, R1C8N1, R1C9N1}
    (setf i 0)
    (iter (repeat 9)
          (iter (repeat 9)
                (setf (rs-dlx:matrix-elements a i j) e)
                (incf i 9))
          (incf j 9))
    ;; Column constraints.  All possibilities for a number in a column.
    ;;
    ;; C1N1 = {R1C1N1, R2C1N1, R3C1N1, R4C1N1, R5C1N1, R6C1N1, R7C1N1, R8C1N1, R9C1N1}
    (setf i 0)
    (iter (repeat 9)
          (iter (repeat 9)
                (setf (rs-dlx:matrix-elements a i j) e)
                (incf i 9)
                (incf j 9))
          (decf j 81))
    (incf j 81)
    ;; Block constraints.  All possibilities for a number in a 3×3 block.
    ;; Blocks are numbered sequentially in row major mode.
    ;;
    ;; B1N1 = {R1C1N1, R1C2N1, R1C3N1, R2C1N1, R2C2N1, R2C3N1, R3C1N1, R3C2N1, R3C3N1}
    (setf i 0)
    (iter (repeat 3)
          (iter (repeat 3)
                (iter (repeat 3)
                      (iter (repeat 3)
                            (setf (rs-dlx:matrix-elements a i j) e)
                            (incf i 9))
                      (incf j 9))
                (decf j 27))
          (incf j 27))
    ;; Return value.
    a))

(defun make-sudoku-matrix ()
  "Create a new incidence matrix for Sudoku."
  (let ((m (* 9 9 9)) ;729
        (n (* 9 9 4)) ;324
        ;; The matrix.
        (a nil))
    (cond ((null sudoku-matrix-elements)
           (setf a (initialize-sudoku-matrix (rs-dlx:make-matrix m n)))
           ;; Update cache.
           (iter (for i :from 0 :below m)
                 ;; See call of ‘rs-dlx:add-matrix-row’ below.
                 (push (cons (rs-dlx:row-name a i)
                             (rs-dlx:map-matrix-row #'rs-dlx:column-index a i))
                       sudoku-matrix-elements))
           (setf sudoku-matrix-elements (nreverse sudoku-matrix-elements)))
          (t
           ;; Load from cache.
           (setf a (rs-dlx:make-matrix 0 n))
           (mapc (lambda (cell)
                   (rs-dlx:add-matrix-row a (cdr cell) :name (car cell)))
                 sudoku-matrix-elements)))
    ;; Return value.
    a))

(defun solve (&rest sequences)
  "Solve a Sudoku puzzle.

Arguments are sequences of numbers in the range from 1 to 9 inclusive
 or characters in the range from ‘1’ to ‘9’ inclusive.  All sequences
 are rearranged to a 9×9 matrix in row major layout.  The number 0,
 the symbol ‘nil’, the space character ‘ ’, and the character
 ‘.’ (period) represent non-given numbers.

Value is a 9×9 array of integers in the range from 1 to 9 inclusive
which is one possible solution of the Sudoku puzzle."
  (let ((*board* (make-board)))
    ;; Import the board.
    (let ((vector (apply #'concatenate 'vector sequences)))
      (unless (= (length vector) 81)
        (error "Wrong number of elements."))
      (iter (for pos :from 0 :below 81)
            (multiple-value-bind (row column)
                (truncate pos 9)
              (setf (aref *board* row column)
                    (ecase (aref vector pos)
                      ((1 #\1) 1)
                      ((2 #\2) 2)
                      ((3 #\3) 3)
                      ((4 #\4) 4)
                      ((5 #\5) 5)
                      ((6 #\6) 6)
                      ((7 #\7) 7)
                      ((8 #\8) 8)
                      ((9 #\9) 9)
                      ;; Empty field.
                      ((nil 0 #\Space #\.)
                       (make-marks)))))))
    ;; Initial cleanup.
    (iter (for row :from 0 :below 9)
          (iter (for column :from 0 :below 9)
                (for value = (aref *board* row column))
                (when (givenp value)
                  (eliminate value row column))))
    ;; Solve the board.
    (let ((matrix (make-sudoku-matrix)) failure)
      (let (possibilities)
        (iter (for row :from 0 :below 9)
              (iter (for column :from 0 :below 9)
                    (for value = (aref *board* row column))
                    (if (givenp value)
                        (push (encode-name row column value) possibilities)
                      (iter (for number :in value)
                            (push (encode-name row column number) possibilities)))))
        (rs-dlx:remove-matrix-rows-by-name matrix (set-difference all-possibilities possibilities)))
      (iter (for index :in (first (time (rs-dlx:solve matrix))))
            (multiple-value-bind (row column number)
                (decode-name (rs-dlx:row-name matrix index))
              ;; Check actual solution with initial possibilities.
              (for value = (aref *board* row column))
              (if (if (givenp value)
                      (= number value)
                    (member number value))
                  (setf (aref *board* row column) number)
                (progn
                  (format *trace-output* "Error in cell (~A ~A), solver found ~A but should be ~S.~%" row column number value)
                  (setf failure t)))))
      (unless failure *board*))))

;;; sudoku.lisp ends here
