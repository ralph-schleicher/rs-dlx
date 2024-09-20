;;; api.lisp --- programming interface

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

(in-package :rs-dlx)

(defun make-matrix (rows columns &key (element-type 'bit) (null-element nil null-element-supplied-p))
  "Create a new matrix object.

A matrix object is a sparse matrix based on circular doubly linked
lists as described in Knuth's “Dancing Links” paper.  The matrix
elements are addressed by a zero-based row and column index.  Any
index has to be a valid array index.  All matrix elements are set
to the null element initially.  You can read or update a matrix
element with the ‘matrix-element’ function.  A more efficient way
to fill a matrix is to create an empty matrix with either zero
rows or zero columns, then use the ‘add-matrix-row’ function or
‘add-matrix-column’ function respectively to define the non-null
elements.

First argument ROWS is the number of matrix rows.  Value has to
 be a non-negative integer.
Second argument COLUMNS is the number of matrix columns.  Value
 has to be a non-negative integer.
Keyword argument ELEMENT-TYPE specifies the type of the values
 intended to be stored in the matrix elements.  Value is a type
 specifier.  Default is ‘bit’.
Keyword argument NULL-ELEMENT is the value of a null element.
 Value has to be either the number zero or ‘nil’.  Default is
 the number zero if ELEMENT-TYPE specifies a numeric type.
 Otherwise, the default is ‘nil’.

Value is a matrix object.

See also ‘matrix-rows’, ‘matrix-columns’, ‘matrix-element’,
‘matrix-elements’, ‘add-matrix-row’, ‘add-matrix-column’,
‘matrix-from-array’, and ‘array-from-matrix’.

Exceptional Situations:

   * The consequences are undefined if ELEMENT-TYPE is not
     a valid type specifier.

Examples:

;; Create a logical matrix with elements in {0, 1}.
(array-from-matrix (make-matrix 3 4))
 ⇒ #2A((0 0 0 0) (0 0 0 0) (0 0 0 0))

;; Create a logical matrix of generalized Boolean values.
(array-from-matrix (make-matrix 3 4 :element-type t))
 ⇒ #2A((NIL NIL NIL NIL) (NIL NIL NIL NIL) (NIL NIL NIL NIL))"
  (check-type rows alexandria:non-negative-fixnum)
  (check-type columns alexandria:non-negative-fixnum)
  (unless null-element-supplied-p
    (setf null-element (when (subtypep element-type 'number)
                         (coerce 0 element-type))))
  ;; Sanity check.
  (unless (subtypep element-type t)
    (error 'simple-type-error
           :format-control "Invalid type specifier: ~S"
           :format-arguments (list element-type)))
  (unless (typep null-element element-type)
    (error 'type-error :datum null-element :expected-type element-type))
  ;; This restriction enables the automatic computation of the logical
  ;; complement, see the ‘not-null-element’ function.
  (unless (if (numberp null-element)
              (zerop null-element)
            (null null-element))
    (error 'simple-type-error
           :format-control "Invalid null element: ~S"
           :format-arguments (list null-element)))
  (let ((matrix (make-root rows columns element-type null-element)))
    (iter (for index :from 0 :below rows)
          (column-insert-node-before (make-header index) matrix))
    (iter (for index :from 0 :below columns)
          (row-insert-node-before (make-header index) matrix))
    ;; Return value.
    matrix))

(defun matrix-rows (matrix)
  "Return the number of matrix rows.

Argument MATRIX is a matrix object.

Value is the number of matrix rows."
  (check-type matrix root)
  (rows matrix))

(defun matrix-columns (matrix)
  "Return the number of matrix columns.

Argument MATRIX is a matrix object.

Value is the number of matrix columns."
  (check-type matrix root)
  (columns matrix))

(defun matrix-element (matrix row-index column-index)
  "Read or update the value of a matrix element.

First argument MATRIX is a matrix object.
Second argument ROW-INDEX is the row index of the matrix element.
Third argument COLUMN-INDEX is the column index of the matrix element.

Value is the value of the referenced matrix element.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if ROW-INDEX or
     COLUMN-INDEX is out of bounds.

   * Signals an error of type ‘type-error’ if the new value is
     not compatible with the matrix element type."
  (check-type matrix root)
  (check-type row-index fixnum)
  (check-type column-index fixnum)
  (let ((row-header (row-header matrix row-index))
        (column-header (column-header matrix column-index)))
    (when (and (plusp (size row-header))
               (plusp (size column-header)))
      ;; There may be a matching element
      (if (< (size row-header) (size column-header))
          ;; Search in row.
          (iter (with node = row-header)
                (setf node (right node))
                (until (eq node row-header))
                (when (eq (column node) column-header)
                  (return-from matrix-element (value node))))
        ;; Search in column.
        (iter (with node = column-header)
              (setf node (down node))
              (until (eq node column-header))
              (when (eq (row node) row-header)
                (return-from matrix-element (value node)))))))
  ;; Element does not exist.
  (null-element matrix))

(defun (setf matrix-element) (value matrix row-index column-index)
  (check-type matrix root)
  (check-type row-index fixnum)
  (check-type column-index fixnum)
  (let* ((row-header (row-header matrix row-index))
         (row-node row-header)
         (column-header (column-header matrix column-index))
         (column-node column-header)
         (node nil))
    (unless (element-type-p value matrix)
      (error 'type-error :datum value :expected-type (element-type matrix)))
    (when (and (null node) (plusp (size row-header)))
      (iter (setf row-node (right row-node))
            ;; If the column index is larger than any existing
            ;; element.  Insert the new element before the row
            ;; header.
            (until (eq row-node row-header))
            ;; If there is an element with a greater column index,
            ;; insert the new element before this element.
            (until (> (index (column row-node)) column-index))
            ;; If there is an element with that column index,
            ;; update this element.
            (when (= (index (column row-node)) column-index)
              (setf node row-node)
              (finish))))
    (when (and (null node) (plusp (size column-header)))
      (iter (setf column-node (down column-node))
            (until (eq column-node column-header))
            (until (> (index (row column-node)) row-index))
            (when (= (index (row column-node)) row-index)
              (setf node column-node)
              (finish))))
    (cond ((null node)
           ;; Insert a new element.
           (unless (null-element-p value matrix)
             (setf node (make-element row-header column-header value))
             (row-insert-node-before node row-node)
             (incf (size row-header))
             (column-insert-node-before node column-node)
             (incf (size column-header))))
          ((null-element-p value matrix)
           ;; Remove existing element.
           (row-remove-node node)
           (decf (size row-header))
           (column-remove-node node)
           (decf (size column-header)))
          ;; Update existing element.
          ((setf (value node) value)))
    ;; Return value.
    value))

(defun (setf matrix-elements) (value matrix &optional (start-row 0) (start-column 0))
  "Update multiple elements of a matrix.

First argument MATRIX is a matrix object.
Optional second argument START-ROW is the row index of the first
 matrix element.  Default is zero.
Optional third argument START-COLUMN is the column index of the
 first matrix element.  Default is zero.

Value is a two-dimensional array whose elements will be assigned
to the block starting at START-ROW and START-COLUMN.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if START-ROW or
     START-COLUMN is out of bounds or if the dimensions of the
     new value relative to START-ROW and START-COLUMN exceed
     the matrix dimensions.

   * Signals an error of type ‘type-error’ if the array element
     type of the new value is not a subtype of the matrix element
     type."
  (check-type matrix root)
  (check-type start-row fixnum)
  (check-type start-column fixnum)
  (check-type value (array * 2))
  (let ((m (array-dimension value 0))
        (n (array-dimension value 1)))
    (when (or (< start-row 0)
              (> start-row (- (rows matrix) m))
              (< start-column 0)
              (> start-column (- (columns matrix) n)))
      (error 'program-error))
    (unless (subtypep (array-element-type value) (element-type matrix))
      (error 'type-error :datum value :expected-type `(array ,(element-type matrix) 2)))
    ;; FIXME: This is the most simple but also the least efficient way
    ;; to update the matrix elements.
    (iter (for i :from 0 :below m)
          (iter (for j :from 0 :below n)
                (setf (matrix-element matrix (+ start-row i) (+ start-column j)) (aref value i j)))))
  ;; Return value.
  value)

(defun matrix-from-array (array &key (element-type 'bit) (null-element nil null-element-supplied-p))
  "Convert an array into a matrix object.

Argument ARRAY is an array with two dimensions.
Keyword argument ELEMENT-TYPE specifies the type of the values
 intended to be stored in the matrix elements.  Value is a type
 specifier.  Default is ‘bit’.
Keyword argument NULL-ELEMENT defines the value of a null element.
 Default is the number zero if ELEMENT-TYPE specifies a numeric
 type.  Otherwise, the default is ‘nil’.

Value is a matrix object.

See also ‘array-from-matrix’ and ‘make-matrix’.

Exceptional Situations:

   * The consequences are undefined if ELEMENT-TYPE is not
     a valid type specifier."
  (check-type array (array * 2))
  (unless null-element-supplied-p
    (setf null-element (when (subtypep element-type 'number)
                         (coerce 0 element-type))))
  (let ((m (array-dimension array 0))
        (n (array-dimension array 1)))
    (let ((matrix (make-matrix m n :element-type element-type :null-element null-element)))
      (iter (for i :from 0 :below m)
            (iter (for j :from 0 :below n)
                  (for value = (aref array i j))
                  (unless (null-element-p value matrix)
                    (setf (matrix-element matrix i j) value))))
      ;; Return value.
      matrix)))

(defun array-from-matrix (matrix &key (element-type nil element-type-supplied-p))
  "Convert a matrix object into an array.

Argument MATRIX is a matrix object.
Keyword argument ELEMENT-TYPE specifies the type of the values
 intended to be stored in the array elements.  Value is a type
 specifier.  Default is the element type of the matrix object.

Value is an array with two dimensions.

See also ‘matrix-from-array’ and ‘make-matrix’.

Exceptional Situations:

   * The consequences are undefined if ELEMENT-TYPE is not
     a valid type specifier."
  (check-type matrix root)
  (unless element-type-supplied-p
    (setf element-type (element-type matrix)))
  (let ((array (make-array (list (rows matrix) (columns matrix)) :element-type element-type :initial-element (null-element matrix))))
    (iter (with row-header = matrix)
          (setf row-header (down row-header))
          (until (eq row-header matrix))
          (for i = (index row-header))
          (iter (with node = row-header)
                (setf node (right node))
                (until (eq node row-header))
                (for j = (index (column node)))
                (setf (aref array i j) (value node))))
    ;; Return value.
    array))

(defun add-matrix-row (matrix elements &key name (initial-element nil initial-element-supplied-p))
  "Add a row vector at the end of a matrix.

First argument MATRIX is a matrix object.
Second argument ELEMENTS specifies the matrix elements.  Value has
 to be a list with elements of the form ‘(INDEX . VALUE)’ or just
 ‘INDEX’ where INDEX and VALUE is the column index and value of a
 matrix element.  For a plain ‘INDEX’, a value of one is used.
 The matrix elements have to be supplied in strictly monotonically
 increasing order of the column index.
Keyword argument NAME is the symbolic identifier of the new matrix
 row.

Value is the matrix object.  If an error occurs, the structure of
the original matrix is retained.

See also ‘make-matrix’, ‘matrix-element’, and ‘add-matrix-column’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if the matrix
     elements are not specified in strictly monotonically
     increasing order of the column indices or if a column
     index is out of bounds.

Examples:

(array-from-matrix
 (add-matrix-row (add-matrix-row (make-matrix 0 5) '(1 2 4)) '(0 3)))
 ⇒ #2A((0 1 1 0 1) (1 0 0 1 0))"
  (check-type matrix root)
  (check-type elements list)
  (unless initial-element-supplied-p
    (setf initial-element (not-null-element matrix)))
  (unless (element-type-p initial-element matrix)
    (error 'type-error :datum initial-element :expected-type (element-type matrix)))
  (let ((row-header (make-header (rows matrix) name)))
    ;; Attempt to insert the matrix elements.
    ;; If an error occurs, restore the original
    ;; matrix structure.
    (handler-case
        (let ((index-type 'fixnum))
          (iter (for spec :in elements)
                ;; Decode the element specification.
                (for (index . value) = (if (consp spec) spec (cons spec initial-element)))
                (unless (typep index index-type)
                  (error 'type-error :datum index :expected-type index-type))
                (unless (element-type-p value matrix)
                  (error 'type-error :datum value :expected-type (element-type matrix)))
                ;; Move forward to the corresponding column.
                (with column-header = matrix)
                (iter (setf column-header (right column-header))
                      (when (eq column-header matrix)
                        (error 'program-error))
                      (until (= (index column-header) index)))
                ;; Insert the element.
                (unless (null-element-p value matrix)
                  (let ((node (make-element row-header column-header value)))
                    (row-insert-node-before node row-header)
                    (incf (size row-header))
                    (column-insert-node-before node column-header)
                    (incf (size column-header)))))
          ;; Insert the new row header at the end.
          (column-insert-node-before row-header matrix)
          ;; Adjust the matrix dimensions.
          (incf (rows matrix)))
      (error (condition)
        ;; Undo the insertions.
        (iter (with node = row-header)
              (setf node (right node))
              (until (eq node row-header))
              (decf (size (column node)))
              (column-remove-node node))
        ;; Re-throw the error.
        (error condition))))
  ;; Return value.
  matrix)

(defun add-matrix-column (matrix elements &key name (initial-element nil initial-element-supplied-p))
  "Add a column vector at the end of a matrix.

First argument MATRIX is a matrix object.
Second argument ELEMENTS specifies the matrix elements.  Value has
 to be a list with elements of the form ‘(INDEX . VALUE)’ or just
 ‘INDEX’ where INDEX and VALUE is the row index and value of a
 matrix element.  For a plain ‘INDEX’, a value of one is used.
 The matrix elements have to be supplied in strictly monotonically
 increasing order of the row index.
Keyword argument NAME is the symbolic identifier of the new matrix
 column.

Value is the matrix object.  If an error occurs, the structure of
the original matrix is retained.

See also ‘make-matrix’, ‘matrix-element’, and ‘add-matrix-row’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if the matrix
     elements are not specified in strictly monotonically
     increasing order of the row indices or if a row index
     is out of bounds.

Examples:

(array-from-matrix
 (add-matrix-column (add-matrix-column (make-matrix 5 0) '(1 2 4)) '(0 3)))
 ⇒ #2A((0 1) (1 0) (1 0) (0 1) (1 0))"
  (check-type matrix root)
  (check-type elements list)
  (unless initial-element-supplied-p
    (setf initial-element (not-null-element matrix)))
  (unless (element-type-p initial-element matrix)
    (error 'type-error :datum initial-element :expected-type (element-type matrix)))
  (let ((column-header (make-header (columns matrix) name)))
    (handler-case
        (let ((index-type 'fixnum))
          (iter (for spec :in elements)
                ;; Decode the element specification.
                (for (index . value) = (if (consp spec) spec (cons spec initial-element)))
                (unless (typep index index-type)
                  (error 'type-error :datum index :expected-type index-type))
                (unless (element-type-p value matrix)
                  (error 'type-error :datum value :expected-type (element-type matrix)))
                ;; Move forward to the corresponding row.
                (with row-header = matrix)
                (iter (setf row-header (down row-header))
                      (when (eq row-header matrix)
                        (error 'program-error))
                      (until (= (index row-header) index)))
                ;; Insert the element.
                (unless (null-element-p value matrix)
                  (let ((node (make-element row-header column-header value)))
                    (row-insert-node-before node row-header)
                    (incf (size row-header))
                    (column-insert-node-before node column-header)
                    (incf (size column-header)))))
          ;; Insert the new column header at the end.
          (row-insert-node-before column-header matrix)
          ;; Adjust the matrix dimensions.
          (incf (columns matrix)))
      (error (condition)
        ;; Undo the insertions.
        (iter (with node = column-header)
              (setf node (down node))
              (until (eq node column-header))
              (decf (size (row node)))
              (row-remove-node node))
        ;; Re-throw the error.
        (error condition))))
  ;; Return value.
  matrix)

(defun %remove-matrix-row (matrix header)
  "Remove a matrix row."
  (decf (rows matrix))
  (remove-row header)
  ;; Renumber the following rows.
  (iter (for index :from (index header))
        (setf header (down header))
        (until (eq header matrix))
        (setf (index header) index))
  ;; Return value.
  matrix)

(defun %remove-matrix-column (matrix header)
  "Remove a matrix column."
  (decf (columns matrix))
  (remove-column header)
  ;; Renumber the following columns.
  (iter (for index :from (index header))
        (setf header (right header))
        (until (eq header matrix))
        (setf (index header) index))
  ;; Return value.
  matrix)

(defun remove-matrix-row (matrix index)
  "Remove a matrix row.

First argument MATRIX is a matrix object.
Second argument INDEX is a row index.

Value is the matrix object.  If an error occurs, the structure of the
original matrix is retained.

See also ‘remove-matrix-row-by-name’ and ‘remove-matrix-rows’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out of
     bounds."
  (check-type matrix root)
  (check-type index fixnum)
  (%remove-matrix-row matrix (row-header matrix index)))

(defun remove-matrix-column (matrix index)
  "Remove a matrix column.

First argument MATRIX is a matrix object.
Second argument INDEX is a column index.

Value is the matrix object.  If an error occurs, the structure of the
original matrix is retained.

See also ‘remove-matrix-column-by-name’ and ‘remove-matrix-columns’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out of
     bounds."
  (check-type matrix root)
  (check-type index fixnum)
  (%remove-matrix-column matrix (column-header matrix index)))

(defun remove-matrix-row-by-name (matrix name &key (test #'eql))
  "Remove a matrix row.

First argument MATRIX is a matrix object.
Second argument NAME is a row name.  Only the first row name matching
 NAME will be removed.
Keyword argument TEST defines the function to compare two row names
 for equality.  Default is ‘eql’.

Value is the matrix object.  If an error occurs, the structure of the
original matrix is retained.

See also ‘remove-matrix-row’ and ‘remove-matrix-rows-by-name’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if no row named
     NAME exists."
  (check-type matrix root)
  (check-type test function)
  (iter (with header = matrix)
        (setf header (down header))
        (when (eq header matrix)
          (error 'program-error))
        (when (funcall test (name header) name)
          (return (%remove-matrix-row matrix header)))))

(defun remove-matrix-column-by-name (matrix name &key (test #'eql))
  "Remove a matrix column.

First argument MATRIX is a matrix object.
Second argument NAME is a column name.  Only the first column name
 matching NAME will be removed.
Keyword argument TEST defines the function to compare two column names
 for equality.  Default is ‘eql’.

Value is the matrix object.  If an error occurs, the structure of the
original matrix is retained.

See also ‘remove-matrix-column’ and ‘remove-matrix-columns-by-name’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if no column named
     NAME exists."
  (check-type matrix root)
  (check-type test function)
  (iter (with header = matrix)
        (setf header (right header))
        (when (eq header matrix)
          (error 'program-error))
        (when (funcall test (name header) name)
          (return (%remove-matrix-column matrix header)))))

(defun %remove-matrix-rows (matrix items key test)
  "Remove some matrix rows."
  (let ((index 0))
    (iter (with header = matrix)
          (setf header (down header))
          (until (eq header matrix))
          (for item = (funcall key header))
          (if (member item items :test test)
              (remove-row header)
            ;; Renumber the remaining rows.
            (progn
              (setf (index header) index)
              (incf index))))
    ;; Number of remaining rows.
    (setf (rows matrix) index))
  ;; Return value.
  matrix)

(defun %remove-matrix-columns (matrix items key test)
  "Remove some matrix columns."
  (let ((index 0))
    (iter (with header = matrix)
          (setf header (right header))
          (until (eq header matrix))
          (for item = (funcall key header))
          (if (member item items :test test)
              (remove-column header)
            ;; Renumber the remaining columns.
            (progn
              (setf (index header) index)
              (incf index))))
    ;; Number of remaining columns.
    (setf (columns matrix) index))
  ;; Return value.
  matrix)

(defun remove-matrix-rows (matrix indices)
  "Remove some matrix rows.

First argument MATRIX is a matrix object.
Second argument INDICES is a list of row indices.  No error is
 signaled if a row index does not exist.

Value is the matrix object.

See also ‘remove-matrix-rows-by-name’ and ‘remove-matrix-row’."
  (check-type matrix root)
  (check-type indices list)
  (%remove-matrix-rows matrix indices #'header-index #'eql))

(defun remove-matrix-columns (matrix indices)
  "Remove some matrix columns.

First argument MATRIX is a matrix object.
Second argument INDICES is a list of column indices.  No error is
 signaled if a column index does not exist.

Value is the matrix object.

See also ‘remove-matrix-columns-by-name’ and ‘remove-matrix-column’."
  (check-type matrix root)
  (check-type indices list)
  (%remove-matrix-columns matrix indices #'header-index #'eql))

(defun remove-matrix-rows-by-name (matrix names &key (test #'eql))
  "Remove some matrix rows.

First argument MATRIX is a matrix object.
Second argument NAMES is a list of row names.  No error is signaled
 if a row name does not exist.
Keyword argument TEST defines the function to compare two row names
 for equality.  Default is ‘eql’.

Value is the matrix object.

See also ‘remove-matrix-rows’ and ‘remove-matrix-row-by-name’."
  (check-type matrix root)
  (check-type names list)
  (check-type test function)
  (%remove-matrix-rows matrix names #'header-name test))

(defun remove-matrix-columns-by-name (matrix names &key (test #'eql))
  "Remove some matrix columns.

First argument MATRIX is a matrix object.
Second argument NAMES is a list of column names.  No error is signaled
 if a column name does not exist.
Keyword argument TEST defines the function to compare two column names
 for equality.  Default is ‘eql’.

Value is the matrix object.

See also ‘remove-matrix-columns’ and ‘remove-matrix-column-by-name’."
  (check-type matrix root)
  (check-type names list)
  (check-type test function)
  (%remove-matrix-columns matrix names #'header-name test))

(defun map-matrix-row (function matrix index)
  "Apply a function to every non-null element in a matrix row.

First argument FUNCTION is the function to be applied.
Second argument MATRIX is the matrix object.
Third argument INDEX is the row index.

The function is called with one argument, the matrix element.
Candidates for the FUNCTION argument are, e.g. ‘column-index’
or ‘column-name’.

Value is a list containing the results returned by FUNCTION.

See also ‘map-matrix-column’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out
     of bounds."
  (check-type function function)
  (check-type matrix root)
  (check-type index fixnum)
  (iter (with header = (row-header matrix index))
        (with node = header)
        (setf node (right node))
        (until (eq node header))
        (collect (funcall function node))))

(defun map-matrix-column (function matrix index)
  "Apply a function to every non-null element in a matrix column.

First argument FUNCTION is the function to be applied.
Second argument MATRIX is the matrix object.
Third argument INDEX is the column index.

The function is called with one argument, the matrix element.
Candidates for the FUNCTION argument are, e.g. ‘row-index’ or
‘row-name’.

Value is a list containing the results returned by FUNCTION.

See also ‘map-matrix-row’.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out
     of bounds."
  (check-type function function)
  (check-type matrix root)
  (check-type index fixnum)
  (iter (with header = (column-header matrix index))
        (with node = header)
        (setf node (down node))
        (until (eq node header))
        (collect (funcall function node))))

(defun row-index (object)
  "Return the row index of a matrix element.

Argument OBJECT can be a matrix element or a row header.

Value is the zero-based row index."
  (etypecase object
    (element
     (index (row object)))
    (header
     (index object))))

(defun column-index (object)
  "Return the index of a matrix column.

Argument OBJECT can be a matrix element or a column header.

Value is the zero-based column index."
  (etypecase object
    (element
     (index (column object)))
    (header
     (index object))))

(defun row-name (object &optional index)
  "Read or update the symbolic identifier of a matrix row.

First argument OBJECT can be a matrix, a row header, or a matrix
 element.
Optional second argument INDEX is the row index.  This argument is
 only required if OBJECT is a matrix.

Value is the symbolic identifier of the matrix row.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out
     out bounds."
  (name (etypecase object
          (root
           (check-type index fixnum)
           (row-header object index))
          (header
           object)
          (element
           (row object)))))

(defun (setf row-name) (value object &optional index)
  (setf (name (etypecase object
                (root
                 (check-type index fixnum)
                 (row-header object index))
                (header
                 object)
                (element
                 (row object)))) value))

(defun column-name (object &optional index)
  "Read or update the symbolic identifier of a matrix column.

First argument OBJECT can be a matrix, a column header, or a matrix
 element.
Optional second argument INDEX is the column index.  This argument is
 only required if OBJECT is a matrix.

Value is the symbolic identifier of the matrix column.

Exceptional Situations:

   * Signals an error of type ‘program-error’ if INDEX is out
     out bounds."
  (name (etypecase object
          (root
           (check-type index fixnum)
           (column-header object index))
          (header
           object)
          (element
           (column object)))))

(defun (setf column-name) (value object &optional index)
  (setf (name (etypecase object
                (root
                 (check-type index fixnum)
                 (column-header object index))
                (header
                 object)
                (element
                 (column object)))) value))

(defun (setf row-names) (value matrix)
  "Update the symbolic identifiers of the matrix rows.

Argument MATRIX is a matrix object.

Value is a list of row names.  Not more than the total number of
matrix rows are updated.  If the list is shorter, the names of the
remaining matrix rows are set to ‘nil’."
  (check-type matrix root)
  (check-type value list)
  (let ((header (down matrix)))
    (iter (for name :in value)
          (until (eq header matrix))
          (setf (name header) name)
          (setf header (down header)))
    ;; Clear remaining headers.
    (iter (until (eq header matrix))
          (setf (name header) nil)
          (setf header (down header))))
  ;; Return value.
  value)

(defun (setf column-names) (value matrix)
  "Update the symbolic identifiers of the matrix columns.

Argument MATRIX is a matrix object.

Value is a list of column names.  Not more than the total number of
matrix columns are updated.  If the list is shorter, the names of the
remaining matrix columns are set to ‘nil’."
  (check-type matrix root)
  (check-type value list)
  (let ((header (right matrix)))
    (iter (for name :in value)
          (until (eq header matrix))
          (setf (name header) name)
          (setf header (right header)))
    ;; Clear remaining headers.
    (iter (until (eq header matrix))
          (setf (name header) nil)
          (setf header (right header))))
  ;; Return value.
  value)

;;; api.lisp ends here
