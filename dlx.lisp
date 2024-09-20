;;; dlx.lisp --- Knuth's Algorithm X with dancing links

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

;;; Commentary:

;; Donald E. Knuth: “Dancing Links”, ‹https://arxiv.org/abs/cs/0011047›.

;;; Code:

(in-package :rs-dlx)

;; Optimize for speed.
(declaim (optimize speed (safety 0) (space 0) (debug 1) (compilation-speed 0)))
#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;;;; Dancing Links Matrix

(defstruct (node
            (:constructor %make-node ())
            (:predicate nil)
            (:copier nil)
            (:conc-name))
  "Base class for all nodes of a dancing links matrix."
  ;; The link to the right node, i.e. the next node in the same row.
  (right nil :type (or null node))
  ;; The link to the left node, i.e. the previous node in the same row.
  (left nil :type (or null node))
  ;; The link to the below node, i.e. the next node in the same column.
  (down nil :type (or null node))
  ;; The link to the above node, i.e. the previous node in the same column.
  (up nil :type (or null node)))

(defun %initialize-node (node)
  "Link a ‘node’ object to itself."
  (declare (type node node))
  (setf (right node) node
        (left node) node
        (down node) node
        (up node) node))

(defun make-node ()
  "Create a new ‘node’ object."
  (%initialize-node (%make-node)))

(defmethod print-object ((node node) stream)
  "Print a ‘node’ object."
  (print-unreadable-object (node stream :type t :identity t)))

;; The basic dance steps.
(defsubst row-remove-node (node)
  "Remove node NODE from it's row."
  (declare (type node node))
  (setf (right (left node)) (right node)
        (left (right node)) (left node)))

(defsubst column-remove-node (node)
  "Remove node NODE from it's column."
  (declare (type node node))
  (setf (down (up node)) (down node)
        (up (down node)) (up node)))

(defsubst row-insert-node (node)
  "Insert node NODE in it's row."
  (declare (type node node))
  (setf (right (left node)) node
        (left (right node)) node))

(defsubst column-insert-node (node)
  "Insert node NODE in it's column."
  (declare (type node node))
  (setf (down (up node)) node
        (up (down node)) node))

(defsubst row-insert-node-before (node other)
  "Insert node NODE in the row before node OTHER."
  (declare (type node node other))
  (setf (right node) other
        (left node) (left other))
  (row-insert-node node))

(defsubst column-insert-node-before (node other)
  "Insert node NODE in the column before node OTHER."
  (declare (type node node other))
  (setf (down node) other
        (up node) (up other))
  (column-insert-node node))

(defstruct (header
            (:include node)
            (:constructor %make-header (index name))
            (:predicate nil)
            (:copier nil)
            (:conc-name))
  "A header node of a dancing links matrix."
  ;; The zero-based row or column index.
  (index 0 :type fixnum)
  ;; The symbolic identifier of the row or column.
  (name nil)
  ;; The number of non-null elements in the row or column.
  (size 0 :type fixnum))

(defun make-header (index &optional name)
  "Create a new ‘header’ object."
  (declare (type fixnum index))
  (%initialize-node (%make-header index name)))

(defmethod print-object ((header header) stream)
  "Print a ‘header’ object."
  (print-unreadable-object (header stream :type t :identity t)
    (format stream "(~S)~@[ ~S~] ~S ~S"
            (index header) (name header)
            :size (size header))))

;; Key functions to lookup a header by index or by name.
(defun header-index (header)
  "Return the header index of HEADER."
  (declare (type header header))
  (index header))

(defun header-name (header)
  "Return the header name of HEADER."
  (declare (type header header))
  (name header))

(defstruct (element
            (:include node)
            (:constructor %make-element (row column value))
            (:predicate nil)
            (:copier nil)
            (:conc-name))
  "An element (data object) of a dancing links matrix."
  ;; The link to the row header.
  (row nil :type (or null header))
  ;; The link to the column header.
  (column nil :type (or null header))
  ;; The value of the element.  Default is the complement of the null
  ;; element.  See the ‘root’ structure below.
  (value t))

(defun make-element (row column value)
  "Create a new ‘element’ object."
  (declare (type (or null header) row column))
  (%initialize-node (%make-element row column value)))

(defmethod print-object ((element element) stream)
  "Print an ‘element’ object."
  (print-unreadable-object (element stream :type t :identity t)
    (format stream "(~S ~S) ~S ~S"
            (alexandria:when-let ((header (row element)))
              (index header))
            (alexandria:when-let ((header (column element)))
              (index header))
            :value (value element))))

(defstruct (root
            (:include node)
            (:constructor %make-root (rows columns element-type null-element))
            (:predicate nil)
            (:copier nil)
            (:conc-name))
  "The root node of a dancing links matrix."
  ;; The number of matrix rows.
  (rows 0 :type fixnum)
  ;; The number of matrix columns.
  (columns 0 :type fixnum)
  ;; The type of a matrix element.
  (element-type t)
  ;; The value of a null element.
  (null-element nil)
  ;; The function to compare two element values for equality.
  ;; Only used if the null element is not a number.
  (test #'eql :type (function (t t) t)))

(defun make-root (rows columns element-type null-element)
  "Create a new ‘root’ object."
  (declare (type fixnum rows columns))
  (%initialize-node (%make-root rows columns element-type null-element)))

(defmethod print-object ((root root) stream)
  "Print a ‘root’ object."
  (print-unreadable-object (root stream :type t :identity t)
    (format stream "(~S ~S)" (rows root) (columns root))))

(defun row-index-type (root)
  "Return the type specifier for a valid row index."
  (declare (type root root))
  `(integer 0 (,(rows root))))

(defun column-index-type (root)
  "Return the type specifier for a valid column index."
  (declare (type root root))
  `(integer 0 (,(columns root))))

(defun element-type-p (object root)
  "Return true if OBJECT is a valid element."
  (declare (type root root))
  (typep object (element-type root)))

(defun null-element-p (object root)
  "Return true if OBJECT is a null element."
  (declare (type root root))
  (if (numberp (null-element root))
      (and (numberp object) (= (null-element root) object))
    (funcall (test root) (null-element root) object)))

(defun not-null-element (root)
  "Return the complement of the null element."
  (declare (type root root))
  (if (numberp (null-element root))
      (- 1 (null-element root))
    (not (null-element root))))

(defun row-header (root index)
  "Find a row header by it's index.
Signal a ‘program-error’ if the row does not exist."
  (declare (type root root)
           (type fixnum index))
  (iter (with header = root)
        (setf header (down header))
        (when (eq header root)
          (error 'program-error))
        (when (= (index header) index)
          (return header))))

(defun column-header (root index)
  "Find a column header by it's index.
Signal a ‘program-error’ if the column does not exist."
  (declare (type root root)
           (type fixnum index))
  (iter (with header = root)
        (setf header (right header))
        (when (eq header root)
          (error 'program-error))
        (when (= (index header) index)
          (return header))))

(defun remove-row (header)
  "Remove the matrix row designated by HEADER.
Update the size of affected columns but not the root object
or any row header."
  (declare (type header header))
  (iter (with node = header)
        (setf node (right node))
        (until (eq node header))
        (decf (size (column node)))
        (column-remove-node node))
  (column-remove-node header))

(defun remove-column (header)
  "Remove the matrix column designated by HEADER.
Update the size of affected rows but not the root object
or any column header."
  (declare (type header header))
  (iter (with node = header)
        (setf node (down node))
        (until (eq node header))
        (decf (size (row node)))
        (row-remove-node node))
  (row-remove-node header))

;;;; Algorithm X

(defvar *solution* #()
  "The current solution, i.e. search path.

Value is an adjustable vector with fill pointer.  This is slightly
slower than a simple list but it doesn't cons.")
(declaim (type vector *solution*))

(defvar *solutions* ()
  "The set of found solutions.")
(declaim (type list *solutions*))

(defvar *solution-count* 0
  "The number of found solutions.")
(declaim (type fixnum *solution-count*))

(defvar *solution-count-limit* 1
  "The maximum number of solutions to search for.

A value of ‘nil’ means that there is no limit, i.e. search for all
solutions.  Default is to search for a single solution.")
(declaim (type (or fixnum null) *solution-count-limit*))

(defvar *search-tree* ()
  "The complete search tree.")
(declaim (type list *search-tree*))

(defvar *search-tree-p* nil
  "Whether or not to build the search tree.")
(declaim (type boolean *search-tree-p*))

(defsubst make-solution ()
  "Create a new search path object."
  (make-array 100 :element-type 'fixnum :initial-element 0 :adjustable t :fill-pointer 0)) ;()

(defsubst push-solution (object)
  "Add an item to the current search path."
  (vector-push-extend object *solution* 100)) ;(push object *solution*)

(defsubst pop-solution ()
  "Remove the most recently added item from the current search path."
  (vector-pop *solution*)) ;(pop *solution*)

(defsubst copy-solution ()
  "Create a copy of the current search path as a list."
  (coerce *solution* 'list)) ;(nreverse (copy-list *solution*))

(defsubst non-empty-solution-p ()
  "Return true if the current search path is not empty."
  (plusp (length *solution*))) ;(not (null *solution*))

(defmacro with-tracing (&body body)
  "Mark analysis code."
  (declare (ignorable body))
  #+rs-dlx-trace
  `(progn ,@body)
  #-rs-dlx-trace
  ())

;; To choose a column object c, we could simply set c ← R[h]; this
;; is the leftmost uncovered column.  Or if we want to minimize the
;; branching factor, we could set s ← ∞ and then
;;
;;      for each j ← R[h], R[R[h]], ..., while j ≠ h,
;;           if S[j] < s set c ← j and s ← S[j].
;;
;; Then c is a column with the smallest number of 1s.
(defun choose-column (h)
  "Choose a column header from matrix H."
  (declare (type root h))
  (let (c (s most-positive-fixnum))
    (iter (with j = h)
          (setf j (right j))
          (until (eq j h))
          (for siz = (size j))
          (when (< siz s)
            (setf c j s siz)))
    (with-tracing
      (format *trace-output* "Choose column ~S~%" c))
    c))

;; The operation of covering column c is more interesting: It removes
;; c from the header list and removes all rows in c’s own list from
;; the other column lists they are in.
;;
;;      Set L[R[c]] ← L[c] and R[L[c]] ← R[c].
;;      For each i ← D[c], D[D[c]], ..., while i ≠ c,
;;           for each j ← R[i], R[R[i]], ..., white j ≠ i,
;;                set U[D[j]] ← U[j], D[U[j]] ← D[j],
;;                and set S[C[j]] ← S[C[j]] - 1.
(defun cover-column (c)
  "Cover column C."
  (declare (type header c))
  (with-tracing
    (format *trace-output* "Cover column ~S~%" c))
  (row-remove-node c)
  (iter (with i = c)
        (setf i (down i))
        (until (eq i c))
        (iter (with j = i)
              (setf j (right j))
              (until (eq j i))
              (column-remove-node j)
              (when (typep j 'element)
                (decf (size (column j)))))))

;; Finally, we get to the point of this whole algorithm, the operation
;; of uncovering a given column c.  Here is where the links do their
;; dance:
;;
;;      For each i ← U[c], U[U[c]], ..., while i ≠ c,
;;           for each j ← L[i], L[L[i]], ..., white j ≠ i,
;;                set S[C[j]] ← S[C[j]] + 1,
;;                and set U[D[j]] ← j, D[U[j]] ← j.
;;      Set L[R[c]] ← c and R[L[c]] ← c.
(defun uncover-column (c)
  "Uncover column C."
  (declare (type header c))
  (with-tracing
    (format *trace-output* "Uncover column ~S~%" c))
  (iter (with i = c)
        (setf i (up i))
        (until (eq i c))
        (iter (with j = i)
              (setf j (left j))
              (until (eq j i))
              (column-insert-node j)
              (when (typep j 'element)
                (incf (size (column j))))))
  (row-insert-node c))

;; Our nondeterministic algorithm to find all exact covers can now be
;; cast in the following explicit, deterministic form as a recursive
;; procedure search(k), which is invoked initially with k = 0:
;;
;;      If R[h] = h, print the current solution and return.
;;      Otherwise choose a column object c.
;;      Cover column c.
;;      For each r ← D[c], D[D[c]], ..., while r ≠ c,
;;           set Oₖ ← r;
;;           for each j ← R[r], R[R[r]], ..., while j ≠ r,
;;                cover column j;
;;           search(k + 1);
;;           set r ← Oₖ and c ← C[r];
;;           for each j ← L[r], L[L[r]], ..., while j ≠ r,
;;                uncover column j.
;;      Uncover column c and return.
(defun search (h k)
  "Knuth's Algorithm X."
  (declare (type root h)
           (type fixnum k))
  (with-tracing
    (format *trace-output* "Search depth ~S~%" k))
  (if (eq (right h) h)
      ;; Found a solution.
      (when (non-empty-solution-p)
        (with-tracing
          (format *trace-output* "Found solution ~S~%" *solution*))
        (when (or (null *solution-count-limit*) (< *solution-count* *solution-count-limit*))
          (push (copy-solution) *solutions*)
          (incf *solution-count*)))
    (let ((c (choose-column h)))
      (if (eq (down c) c)
          ;; The body of the algorithm (see below) is a no-op.
          (with-tracing
            (format *trace-output* "No solution~%"))
        (progn
          (cover-column c)
          (iter (with r = c)
                (setf r (down r))
                (until (eq r c))
                (when (and *solution-count-limit* (>= *solution-count* *solution-count-limit*))
                  (finish))
                (for o = (index (row r)))
                (with-tracing
                  (format *trace-output* "Add to solution ~S~%" o))
                (push-solution o)
                (iter (with j = r)
                      (setf j (right j))
                      (until (eq j r))
                      (when (typep j 'element)
                        (cover-column (column j))))
                (search h (1+ k))
                (with-tracing
                  (format *trace-output* "Remove from solution ~S~%" o))
                (pop-solution)
                (iter (with j = r)
                      (setf j (left j))
                      (until (eq j r))
                      (when (typep j 'element)
                        (uncover-column (column j)))))
          (uncover-column c))))))

(defun solve (matrix &key (maximum-number-of-solutions 1) (if-does-not-exist :error) raw)
  "Apply Knuth's Algorithm X to an exact cover problem.

Argument MATRIX is the incident matrix of the exact cover problem
 where the rows represent the set of choices (a.k.a. possibilities
 or candidates) and the columns represent the set of constraints.
 Value is a matrix object.
Keyword argument MAXIMUM-NUMBER-OF-SOLUTIONS limits the number of
 solutions to search for.  Value has to be a positive integer.  A
 value of ‘nil’ means that there is no limit, i.e. search for all
 solutions.  Default is one.
Keyword argument IF-DOES-NOT-EXIST specifies the action to be taken
 if no solution can be found.  A value of ‘:error’ means to signal
 an error.  A value of ‘nil’ means to return ‘nil’ to indicate
 failure.  Default is ‘:error’.
If keyword argument RAW is true, the row indices in a solution are
 in the order as they have been found.  Otherwise, they are sorted
 in ascending order.  Default is false.

Return value is the list of found solutions.  Each solution is a
list of row indices of the incident matrix.

See also ‘make-matrix’.

Exceptional Situations:

   * Signals an error of type ‘arithmetic-error’ if no solution
     can be found and the argument IF-DOES-NOT-EXIST is ‘:error’.

Examples:

;; Knuth's example from the “Dancing Links” paper.
(let ((a (matrix-from-array #2A((0 0 1 0 1 1 0)
                                (1 0 0 1 0 0 1)
                                (0 1 1 0 0 1 0)
                                (1 0 0 1 0 0 0)
                                (0 1 0 0 0 0 1)
                                (0 0 0 1 1 0 1)))))
  (setf (column-names a) '(A B C D E F G))
  (let ((s (first (solve a))))
    (format t \"Found a solution containing~%\")
    (dolist (i s)
      (format t \" ~:R row with columns ~S~%\"
              (1+ i) (map-matrix-row #'column-name a i)))
    s))
 ⇒ (0 3 4)
;; And the terminal output is:
Found a solution containing
 first row with columns (C E F)
 fourth row with columns (A D)
 fifth row with columns (B G)"
  (check-type matrix root)
  (check-type maximum-number-of-solutions (or alexandria:positive-fixnum null))
  (check-type if-does-not-exist (member :error nil))
  (let ((*solution* (make-solution))
        (*solutions* ())
        (*solution-count* 0)
        (*solution-count-limit* maximum-number-of-solutions)
        (*search-tree* ())
        (*search-tree-p* nil))
    (search matrix 0)
    ;; Process the solutions.
    (when (and (null *solutions*) (eq if-does-not-exist :error))
      (error 'arithmetic-error :operation 'solve :operands matrix))
    (when (> *solution-count* 1)
      (setf *solutions* (nreverse *solutions*)))
    (when (and *solution-count-limit* (> *solution-count* *solution-count-limit*))
      (setf *solutions* (nbutlast *solutions* (- *solution-count* *solution-count-limit*))))
    (when (not raw)
      (iter (for tail :on *solutions*)
            (for s = (car tail))
            (setf s (sort s #'<))
            (setf (car tail) s)))
    ;; Return values.
    (values *solutions* *search-tree*)))

;;; dlx.lisp ends here
