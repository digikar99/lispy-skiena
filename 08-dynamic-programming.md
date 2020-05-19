
# General Guidelines

- Pay careful attention to boundary conditions and index manipulations!
- Abstract out what parameterizes the problem. See section 8.4 for an illustration.

See section 8.3 for an illustration of these:

- It can be easier to cook up DP algorithms from scratch than use an existing one.
- Formulate the answer as a recurrence relation or algorithm.
- Show that the total number of distinct parameter values taken by the recurrence
is bound by a smallish polynomial
- Specify an order of evaluation so that the required partial results are always
available.

# 8.1 Caching vs Computation

In particular study the `binomial~coefficient` function carefully,
for the rest of the chapter.

TODO: insert code here

# 8.2 Approximate String Matching

For Pattern P and Text T: (... a character in P to obtain T)

- substitute
- insert
- delete

## Edit Distance by Recursion

When each operation is defined to cost 1, what we get is known as *Edit Distance*.

```lisp
;; (declaim (ftype (function (string string integer integer)) string-compare))
;;; match gives the cost of matching two characters
;;; indel gives the cost of character deletion
(defun string-compare (s1 s2 i j &key indel match)
  (cond ((zerop i) (* j (funcall indel " ")))
        ((zerop j) (* i (funcall indel " ")))
        (s2
         (let ((match-cost (+ (string-compare s1 s2 (1- i) (1- j))
                              (funcall match [s1 i] [s2 j])))
               (insert-cost (+ (string-compare s1 s2 i (1-j))
                               (funcall indel [s2 j])))
               (delete-cost (+ (string-compare s1 s2 (1- i) j)
                               (funcall indel [s1 i]))))
           (min match-cost insert-cost delete-cost)))))
```

This algorithm is exponential \~3ⁿ and hence impractical.

## Edit Distance by Dynamic Programming

Note that there can only be `(* (length s1) (length s2))` unique recursive
calls; the others of the 3^n^ calls would have to be repeated.

```lisp
(defstruct cell cost parent)

(let ((m (make-array (list (1+ *maxlen*) (1+ *maxlen*))
                     :element-type 'cell)))
  (declaim (ftype (function (string string function function)) string-compare))
  (defun string-compare (s1 s2 row-init column-init match indel)
    "ROW-INIT and COLUMN-INIT initialize the 0th row and column respectively.
MATCH should return the cost of converting the first character into the second.
INDEL should return the cost of inserting/deleting a character."
    (iter (initially (map-iota λ(funcall row-init m -) *maxlen*)
                     (map-iota λ(funcall column-init m -) *maxlen*))
          (for i from 1 below (length s1))
          (iter (for j from 1 below (length s2))
                (setf [m i j]
                      (let ((match-cost (+ [m (1- i) (1- j)]
                                           (funcall match [s1 i] [s2 j])))
                            (insert-cost (+ [m i (1- j)]
                                            (funcall indel [s2 j])))
                            (delete-cost (+ [m (1- i) j]
                                            (funcall indel [s1 i]))))
                        (iter (for cost in (list match-cost insert-cost delete-cost))
                              (for parent in '(match insert delete))
                              (minimize cost into c)
                              (finding parent minimizing cost into p)
                              (finally (return (make-cell :cost c :parent p)))))))
          (finally (return (-<> (goal-cell s1 s2)
                             (apply #'aref m <>)
                             (cell-cost <>)))))))
```

-   Note that the first character of `m` corresponds to a blank
character.
-   Any evaluation order that computes the cells at `(1- i) (1- j)`,
`(1- i) j`, `i (1- j)` will do.

## Reconstructing the Path

```lisp
(declaim (ftype (function (string string integer integer)) reconstruct-pointer))
(defun reconstruct-path (s1 s2 i j)
  (case (cell-parent [m i j])
    (-1 nil)
    ((match)
     (reconstruct-path s1 s2 (1- i) (1- j))
     (match-out s1 s2 i j))
    (insert
     (reconstruct-path s1 s2 i (1- j))
     (insert-out s2 j))
    (delete
     (reconstruct-path s1 s2 (1- i) j)
     (delete-out s1 i))))
```

-   It is possible to reconstruct the tour without retaining last-move pointer array.

## Varieties of Edit Distance

Four categories of helper functions - suitable for the
standard-edit-distance function:

```lisp
;;; 1.  Table Initialization
(defun row-init (m i)
  (setf [m 0 i] (make-cell :cost i :parent (if (< 0 i) 'insert -1))))
(defun column-init (m i)
  (setf [m i 0] (make-cell :cost i :parent (if (< 0 i) 'delete -1))))

;;; 2. Penalty Costs
(defun match (c d) (if (char= c d) 0 1))
(defun indel (c) 1)

;;; 3. Goal Cell Identification
(defun goal-cell (s1 s2) (list (length s1) (length s2)))

;;; 4. Traceback Actions
(defun insert-out (s2 j) (write-string "I"))
(defun match-out (s1 s2 i j)
  (write-string (if (char= [s1 i] [s2 j]) "M" "S")))
(defun delete-out (s1 i) (write-string "D"))
```

Small tweaks to the above enable solving several other problems:

- **Substring Matching**: The cost of starting the match needs to be independent
of the position in the text (why?). And the goal state needs to be at the cheapest
place to match the entire pattern. So that -

```lisp
(defun row-init (m i) (setf [m 0 i] (make-cell :cost 0 :parent -1)))
(defun goal-state (s1 s2)
  (let ((i (1- (length s1))))
    (list i
          (iter (for j from 1 below (length s2))
                (finding j minimizing [m i j])))))
```

- **Longest Common Subsequence**: For example, LCS between "democrat" and "republican"
is *cca*. Thus, we want the cost of substitution to be greater than that of an insertion
plus a deletion:

```lisp
(defun match (c d) (if (char= c d) 0 +maxlen+))
```

- **Maximum Monotone Subsequence**: LCS with the second sequence being the sorted
first sequence.

# 8.3 Longest Increasing Sequence

It is possible to do this in O(n lgn) time using dictionary data structures.


# 8.4 War Story: Evolution of the Lobster

\-

# 8.5 The Partition Problem

- Input: An arrangement S of nonnegative numbers {s1, ..., sn} and an integer k.
- Output: Partition of S into at most k ranges, to minimize the sum over all the
ranges without reordering the numbers
- The below implementation runs in O(kn²).


```lisp
(defun reduce-collect (function sequence &key initial-value from-end)
  (let ((s initial-value))
    (coerce (let ((list (iter (for elt in-sequence (if from-end (reverse sequence) sequence))
                              (setq s (funcall function s elt))
                              (collect s))))
              (if from-end (nreverse list) list))
            (typecase sequence
              (vector 'vector)
              (list 'list)))))

(defun row-major-index->subscripts (array row-major-index)
  (let* ((dimensions (array-dimensions array))
         (strides (reduce-collect #'* (nconc (cdr dimensions) '(1))
                                  :initial-value 1
                                  :from-end t))
         (current-index row-major-index))
    (loop for stride in strides
       for idx = (floor current-index stride)
       do (setq current-index (rem current-index stride))
       collect idx)))

(defun array-initialize (dimensions array-initializer)
  (let ((array (make-array dimensions)))
    (loop for i below (array-total-size array)
       do (setf (row-major-aref array i)
                (if (functionp array-initializer)
                    (apply array-initializer (row-major-index->subscripts array i))
                    array-initializer)))
    array))

(defun partition (s n k)
  ;; k - number of partitions
  ;; n - number of elements (size of s)
  (let ((p (reduce-collect #'+ s :initial-value (- [s 0])))
        (d (make-array (list (1+ +maxn+) (1+ +maxn+))))     ; dividers
        (m (array-initialize (list (1+ +maxn+) (1+ +maxn+)) ; values
                             (lambda (i j) (cond ((= j 1) [p i])
                                                 ((= i 1) [s 1]))))))
    (iter (for i from 2 to n)
          (iter (for j from 2 to k)
                (dsetq ([d i j] [m i j])
                       (iter (for x from 1 below i)
                             (with cost = (max [x (1- j)]
                                               (- [p i] [p x])))
                             (finding (list x cost) minimizing cost)))))
    (reconstruct-partition s d n k)))

(defun reconstruct-partition (s d n k)
  (if (= k 1)
      (print-books s 1 n)
      (progn (reconstruct-partition s d [d n k] (1- k))
             (print-books s (1+ [d n k]) n))))

(defun print-books (s start end)
  (iter (for i from start to end)
        (format t " ~D" [s i])
        (terpri)))
```

# 8.6 Parsing Context-Free Grammars

Given a constant size CFG in Chomsky Normal Form - X->YZ or X->α - as an exercise,
determine the recurrence relation involved in parsing a string S of length n
according to the given CFG.

- Parsimonious Parserization
- Minimum Weight Triangulation is based on Parsimonious Parserization

```lisp
(defun minimum-weight-triangulation (p)
  (let ((cost (array-initialize (list n n)
                                (lambda (i j) (if (= i (1+ j)) 0 nil)))))
    (iter (for gap from 2 below n)
          (iter (for i from 1 to (- n gap))
                (with j = (+ i gap))
                (setf [cost i j]
                      (iter (for k from (1+ i) below j)
                            (minimize (+ [cost i k]
                                         [cost k j]
                                         (distance [p i] [p k])
                                         (distance [p k] [p j])))))))
    cost))
```


# 8.7 Limitations of Dynamic Programming: TSP

- Input: A weighted graph G with specified start and end vertices
- Output: Most expensive path from `start` to `end` that does not visit any
vertex more than once (= simple path)

**Correctness of DP Algorithms**

- Ensure the correctness of recurrence relations
- Let there be an evaluation order
- Obey principle of optimality - partial solutions can be extended using only
the *state* rather than the whole history of operations that led to that state

**Efficiency of DP Algorithms**

- Minimize the number of partial solutions we need to keep track of
- Minimize the time for evaluating each partial solution
- Fixing an order helps

# 8.8 War Story: What's Past is Prolog

TODO: Understand the recurrence. (Page 306.)

- Doesn't the recurrence only answer a single level of the tree?
- So, that's basically O(n²) for 1 level? And the total complexity is O(mn²)?

# 8.9 War Story: Text Compression for Bar Codes

A reasonable heuristic often works well. But with DP, you can find the global optimum.


