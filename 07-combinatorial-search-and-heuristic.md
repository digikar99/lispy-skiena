7.1 Backtracking
================

- Using a systematic generation order to try all possible solutions.
- A DFS is preferred over BFS (Why?)

```lisp
(defun when-funcall (function &rest arguments)
  (when function (apply function arguments)))

(defconstant +nmax+ 1000)

(defparameter *finished* nil)
;;; Users are required to implement the following functions: 
(defparameter *solutionp* nil
  "Supposed to be a function that has (config k input) as its arguments. Should return T
if the first K elements of CONFIG form a complete solution for INPUT.") 
(defparameter *construct-candidates* nil
  "Supposed to be a function that has (config k input) as its lambda list. Should return
a list of possible candidates for the kth position of CONFIG.")
(defparameter *process-solution* nil
  "A function that has (config k input) as its lambda list. This function
is called once a complete solution is constructed.")
(defparameter *make-move* nil
  "A function takes takes (config k input) as its input. Useful if one wants to 
allocate / modify a data structure. Called before the next call to backtrack.")
(defparameter *unmake-move* nil
  "A function takes takes (config k input) as its input. Useful if one wants to 
allocate / modify a data structure. Called after a call to backtrack.")

(defun backtrack (config k input)
  ;; TODO: Verify contrast with DFS:
  ;; In DFS, already-visited nodes had to be kept track of by maintaining the
  ;; discovered and processed arrays; here, they need to be kept track of by the
  ;; construct-candidates function.
  (if (funcall *solutionp* config k input)        
      (when-funcall *process-solution* config k input) 
      (let* ((k (1+ k))               ; extending the partial solution
             (candidates (funcall *construct-candidates* config k input)))
        (iter (for c in candidates)
              (setf [config k] c)
              (when-funcall *make-move* config k input)
              (backtrack config k input)
              (when-funcall *unmake-move* config k input)
              (until *finished*)))))
```

## 7.1.1 Constructing all subsets

```lisp
(defun generate-subsets (n)
  (let ((config (make-array +nmax+))
        (*solutionp* λ3(= -- ---))
        (*construct-candidates* λ3(list t nil))
        (*process-solution* (lambda (set k n)
                              (declare (ignore n))
                              (iter (initially (write-char #\{))
                                    (for i from 1 to k)
                                    (if [set i] (format t " ~D" i))
                                    (finally (write-line " }"))))))
    (backtrack config 0 n)))
```

## 7.1.2 Constructing all Permutations

```lisp
(defun generate-permutations (n)
  (let ((config (make-array +nmax+))

        (*solutionp* λ3(= -- ---))

        (*construct-candidates*
         (lambda (permutation k n)
           (let ((in-perm-p (let ((in-perm-p (make-array *nmax* :initial-element nil)))
                              (iter (for i below k) (setf [in-perm-p [permutation i]] t))
                              in-perm-p)))
             (find-if-not λ[in-perm-p i] (iota n :start 1)))))
        
        (*process-solution* (lambda (permutation k n)
                              (declare (ignore n))
                              (iter (for i from 1 to k)
                                    (format t " ~D" [permutation i])
                                    (finally (terpri))))))
    (backtrack config 0 n)))
```

## 7.1.3 Constructing All Paths in a Graph

```lisp
(defun construct-candidates (path k n)
  (let ((in-path-p (let ((in-path-p (make-array +nmax+ :initial-element nil)))
                     (iter (for i from 1 below k) ; looping from 1, because, graphs
                           (setf [in-path-p [path i]] t)
                           (finally (return in-path-p))))))
    (if (= k 1)
        '(1)
        (iter (for p initially [(graph-edges *graph*) [path (1- k)]]
                   then (edgenode-next p))
              (while p)
              (unless [in-path-p (edgenode-y p)]
                (collect (edgenode-y p)))))))

(defun solutionp (config k destination) (= [config k] destination))

(defun process-solution (config k n) (incf *solution-count*))

(defparameter *graph* nil)
(defparameter *solution-count* nil)

(defun count-paths (g destination)
  ;; start is 1 is implicit?
  (let ((config (make-array +nmax+))
        (*solutionp* #'solutionp)
        (*process-solution* #'*process-solution*)
        (*construct-candidates* #'construct-candidates)
        (*graph* g)
        (*solution-count* 0))
    (backtrack config 0 destination)))
```

# 7.2 Search Pruning

Prune

-   if it can be found that a certain partial solution cannot be
developed into a solution.
-   if newer solutions would be symmetric to previous ones

# 7.3 Sudoku

```lisp
(defconstant +dimension+ 9)
(defconstant +ncells+ (* +dimension+ +dimension+))
(defparameter *finished* nil)

(defstruct point x y)
(defstruct boardtype
  (m (make-array (list (1+ +dimension+) (1+ +dimension+))))
  freecount
  (move (make-array (list (1+ +ncells+)))))

(defun construct-candidates (config k board)
  (destructuring-bind (x y) (next-square board)
    (setf [(boardtype-move board) k]
          (make-point :x x :y y))
    (if (and (< x 0) (< y 0))
        nil
        (let ((possible (possible-values x y board)))
          (iter (for i to +dimension+)
                (when [possible i] (collect i)))))))

(defun solutionp (config k board) (zerop (boardtype-freecount board)))

(defun process-solution (config k board)
  (print-board board)
  (setf *finished* t))

(defun make-move (config k board)
  (fill-square [(boardtype-move board) k] board))
(defun unmake-move (config k board)
  (free-square [(boardtype-move board) k] board))
;;; fill-square and free-square should be handling the conversion of move array
;;; to m array

```
Non-trivial functions that remain are next-square and possible-values:

- reasonable ways for next-square include arbitrary square selection, and
most-constrained-square selection
- possible values can either use a simple local-count in that it allows
all numbers not in the given row, column or sector; or it can "look ahead"
and prematurely determine if there some *other* cells would have no solutions
due to some of the current choices

# 7.4 War Story: Covering Chessboards

- Problem: Covering the chessboard using the 8 main pieces
- Pruning helps radically! (Exercise: guess the order in which pieces might be
inserted to aid pruning.)
- Further pruning using "subset" elmination
- "Strong" and "weak" attacks - latter avoids blocking effect

# 7.5 Heuristic Search Methods

## 7.5.1 Random Sampling

```lisp
(defun random-sampling (tsp-instance nsamples
                        &key initialize-solution solution-cost random-solution)
  "Returns the BEST-SOLUTION (as defined in the book)."
  (iter (for i from 0 to nsamples)
        (for s initially (funcall initialize-solution (tsp-instance-n tsp-instance))
             then (funcall random-solution))
        (finding s minimizing (funcall solution-cost s tsp-instance))))
```

Can be useful:

-   when there are a high proportion of acceptable solutions
-   when there is no coherence in the solution space

### Stop and Think

Propose an efficient and *unbiased* (uniformly) method to generate
random pairs of vertices from the C(n, 2) unordered pairs on {1, ...,
n}.

**Local Search**

```lisp
(defmacro-clause (for var initially value)
  `(for ,var initially ,value then ,var))

(defun hill-climbing (tsp-instance &key transition)
  (with-slots (n) tsp-instance
    (iter (for solution initially (random-solution (initialize-solution n)))
          (for cost initially (solution-cost solution))
          (with stuck = t) ; still simpler method to use stuck?
          (iter (for i from 1 to n)
                (iter (for j from (1+ i) to n)
                      (for (values new-solution delta)
                           = (funcall transition solution tsp-instance i j))
                      (when (< delta 0)
                        (setq stuck nil)
                        (incf cost delta)
                        (setq solution new-solution))))
          (until stuck))))
```

Use local search:

- when there is great coherence in the solution space
- when the cost of incremental evaluation is much cheaper than global
evaluation
- when greedy search is good enough

**Simulated Annealing**

```lisp
(defun anneal (tsp-instance)
  (with-slots (n) tsp-instance
    (iter (for solution initially (initialize-solution n))
          (for current-cost initially (solution-cost solution tsp-instance))
          (for initial-cost initially current-cost)
          (for temperature initially (* *initial-temperature* cooling-factor)
               then (if (< current-cost initial-cost)
                        (* *initial-temperature* cooling-factor)
                        temperature))
          (for i from 0 to *cooling-steps*)
          (iter (initially (setq initial-cost current-cost))
                (repeat *steps-per-temp*)
                (for (values new-soluton delta)
                     = (transition solution
                                   tsp-instance
                                   (random-int 1 n)
                                   (random-int 1 n)))
                (when (or (< delta 0)     ; win
                          (> (exp (- (/ delta
                                        current-cost
                                        k
                                        temperature)))
                             (random-float 0 1))) ; loss
                  (setq current-cost (+ delta current-cost)))))))
```

Useful for efficiently obtaining good, but not optimal solutions to
combinatorial search problems.

**Applications of Simulated Annealing**

-   Maximum Cut
-   Independent Set
-   Circuit Board Placement

# 7.6 War Story: Only it is Not a Radio

- Hypergraphs
- Cost functions should reflect partial progress


# 7.7 War Story: Annealing Arrays

-   Input: a set S of n strings to fabricate in an mxm array; output the
smallest set of prefixes and suffixes to realize all the given strings


# 7.8 Other Heuristic Search Methods

- Genetic algorithms, neural networks, and ant colony optimization
- Genetic algorithms maintain a population of solutions, and "breed" them
to produce (hopefully) better solution; this modeling adds a layer
of complexity; and this doesn't usually use of problem specific structures.

# 7.9 Parallel Algorithms

- Using better sequential algorithms can be more beneficial than parallelizing
the worse ones.
- Parallel algorithms can be hard to debug, and it's recommended to only employ
"independent" execution.

# 7.10 War Story: Going Nowhere Fast

- Load-balance properly! Ensure all parallel jobs roughly take equal time.
