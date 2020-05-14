A graph G consists of a set of vertices V together with a set of vertex pairs (edges) E.

# 5.1 Flavours of Graphs

Undirected vs Directed, Weighted vs Unweighted, Simple vs Non-simple, Sparse vs Dense, Cyclic
vs Acyclic, Embedded vs Topological, Implicit vs Explicit, Labeled vs Unlabeled.

See [this](https://stackoverflow.com/questions/10010213/graph-what-are-the-differences-between-embedded-and-topological-in-graph) for an elaboration of embedded vs topological graphs.

# 5.2 Data Structures for Graphs

Adjacency Matrix and Adjacency Lists

```lisp
(defconstant +maxv+ 1000)

(defstruct edgenode y weight next)

(defstruct (graph
             (:constructor initialize-graph (directed)))
  (edges (make-array (1+ +maxv+) :initial-element nil))
  (degree (make-array (1+ +maxv+) :initial-element 0))
  (nvertices 0)
  (nedges 0)
  directed)

(declaim (ftype (function (graph boolean)) read-graph))
(defun read-graph (g directed)
  (let ((g (initialize-graph directed))
        nedges)
    (setf (graph-nvertices g) (read)
          nedges (read))
    (iter (for i from 1 to nedges)
          (insert-edge g (read) (read) directed))))

(declaim (ftype (function (graph integer integer boolean)) insert-edge))
(defun insert-edge (g x y directed)
  "Insert edge (x,y) into graph g."
  (let ((p (make-edgenode :y y
                          :weight nil
                          :next (aref (graph-edges g) x))))
    (with-slots (edges nedges degree) g
      (setf [edges x] p)
      (incf [degree x])
      (if (null directed)
          (insert-edge g y x t)
          (incf nedges)))))
```

# 5.3 War Story: I was a Victim of Moore's Law

If the implementation complexity isn't substantially greater, go with the asymptotically
better algorithm.

# 5.4 Was Story: Getting the Graph

Take care to initialize data structures efficiently as well!

# 5.5 Traversing a Graph

For efficiency, avoid already visited nodes. For correctness, travel systematically.

A vertex may be *undiscovered*, *discovered*, or *processed* in that temporal order. Thus, we can
ignore processed vertices, as well as vertices that are already discovered (but not yet processed).
The discovered but not processed vertices need to be kept track of.


# 5.6 Breadth-First Search

BFS runs in O(n+m) time: see how.

```lisp

(defparameter *discovered* nil)
(defparameter *processed* nil)
(defparameter *parent* nil)

(defun bfs (graph start &key process-vertex-early process-vertex-late process-edge)
  (let ((queue ())
        (discovered (or *discovered*
                        (make-array (graph-nvertices graph) :initial-element nil)))
        (processed (or *processed*
                       (make-array (graph-nvertices graph) :initial-element nil)))
        (parent (or *parent*
                    (make-array (graph-nvertices graph) :initial-element nil))))
    (iter (initially (setf [discovered start] t)
                     (enqueue queue start))
          (while queue)
          (let ((v (dequeue queue)))
            (iter (initially (when process-vertex-early
                               (funcall process-vertex-early v))
                             (setf [processed v] t))
                  (for p
                       initially [(graph-edges graph) v]
                       then (edgenode-next p))
                  (with y = (edgenode-y p))
                  (when (and process-edge
                             (or (not [processed y])
                                 (graph-directed graph)))
                    (funcall process-edge v y))
                  (when (not [discovered y])
                    (enqueue queue y)
                    (setf [discovered y] t)
                    (setf [parent y] v))
                  (finally (when process-vertex-late
                             (funcall process-vertex-late v)))))
          (finally (return parent)))))
```

Note that vertices are discovered in order of increasing distance from the parent. (Why?)
Thus, the path from root to each node, using parent, is shortest for each node.

```lisp
(defun find-path (start end parents)
  (if (or (= start end)
          (= -1 end))
      (format t "~%~D" start)
      (progn
        (find-path start [parents end] parents)
        (format t " ~D" end))))
```

Note that the shortest path is from `start` to each node, and not from an arbitrary node.
Further, the path is shortest only if the graph is unweighted.

# 5.7 Applications of Breadth-First Search

```lisp
(defun connected-components (g)
  (declare (type graph g))
  (let ((*discovered* (make-array (graph-nvertices g) :initial-element nil)))
    (iter (generate i below (length *discovered*))
          (loop while [*discovered* (next i)])
          (generate c from 0)
          (format t "Component ~D" (next c))
          (bfs g (next i) :process-vertex-early λ(format t " ~D" -))
          (terpri))))

(defun two-color (g)
  (declare (type graph g))
  (let ((*color* (make-array (graph-nvertices g) :initial-element 'uncolored))
        (*discovered* (make-array (graph-nvertices g) :initial-element nil)))
    (declare (special *color*))
    (flet ((complement-color (color)
             (case color
               (white 'black)
               (black 'white)
               (t 'uncolored))))
      (iter (generate i below (length *discovered*))
            (loop while [*discovered* (next i)])
            (bfs g i :process-edge (lambda (x y)
                                     (if (eq [*color* x] [*color* y])
                                         (error "Not bipartite due to (~D, ~D)" x y))
                                     (setf [*color* y] (complement-color [*color* x]))))
            (terpri)))))
```

# 5.8 Depth-First-Search

... on undirected graphs. See 5.10 for directed graphs.

For DFS-based algorithms, note exactly when to process the edges and vertices.

```lisp
(defparameter *time* 0)
(defvar *process-vertex-early* nil)
(defvar *process-vertex-late* nil)
(defvar *process-edge* nil)
(defvar *finished* nil)
(defvar *entry-time* nil)
(defvar *exit-time* nil)
(defun dfs (g s &key process-vertex-early process-vertex-late process-edge)
  (let ((*discovered* (or *discovered*
                          (make-array (graph-nvertices g) :initial-element nil)))
        (*processed* (or *processed*
                         (make-array (graph-nvertices g) :initial-element nil)))
        (*parent* (or *parent*
                      (make-array (graph-nvertices g) :initial-element nil)))
        (*time* 0)
        (*entry-time* (or *entry-time*
                          (make-array (graph-nvertices g) :initial-element 0)))
        (*exit-time* (or *exit-time*
                         (make-array (graph-nvertices g) :initial-element 0)))
        (*process-vertex-late* process-vertex-late)
        (*process-vertex-early* process-vertex-early)
        (*process-edge* process-edge)
        (*finished* nil))
    (%dfs g s)))

(defun edge-classification (x y)
  ;; undiscovered -> discovered -> processed
  ;; TODO: verify! This function is unused here; another version for directed
  ;; graphs is defined below.
  (cond ((not [*discovered* y]) 'tree)
        ((not [*processed* y]) 'back)
        (t 'tree)))

(defun %dfs (g v)
  (unless *finished*
    ;; Who sets *finished* to T? Or perhaps, it is used for premature termination
    ;; See next section.
    (iter (initially (setf [*discovered* v] t)
                     (incf *time*)
                     (setf [*entry-time* v] *time*)
                     (when *process-vertex-early*
                       (funcall *process-vertex-early* v)))
          ;; compilation fails on asserted type of p - WHY??? - TODO
          (iter (for p initially [(graph-edges g) v]
                     then (edgenode-next p))
                (with y = (edgenode-y p))
                (cond ((not [*discovered* y])
                       (when *process-edge*
                         (funcall *process-edge* v y))
                       (dfs g y))
                      ((or (not [*processed* y])
                           (graph-directed g))
                       (when *process-edge*
                         (funcall *process-edge* v y))))
                (when *finished* (return-from %dfs nil)))
          (finally (when *process-vertex-late*
                     (funcall *process-vertex-late* v))
                   (incf *time*)
                   (setf [*exit-time* v] *time*)
                   (setf [*processed* v] t)))))
```

# 5.9 Applications of Depth-First Search

## 5.9.1 Finding Cycles

```lisp
;;; process-edge for finding cycles - we detect back-edges
;;; No cycles if only tree-edges and no back-edges.
(defun process-edge (x y)
  (when (/= [*parent* x] y)
    (format t "Cycle from ~D to ~D: " y x)
    (find-path y x)
    (terpri) (terpri)
    (setq *finished* t)))
```

## 5.9.2 Articulation Vertices

Articulation vertex - a single vertex whose deletion disconnects a connected
component of the graph.

```lisp
(defparameter *reachable-ancestor* (make-array (1+ +maxv+) :initial-element 0))
(defparameter *tree-out-degree* (make-array (1+ +maxv+) :initial-element 0))
(defun process-vertex-early (v)
  (setf [*reachable-ancestor* v] v))
(defun process-edge (x y)
  (let ((class (edge-classification x y)))
    (ecase class
      (tree (incf [*tree-out-degree* x]))
      (back (when (and (/= [*parent* x] y)
                       (< [*entry-time* y]
                          [*reachable-ancestor* [*entry-time* x]]))
              (setf [*reachable-ancestor* x] y))))))
(defun process-vertex-late (v)
  (flet ((rootp (v) (< [*parent* v] 1))
         (leafp (v) (zerop [*tree-out-degree* v])))
    (cond ((rootp v)
           (when (> [*tree-out-degree* v] 1)
             (format t "root articulation vertex: ~D~%" v)))
          ((and (= [*reachable-ancestor* v]
                   [*parent* v])
                (not (rootp [*parent* v])))
           (format t "parent articulation vertex: ~D~%" [*parent* v]))
          ((= v [*reachable-ancestor* v])
           (format t "bridge articulation vertex: ~D~%" [*parent* v])
           (unless (leafp v)
             (format t "bridge articulation vertex: ~D~%" v))))
    (when (< [*entry-time* [*reachable-ancestor* v]]
             [*entry-time* [*reachable-ancestor* [*parent* v]]])
      (setf [*reachable-ancestor* [*parent* v]]
            [*reachable-ancestor* v]))))
```

# 5.10 Depth-First Search on Directed Graphs

In undirected graphs, we have tree edges and back edges; in directed, we have
the following four:

```lisp
(defun edge-classification (x y)
  (cond ((= x [*parent* y]) 'tree)
        ((and [*discovered* y]
              (not [*processed* y]))
         'back)
        ((and [*processed* y]
              (> [*entry-time* y]
                 [*entry-time* x]))
         'forward)
        ((and [*processed* y]
              (< [*entry-time* y]
                 [*entry-time* x]))
         'cross)
        (t (error "Unclassified edge (~D, ~D)" x y))))
```

## 5.10.1 Topological Sort

Note that the top vertex on the stack (\*sorted\*) has no incoming edges from any vertex
on the stack; thus, repeatedly popping them off yields a topological ordering.

```lisp
(defvar *sorted* nil)

(defun topological-sort (g)
  (declare (type graph g))
  (let ((*sorted* nil)
        (*discovered* (make-array (graph-nvertices g) :initial-element nil)))
    (iter (for i from 1 to (graph-nvertices g))
          (when (not [*discovered* nil])
            (dfs g i :process-vertex-late λ(push - *sorted*)
                 :process-edge λ(when (eq 'back (edge-classification - --))
                                  (error "Directed cycle found due to (~D, ~D); not a DAG!"
                                         - --))))
          (finally (return *sorted*)))))
```

## 5.10.2 Strongly Connected Components

A directed path exists between all pairs of vertices within a given strongly connected
component. (This differs from a [Clique](https://en.wikipedia.org/wiki/Clique_(graph_theory))
in that a clique is a subset of vertices in an undirected graph with every two vertices
being distinct.)

A simple way to determine whether a graph is strongly connected is simply to 
1. Do a traversal on the given graph G(V, E)
2. Do a traversal on the graph G'(V, E'), with E' consisting of reversed edges of E

1 ensures every other vertex can be reached from starting vertex `v`. If 2 is possible,
because a path from `v` to any other vertex in 2 implies a path from this any other vertex
to `v`, plausibility of 1 and 2 implies a strongly connected graph.

```lisp
(defparameter *low* nil)
(defparameter *scc* nil)
(defparameter *components-found* 0)

(defun strong-components (g)
  (let ((n (graph-nvertices g)))
    (let ((*low* (make-array n :initial-contents
                             (loop for i from 1 to n collect n)))
          ;; *low* is the oldest vertex known to be in the same
          ;; strongly connected component - an ancestor or a distant
          ;; cousin (not yet part of another scc) due to cross edges.
          (*scc* (make-array n :initial-element -1))
          (*components-found* 0)
          (active ())
          (*discovered* (make-array n :initial-element nil)))
      (flet ((pop-component (v)
               (incf *components-found*)
               (setf [*scc* v] *components-found*)
               (iter (with ti = (pop active))
                     (while (/= ti v))
                     (setf [*scc* ti] *components-found*))))
        (iter (for i from 1 to n)
              (unless [*discovered* i]
                (dfs g i
                     :process-vertex-early λ(push - active)
                     :process-edge (lambda (x y)
                                     (case (edge-classification x y)
                                       (back (when (< [*entry-time* y]
                                                      [*entry-time* [*low* x]])
                                               (setf [*low* x] y)))
                                       (cross (when (and (= -1 [*scc* y])
                                                         (< [*entry-time* y]
                                                            [*entry-time* [*low* x]]))
                                                (setf [*low* x] y)))))
                     :process-vertex-late (lambda (v)
                                            (when (= v [*low* v])
                                              (pop-component v))))))))))
```
