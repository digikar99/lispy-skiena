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
```

# 6.1 Minimum Spanning Trees

\- A tree connecting all vertices of a graph G(V, E) with the sum of edges as
small as possible.

For unweighted graphs, either a BFS or a DFS can be used to find a MST.

Prim, implemented below, takes O(n²). (Why?) In fact,
depending on the representation details, Prim's algorithm has [several time
complexities](https://en.wikipedia.org/wiki/Prim%27s_algorithm#Time_complexity).

```lisp
(defun prim (g start)
  (with-slots (nvertices edges) g
    (let ((intree (make-array (1+ +maxv+) :initial-element nil))
          (distance (make-array (1+ +maxv+) :initial-element most-positive-fixnum))
          (parent (make-array (1+ +maxv+) :initial-element -1)))
      (flet ((tree-vertex-p (v) [intree v]))
        (iter (initially (setf [distance start] 0))
              (for v initially start
                   then (iter (for i from 1 to nvertices)   ; improvisation is 
                              (when (not (tree-vertex-p i)) ; perhaps here
                                (finding i minimizing [distance i]))))
              (while (not (tree-vertex-p v)))
              (setf [intree v] t)
              (iter (for p initially [edges v]
                         then (edgenode-next p))
                    (while p)
                    (with-slots (weight y) p
                      (when (and (not (tree-vertex-p y))
                                 (> [distance y] weight))
                        (setf [distance y] weight)
                        (setf [parent y] v))))
              (finally (return parent)))))))
```

By minimizing the time to find minimum-cost-edge on each iteration, using
priority queues, it is possible to implement Prim in O(m+nlgn) time. (See comment.)

With union-find data structures, see how Kruskal's algorithm can be implemented
in O(m lg m) time, for sparse graphs.

```lisp
(defconstant +set-size+ 1000)
(defstruct set-union
  (parent (make-array (1+ +set-size+) :initial-contents (iota +set-size+)))
  (size (make-array (1+ +set-size+) :initial-element 1))
  n)

;;; Both find and union-sets take (lg n) time. (Why?)

(defun set-union-find (s x)
  (declare (type set-union s))
  (with-slots (parent) s
    (if (= x [parent x])
        x
        (find s [parent x]))))

(defun set-union-union-sets (s s1 s2)
  (declare (type set-union s))
  (let ((r1 (set-union-find s s1))
        (r2 (set-union-find s s2)))
    (with-slots (size parent) s
      (cond ((= r1 r2) nil)
            ;; merging smaller into the smaller increases height the least
            ((>= [size r1] [size r2])
             (incf [size r1] [size r2])
             (setf [parent r2] r1))
            (t
             (incf [size r2] [size r1])
             (setf [parent r1] r2))))))

(defun same-component-p (s s1 s2) (= (set-union-find s s1) (set-union-find s s2)))

(defstruct edge-pair x y weight)
(defun initialize-edge-array (g)
  (with-slots (nedges nvertices edges) g
    (let ((edge-array (make-array nedges :initial-element nil)))
      (iter outer
            (generate i from 0)
            (for v below nvertices)
            (iter (for e initially [edges v]
                       then (edgenode-next e))
                  (while e)
                  (with-slots (weight y) e
                    (setf [edge-array (in outer (next i))]
                          (make-edge-pair :x v :y y :weight weight))))
            (finally (return-from outer edge-array))))))

(defun kruskal (g)
  (with-slots (nvertices nedges) g
    (let ((s (make-set-union :n (graph-nvertices g)))
          (e (sort (initialize-edge-array g) #'edgenode-weight)))
      (iter (for i below nedges) ; m
            (with-slots (x y) [e i]
              (unless (same-component-p s x y) ; lg m
                (format t "edge (~D, ~D) in MST~%" x y)
                (set-union-union-sets s x y)))))))
```

- Akin to minimum spanning trees, one can have maximum spanning trees,
minimum product spanning trees, minimum bottleneck spanning trees - all
doable using BFS/DFS.
- MST is unique only when all edge weights are distinct.
- Steiner tree: see section 16.10; not solvable using simple DFS/BFS
- Low-degree Spanning Tree: see section 16.5; not solvable using simple DFS/BFS

# 6.2 War Story: Nothing but Nets

Minimum spanning trees can be used for clustering. Connectivity of larger networks
can be ensured by ensuring the connectivity of smaller networks, and ensuring
each of these small networks (clusters) are connected.

# 6.3 Shortest Paths

BFS is insufficient to find shortest paths in weighted graphs. Dijkstra's algorithm
finds shortest path from `start` to all the other vertices. However, the algorithm
assumes non-negative weights.

## 6.3.1 Dijkstra's Algorithm

\- Single source shortest path

```lisp
(defun dijkstra (g start) ; name changed from prim to dijkstra
  (with-slots (nvertices edges) g
    (let ((intree (make-array (1+ +maxv+) :initial-element nil))
          (distance (make-array (1+ +maxv+) :initial-element most-positive-fixnum))
          (parent (make-array (1+ +maxv+) :initial-element -1)))
      (flet ((tree-vertex-p (v) [intree v]))
        (iter (initially (setf [distance start] 0))
              (for v initially start
                   then (iter (for i from 1 to nvertices)
                              (when (not (tree-vertex-p i))
                                (finding i minimizing [distance i]))))
              (while (not (tree-vertex-p v)))
              (setf [intree v] t)
              (iter (for p initially [edges v]
                         then (edgenode-next p))
                    (while p)
                    (with-slots (weight y) p
                      (when (> [distance y]                         ; changed
                               (+ [distance v] weight))             ; changed
                        (setf [distance y] (+ [distance v] weight)) ; changed
                        (setf [parent y] v))))
              (finally (return parent)))))))
```

## 6.3.2 All-Pairs Shortest Path

While asymptotically not better than n calls to O(n²) Djikstra's algorithm, Floyd's
algorithm can perform better due to "tight" looping:

```lisp
(defstruct adjacency-matrix
  (weight (make-array (list (1+ +maxv+) (1+ +maxv+))
                      :initial-element most-positive-fixnum))
  nvertices)

(defun floyd (g)
  (with-slots (nvertices weight) g
    (iter (for k from 1 to nvertices)
          (iter (for i from 1 to nvertices)
                (iter (for j from 1 to nvertices)
                      (setf [weight i j]
                            (min [weight i j]
                                 (+ [weight i k] [weight k j]))))))))
```

However, recovering the path entails maintaining a parent matrix containing the
"last intermediate vertex" used for each vertex pair.

# 6.4 War Story: Dialing for Documents

Basically, the [Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm).

# 6.5 Network Flow Problem

-   Maximum amount of flow possible from vertices s to t, given edge
capacities. This equals the weight of the minimum s-t cut.
-   Can be used to find largest bipartite matching, as well as general
edge and vertex connectivity problems.

```lisp
;;;; EDMONDS-KARP ALGORITHM

(defstruct edge v capacity flow residual next)

(defun find-edge (graph x y)
  (iter (for e initially [(graph-edges graph) x]
             then (edge-next e))
        (while e)
        (until (= y (edge-v e)))
        (finally (return e))))

;;; used by breadth-first-search
;;; - we are to only consider edges with positive residual capacity
(defun valid-edge (e) (positivep (edge-residual e)))

(defun augment-path (graph start end parent volume)
  (when (not (= start end))
    (let ((e (find-edge graph [parent end] end)))
      (incf (edge-flow e) volume)
      (decf (edge-residual e) volume))
    (let ((e (find-edge graph end [parent end])))
      (incf (edge-residual e) volume))
    (augment-path graph start [parent end] parent volume)))

;;; Traffic is limited by the most congested point
(defun path-volume (graph start end parent)
  (if (= [parent end] -1)
      0
      (let ((e (find-edge graph [parent end] end)))
        (if (= start [parent end])
            (edge-residual e)
            (min (edge-residual e)
                 (path-volume graph start [parent end] parent))))))

(defun net-flow (graph source sink)
  (iter (when (first-iteration-p)
          (add-residual-edges graph))
        ;; The other actions are found to be common :/
        (unless (first-iteration-p)          
          (augment-path graph source sink parent volume))
        ;; search for a path to the sink from the source
        ;; these paths would be stored in *parent*
        (with parent = (bfs graph source)) ; Yes! it returns the parent!
        ;; In addition to returning parent, bfs - as we designed - takes care
        ;; of initialization the various arrays discovered and processed locally,
        ;; and, except for parent, these aren't used here
        (for volume = (path-volume graph source sink parent))
        ;; if no such augmenting path exists, the volume would be 0
        (while (positivep volume))))

```

# 6.6 Design Graphs, Not Algorithms

-   I'm looking for an algorithm to design natural routes for video-game
characters to follow through an obstacle-filled room. How should I
do it?

-   A DNA sequencing project generates experimental data consisting of
small fragments. For each given fragment f, we know certain other
fragments are forced to lie to the left of f, and certain other
fragments are forced to be to the right of f. How can we find a
consistent ordering of the fragments from left to right that
satisfies all the constraints?

-   In my graphics work I need to solve the following problem. Given an
arbitrary set of rectangles in the plane, how can I distribute them
into a minimum number of buckets such that no subset of rectangles
in any given bucket intersects another? In other words, there can
not be any overlapping area between two rectangles in the same
bucket.

-   In porting code from UNIX to DOS, I have to shorten several hundred
file names down to at most 8 characters each. I can’t just use the
first eight characters from each name, because “filename1” and
“filename2” would be assigned the exact same name. How can I
meaningfully shorten the names while ensuring that they do not
collide?

-   We need a way to separate the lines of text in the optical
character-recognition system that we are building. Although there is
some white space between the lines, problems like noise and the tilt
of the page makes it hard to find. How can we do line segmentation?


