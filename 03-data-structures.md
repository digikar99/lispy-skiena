# 3.1 Contiguous vs Linked Data Structures

## 3.1.1 Arrays

- **Advantages**: Constant time access given the index, space efficiency, memory locality
- **Disadvantages**: Size is \~mostly\~ unadjustable; even though dynamic arrays exist. 
What dynamic arrays lose out on is the guarantee of constant time access (or, constant time
writing?) in the worst case.

## 3.1.2 Pointers and Linked Structures

- **Advantages**: Overflow not usually possible, simpler insertions and deletions,
moving pointers can be easier than the items themselves
- **Disadvantages**: Require extra space for pointers, inefficient random access,
harder to derive cache benefits due to memory non-locality

```lisp
(defun search-list (l x)
  (cond ((null l) nil)
        ((eq x (car l)) l) ; eq or equal or equalp
        (t (search-list (cdr l) x))))

(defun insert-list (l x)
  (cons x l))

(defun predecessor-list (l x)
  (cond ((or (null l) (null (cdr l)))
         (signal "Predecessor sought on null list")
         nil)
        ((eq (cadr l) x) l)
        (t (predecessor-list (cdr l) x))))

(defun delete-list (l x)
  ;; Non-destructive
  (cond ((null l) l)
        ((eq x (car l))
         (cdr l))
        (t (cons (car l)
                 (delete-list (cdr l) x)))))

(defmacro delete-list (l x)
  ;; Incomplete destructive
  (with-gensyms (l-var x-var)
    `(let ((,l-var ,l)
           (,x-var ,x))
       (when-let ((p (search-list ,l-var ,x-var)))
         (let ((pred (predecessor-list ,l-var ,x-var)))
           (if pred ; we know x exists in l: if pred is null, x is the first elt!
               (setf (cdr pred) (cddr pred))
               (setq ,l (cdr p)))
           ,l)))))
```

# 3.2 Stacks and Queues

- **Stacks**: LIFO; `push`, `pop`; use when retrieval order is irrelevant
- **Queues**: FIFO; `enqueue`, `dequeue`; order is relevant; "fairest"

Average waiting time is same in either case though.

# 3.3 Dictionaries

Search, insert, delete. Depending on the implementation, efficient min, max predecessor, successor are also possible.

Exercise: compare each of the seven operations in the case when dictionary is implemented
as a sorted array, and as an unsorted array.

.

.

.

(Hope you did the exercise, 'cause here are the answers!)

```text
Operation     | Unsorted | Sorted   |
search        | O(n)     | O(logn)  |
insert        | O(1)     | O(n)     |
delete        | O(1)     | O(n)     |
successor     | O(n)     | O(1)     |
predecessor   | O(n)     | O(1)     |
minimum       | O(n)     | O(1)     |
maximum       | O(n)     | O(1)     |
```

How about singly-linked unsorted, doubly-linked unsorted, singly-linked sorted
and double-linked sorted lists?

.

.

.

Answers!

```text
Operation     | Singly un- | Doubly un- | Singly sorted | Doubly sorted
search        | O(n)       | O(n)       | O(n)          | O(n)
insert        | O(1)       | O(1)       | O(n)          | O(n)
delete        | O(n)       | O(1)       | O(n)          | O(1)
successor     | O(n)       | O(n)       | O(1)          | O(1)
predecessor   | O(n)       | O(n)       | O(n)          | O(1)
max           | O(n)       | O(n)       | O(1)          | O(1)
min           | O(n)       | O(n)       | O(1)          | O(1)         
```

# 3.4 Binary Search Trees

- Fast search *and* flexible update.
- Take a binary tree with n nodes, and take a set of n keys. Exactly one
labeling makes this a binary search tree.
- Search, insertion and deletion with binary search trees all take O(h) time;
in the best case, that's h = (ceiling (log n)). Even in the average case,
performance can be as good as O(log n) - this can be brought about by randomization.
- Red-black trees and splay-trees help with balancing.

```lisp
(defstruct our-tree item parent left right)

(defun search-tree (l x)
  (when l
    (with-slots (item left right) l
      (cond ((= x item) l)
            ((< x item) (search-tree left x))
            ((> x item) (search-tree right x))))))

(defun find-minimum (tree)
  (when tree
    (iter (while (our-tree-left min))
          (for min initially tree
               then (our-tree-left min))
          (finally (return min)))))

(defun traverse-tree (tree &optional process-item)
  (when tree
    (with-slots (item left right) tree
      (traverse-tree left process-item)
      (when process-item (funcall process-item item))
      (traverse-tree right process-item))))

(defun insert-tree (tree x p)
  (if (null tree)
      (make-our-tree :item x
                     :parent p)
      (with-slots (item left right) tree
        (if (< x item)
            (setf left (insert-tree left x tree))
            (setf right (insert-tree right x tree)))
        tree)))

(defun delete-tree (tree x)
  ;; WRONG!
  (if (null tree)
      nil
      (with-slots (item left right) tree
        (cond ((= x item) (setq tree nil))
              ((< x item)
               (setf left (delete-tree left x)))
              ((> x item)
               (setf right (delete-tree right x))))
        tree)))

(defparameter tree-instance
  (make-our-tree :item 5
                 :left (make-our-tree :item 3)
                 :right (make-our-tree :item 7)))
```


# 3.5 Priority Queues

Insert, find-minimum, delete-minimum - or maximums. Guess the time complexities for
these operations when priority queues are implemented as either of unsorted array, sorted
array, and balanced binary search tree.

```text
Operation       | Unsorted array | Sorted array | Balanced BST
insert          | O(1)           | O(n)         | O(log n)
find-minimum    | O(1)           | O(1)         | O(1)     <– think why?
delete-minimum  | O(n)           | O(1)         | O(log n)
```

# 3.6 War Story: Stripping Triangulations

Consider each triangle as a single point - and adjacent triangles as two connected points.
Thus, we need to feed chains of these points to the hardware engine. We need to find a "small"
number of strips that cover each triangle in the mesh (= graph).

The naive approach: with k as length of walk from "average" vertex, and n vertices, finding
one "best" strip requiers O(nk) time. For n/k strips, that's O(n²) operations.

Rewalking from each triangle after a strip deletion is wasteful: maintain a priority queue
of longest strips, with the ability to reduce priorities. An array of buckets suffices due to
hardware constraints. In addition, we store pointers
indicating where each triangle is in the queue in a dictionary. This final implementation
requires just O(nk) time.

# 3.7 Hashing and Strings

- **Hash function**: mathematical function mapping keys to integers
- **Collision Resolution**: chaining, open addressng
- **Efficient String Matching**: Rabin-Karp algorithm
- **Hashing for Duplicate Detection**: uniqueness of documents, plagiarism, bidding

# 3.8 Specialized Data Structures

- **Strings** and suffix trees/arrays
- **Geometric data structures**: collections of data points and regions; for example, polygons
as array of points. Also see kd-trees.
- **Graph data structures**: adjacency matrices or lists
- **Set data structures**: implemented using dictionaries and bit vectors. Also see union-find data structure.

# 3.9 War Story: String 'em Up

*Identify all strings of length 2k that are possible substrings of an unknown string S
given the set of all length k substrings of S.*

Consider O(n²) concatenations from n length-k substrings. For each concatenation,
we need to test whether the each of the length-k k-1 substrings existed in the set.

- A binary search tree meant O(k log n) for each concatenation
- A hash table brought it to O(k)
- Suffix trees ran out of memory
- Compressed suffix trees solved the problem

See section 12.3 for suffix and compressed suffix trees.
