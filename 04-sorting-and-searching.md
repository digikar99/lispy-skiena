# 4.1 Applications of Sorting

Search preprocessing, finding closest pairs in, element uniqueness, frequency distributions,
selecting kth largest items, convex hulls.

TODO: convex hulls

Two sets can be efficiently determined to be disjoint by sorting the smaller set. (See why
this is more efficient than sorting both sets - or sorting the larger set.)

# 4.2 Pragmatics of Sorting

Order, decding on the keys, comparison functions. See [CLHS documentation for the Common
Lisp `sort` function](#http://www.lispworks.com/documentation/HyperSpec/Body/f_sort_.htm).

# 4.3 Heapsort: Fast Sorting via Data Structures

Heapsort is just selection sort using heaps as the "right" data structure:

```lisp
(defun selection-sort (a)
  (let ((sort (make-array (length a))))
    (iter (for i from below n)
          (setf [sort i] (pop-minumum a))
          (finally (return sort)))))
```

## Heaps

By using arrays for priority queues, we save space, if we ensure packing; however,
we lose out on flexibility in that we cannot relocate subtrees without moving all
of its elements.

```lisp
(defconstant +priority-queue-size+ 16)
(defclass priority-queue ()
  ((storage-array :reader priority-queue-storage-array
                  :initform (make-array (1+ +priority-queue-size+)))
   ;; Left child of k is at 2k and right child at 2k+1
   (size :reader priority-queue-size
         :initform 0)))

(defun priority-queue-parent (n) (if (= n 1) -1 (floor n 2)))
(defun priority-queue-young-child (n) (* 2 n))

(defun priority-queue-insert (priority-queue item)
  (with-slots (storage-array size) priority-queue
    (when (>= size +priority-queue-size+)
      (error "priority queue overflow while inserting ~D" item))
    (incf size)
    (setf [storage-array size] item)
    (bubble-up priority-queue size)))

(defun bubble-up (priority-queue position)
  (let ((parent (priority-queue-parent position)))
    (case parent
      (-1 nil)
      (t (with-slots (storage-array) priority-queue
           (when (> [storage-array parent]
                    [storage-array position])
             (rotatef [storage-array parent]
                      [storage-array position])
             (bubble-up priority-queue parent)))))))

(defun make-priority-queue (&optional initial-elements)
  (let ((priority-queue (make-instance 'priority-queue)))
    (when initial-elements
      (iter (for elt in-sequence initial-elements)
            (priority-queue-insert priority-queue elt)))
    priority-queue))

(defun extract-min (priority-queue)
  (with-slots (storage-array size) priority-queue
    (if (zerop size)
        (error "Empty priority queue")
        (let ((min [storage-array 1]))
          (setf [storage-array 1]
                [storage-array size])
          (decf size)
          (bubble-down priority-queue 1)
          min))))

(defun bubble-down (priority-queue position)
  (with-slots (storage-array size) priority-queue
    (let* ((c (priority-queue-young-child priority-queue))
           (min (iter (for pos in (list position c (1+ c)))
                      (when (<= pos size)
                        (finding pos minimizing [storage-array pos])))))
      (when (/= min position)
        (rotatef [storage-array position]
                 [storage-array min])
        (bubble-down priority-queue min)))))

(defun heap-sort (s)
  (let ((heap (make-priority-queue s)))
    (iter (for i below (length s))
          (setf [s i] (extract-min heap))
          (finally (return s)))))

(defun make-priority-queue (&optional initial-elements)
  ;; The faster version
  (let ((priority-queue (make-instance 'priority-queue)))
    (when initial-elements
      (with-slots (storage-array size) priority-queue
        (setf size (length initial-elements))
        (iter (for i below size)
              (setf [storage-array (1+ i)]
                    [initial-elements i]))
        (iter (for i from size downto 1)
              (bubble-down priority-queue i))))
    priority-queue))
```

Rethink why the faster version of `make-priority-queue` can work in linear time.

Can we detect the kth smallest element in less than O(k log n) time? TODO: Understand this!

```lisp
(defun heap-compare (priority-queue i count x)
  (with-slots (storage-array size) priority-queue
    (cond ((or (<= count 0)
               (> i size))
           nil)
          ((< [storage-array i] x)
           (setq count (heap-compare priority-queue
                                     (priority-queue-young-child i)
                                     (1- count)
                                     x))
           (setq count (heap-compare priority-queue
                                     (1+ (priority-queue-young-child i))
                                     count
                                     x))))
    count))
```

As in the case of selection-sort becoming faster using heaps, so does insertion-sort.

# 4.4 War Story: Give me a Ticket on an Airplane

Some combination of priority queues and merge sort. No significant lesson.

Just a good data structure: returning next item of a sorted combination of list
without actually sorting the list before-hand.

# 4.5 Mergesort: Sorting by Divide-and-Conquer

```lisp
(defun merge-sort (sequence &optional (low 0) (high (length sequence)))
  (when (< low high)
    (let ((middle (floor (+ low high) 2)))
      (merge-sort sequence low middle)
      (merge-sort sequence middle high)
      (our-merge sequence low middle high))))

(defun our-merge (sequence low middle high)
  (let ((buffer-1 (subseq sequence low middle))
        (buffer-2 (subseq sequence middle high)))
    (flet ((end-of-subseq-1 (pos)
             (>= pos (length buffer-1)))
           (end-of-subseq-2 (pos)
             (>= pos (length buffer-2))))
      ;; off-by-1 errors?
      (iter (generate i1 from 0)
            (generate i2 from 0)
            (for i from 0)
            (while (or (end-of-subseq-1 i1)
                       (end-of-subseq-2 i2)))
            (if (< [buffer-1 i1] [buffer-2 i2])
                (setf [sequence i] [buffer-1 (next i1)])
                (setf [sequence i] [buffer-2 (next i2)]))))))
```

# 4.6 Quicksort: Sorting by Randomization

```lisp
(defun quicksort (s l h)
  (when (> (- h l) 0)
    (let ((p (partition s l h)))
      (quicksort s l p)
      (quicksort s (1+ p) h))))

(defun partition (s l h)
  (let ((p h))
    (iter (for i from l below h)
          (generate first-high from l)
          (when (< [s i] [s p])
            (rotatef [s i]
                     [s (next first-high)]))
          (finally (rotatef [s p]
                            [s first-high])
                   (return first-high)))))
```

Randomized quicksort runs in Θ(n log n) time on any input, with high probability.
Randomization is crucial - and should be added as a first step above. (See how randomization
helps.)

The name Quicksort owes itself to being "empirically" faster - however, it's best to compare
performance post implementation.

# 4.7 Distribution Sort: Sorting via Bucketing

Bucketing is effective when the data distribution is roughly uniform.

Recall: why the lower bound on sorting is Ω(n log n)?

# 4.8 War Story: Skiena for the Defense

Lessons learnt: stay away from courts :P. For large datasets, often, (external) memory performance
is the bottleneck.

# 4.9 Binary Search and Related Algorithms

```lisp
(defun binary-search (s key low high)
  (if (> low high) -1
      (let ((middle (floor (+ low high) 2)))
        (cond ((= [s middle] key) middle)
              ((> [s middle] key)
               (binary-search s key low (1- middle)))
              ((< [s middle] key)
               (binary-search s key (1+ middle) high))))))
```

Binary search can be useful for counting occurences, as well as root-finding. One sided binary
search proves useful when we know the key is closer to our current position.

# 4.10 Divide-and-Conquer

Mergesort, fast Fourier transform, Strassen's matrix multiplication, binary search all
use divide and conquer.

For recurrences of the form T(n) = aT(n/b) + f(n):

1. If f(n) = O(n^{log\_b(a) - ε}) for ε>0, then T = Θ(n^{log\_b(a)}).
2. If f(n) = Θ(n^{log\_b(a)}), then T = Θ(n^{log\_b(a)} lg n).
3. If f(n) = Ω(n^{log\_b(a) + ε}) for ε>0, and a f(n/b) ≤ c f(n) for some c<1,
then T(n) = Θ(f(n)).
