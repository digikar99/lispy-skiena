# Introduction

- Problem vs Problem Instance


```lisp
(defun insertion-sort (s &key (predicate '<) (n (length s)))
  (iter (for i from 1 below n)
        (iter (for j from i above 0)
              (while (funcall predicate
                              [s j]
                              [s (1- j)]))
              (rotatef [s j] [s (1- j)]))
        (finally (return s))))
```

- In industrial settings, any program that seems to give good enough answers without slowing the application down is often acceptable, regardless of whether a better algorithm exists.
- The issue of finding the best possible answer or achieving maximum efficiency usually arises in industry only after serious performance or legal troubles.


# 1.1 Robot Tour Optimization

*Why "it's obvious" is insufficient?*

Input: A set S of n points on a plane. <br/>
Output: What is the *shortest cycle tour* that visits each point in the set S?

Here's the *"it's obvious"* solution:
```lisp
;;; make structure for p
;;; make functions unvisited-p, closest-unvisited-point
;;; possibly replace call to format with a generic function
(defun nearest-neighbour (p)
  (let ((p0 (random-elt p)))
    (iter (for p initially p0
               then (closest-unvisited-point p))
          (format t "~D - " p)
          (while (some #'unvisited-p p))
          (finally (format t "~D~%" p0)))))
```

That fails. (Exercise: find a bad instance. Hint: oscillate.)

```lisp
;;; Undefined functions: end-points, distinct-vertext-chains, distance, connect
(defun closest-pair (p)
  (iter (for i from 1 to (1- (length p)))
        (iter (for (s t) in (-> (distinct-vertex-chains)
                                (end-points))) 
              (finding (list s t)
                       minimizing (distance s t)
                       into minimizers)
              (finally (apply 'connect minimizers)))
        (finally (connect end-points)))) ; What?
```
Fails: 6 point rectangle, (1-ε) and (1+ε).

Correct. But, too slow.
```lisp
;;; Define permutations and cost
(defun optimal-tsp (p)
  (iter (for p-i in (permutations p))
        (finding p-i minimizing (cost p-i))))
```
This is the Traveling Salesman Problem.

# 1.2 Selecting the Right Jobs

Input: A set I of n intervals on a line.
Output: Largest subset of mutually non-overlapping intervals from I.

Failing approaches (exercise: find the counterexample):

- Earliest Job First
- Shortest Job First

A correct algorithm:

```lisp
(defun exhaustive-scheduling (i)
  (iter (for s in (subsets i))
        (when (mutually-overlapping-p s)
          (finding s maximizing (size s)))))
```

However, `(length (subsets i))` is `(expt 2 (length i))`. Does there exist a faster algorithm? (Hint: yes.)

```lisp
(defun optimal-scheduling (i)
  (iter (while i) ; (not (null i))
        (with j = (job-with-earliest-completion-date i))
        (remove j i)
        (mapcar λ(remove - i)
                (find-if λ(intersect j -) i))))
```

# 1.3 Reasoning about Correctness

## 1.3.1 Expressing Algorithms

We use a combination of (common) lisp and pseudo-code. (PS: There is an [clj-iterate](https://github.com/jpalmucci/clj-iterate) for Clojure; though, we stick to the Common Lisp [iterate](https://digikar99.github.io/common-lisp.readthedocs/iterate/).)

## 1.3.2 Problems and Properties

- The set of allowed input instances
- allow too broad a set of inputs, and one might not have an efficient algorithm
- The required properties of the algorithm's output
- "best" route between two places on a map: *what is "best"?*
- compound goals can make things complex

## 1.3.3 Demonstrating Incorrectness

Find \*good\* counterexamples.

- Ensure verifiability.
- Simplicity.
- Think small.
- Think exhaustively.
- Hunt for the weakness: go for a tie, seek extremes

## 1.3.4 Induction and Recursion

## 1.3.5 Summations

# 1.4 Modeling the Problem

Reduce your problems to something that has already been solved before.

## 1.4.1 Combinatorial Objects

- **Permutations:** arrangement, tour, ordering, sequence
- **Subsets:** cluster, collection, committee, group, packaging, selection
- **Trees:** hierarchy, dominance relationship, ancestor/descendant relationship, taxonomy
- **Graphs:** network, circuit, web, relationship
- **Points:** sites, positions, data records, locations
- **Polygons:** shapes, regions, configurations, boundaries
- **Strings:** text, characters, patterns, labels

## 1.4.2 Recursive Objects

Permutations, Subsets, Trees, Graphs, Points, Polygons, Strings. (Exercise: see how.)

# 1.5 About the War Stories

# 1.6 War Story: Psychic Modeling

Lesson: *(Ensure you) Model the problem correctly.*

(The below is the result of wrong modeling. The correct solution - or the problem itself is not very clear to me.)

```lisp
(defun lotto-ticket-ket (n k l)
  (let ((v (make-array (count-combinations n l) :initial-element nil)))
    (iter (while (some 'null v))
          (for t0 in (subsets n k))
          (iter (for ti in (subsets t0 l))
                (setf [v (rank ti)] t)))))
```
