# 9.1 Problems and Reductions

- Converting one problem into another: proving that the translation (reduction) is correct.
- Let A be a problem solvable by translating it to B. Suppose we know A takes at least Ω(P'(n))
to be solved. And let the tranlation require O(P(n)). Then we can be sure that B requires
at least Ω(P'(n) - P(n)).
- The domain of answers of Decision Problems are restricted to true and false; yet, they
happen to be good enough for the discussion

# 9.2 Reduction for Algorithms

- Translate the input for a problem we want to solve to a problem we know how to solve.

- <u>**Closest Pair**</u>: Find closest numbers in a given set.
    - **Decision Problem**: Is the difference between the closest pair below some given
	threshold?
- <u>**Longest Increasing Subsequence**</u>: O(n²) algorithm using edit-distance; though
O(n log n) algorithm exists.
- <u>**Least Common Multiple**</u>: Finding LCM and GCD. Easy when prime factors are known.
But finding them is hard. Enter Euclid's algorithm

```lisp
(defun our-gcd (a b)
  (let ((r (rem a b)))
    (if (zerop r) b (our-gcd b r))))
(defun our-lcm (a b) (/ (* x y) (our-gcd x y)))
```

- <u>**Convex Hull**</u>: By mapping i to (i, i²), sorting can be reduced to convex hull.
A faster than O{n logn) algorithm for convex hull would, therefore, imply a faster
algorithm for sorting; but we know that is not possible. Hence, O(n log n) lower bound for
convex hull as well.

# 9.3 Elementary Hardness Reductions

Assume Hamiltonian and Vertex Cover are hard. NOTE THE ORDER of reductions!
- Hamiltonian Cycle can be reduced to TSP
- Vertex Cover can be reduced to Independent Set
- Independent Set reduced to General Movie Scheduling
- Independent Set reduced to Cliques

# 9.4 Satisfiability

Given a set of clauses C, does there exist a satisfying truth assignment for the
variables v1, v2, ..., vn so tat each clause contains at least one true literal?

Satisfiability is proven to be NP-complete. But whether it has a polynomial time
algorithm remains an open problem.

The hardness of 3SAT can be shown by reducing the SAT problem to a 3SAT one.

# 9.5 Creative Reductions

Repeat: we reduce the new problem to a known hard problem, and not the other way!

# 9.6 The Art of Proving Hardness

Also refer Garey and Johnson's Computers and Intractability while searching for
NP-complete problems.

- Restrict the source problem: Hamiltonian path with each degree 3, or planar SAT
- Make the target problem as hard as possible. [TODO: What? What exactly is the order
of hardness?]
- Select the right source problem for the right reason: 3SAT, Integer Prog, Vertex
Cover, Hamiltonian Path
- Amplify the penalties for making the undesired selection
- Think strategically at a high level, then build gadgets to enforce tactics.
- When you get stuck, alternate between looking for an algorithm or a reduction

# 9.7 War Story: Hard Against the Clock

-

# 9.8 War Story: And Then I Failed

TODO: This is exactly the same as finding the vertex cover in this graph! Why???

# 9.9 P vs NP

- Non-deterministic Polynomial - recall non-deterministic automata
- Tangential thought: quantum computing and satisfiability??? Satisfiability and
neural networks???
- NP-hard ≥ NP-complete; problems discussed are NP-complete

# 9.10 Dealing with NP-complete Problems

- Algorithms fast in the average case
- Heuristics
- Approximation algorithms

**Approximating Vertex Cover**

- why only ≤ n/2 edges? Doesn't selecting n vertices require
at least n/2 edges, rather than at most?
- What's the matching here?
- Why no two edge share?

**The Euclidean Traveling Salesman**

TODO:
- Who gave us the permission to build new roads???
- "This shortcut tour is also within weight and twice that of optimal." Why???

**Maximum Acyclic Subgraph**

-

**Set Cover**

TODO: 
- One consequence ... nonincreasing sequence as the algorithm proceeds. Nope. Why???
>{1..10} as U. S₁={1,2,3,4,5}, S₂={1,2,3,6}, S₃={7,8,9}. What is the choice of the algorithm here???
- How the heck are you labelling milestones???
- Thus, no subset exists in S ... elements. Why???

