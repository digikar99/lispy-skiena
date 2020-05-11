# 2.1 The RAM Model of Computation

Simple operations - +, \*, -, =, if, call, memory access - that take exactly one step; others like loops and subroutines are non-simple.

# 2.2 The Big Oh Notation

- f(n) = O(g(n)): ∃c : f(n)≤c·g(n) for all n≥n₀ for some constant n₀.
- f(n) = Ω(g(n)): ∃c : f(n)≥c·g(n) for all n≥n₀ for some constant n₀.
- f(n) = Θ(g(n)): ∃c₁c₂ : c₂·g(n)≤f(n)≤c₁·g(n) for all n≥n₀ for some constant n₀.

# 2.3 Growth Rates and Dominance Relations

- f(n) = O(g(n)) ≡ g dominates f ≡ g≫f
- Function classes in order of increasing dominance: constant, logarithmic
linear, superlinear, quadratic, cubic, exponential, factorial

# 2.4 Working with Big Oh

- The sum of two functions is governed by the dominant one.
- For non-constants, multiplication simply multiplies.

# 2.5 Reasoning about Efficiency

```lisp
(defun selection-sort (s &optional (n (length s)))
  (iter (for i below n)
        (when-let (minimizer (iter (for j from (1+ i) below n)
                                   (finding j minimizing [s j])))
          (rotatef [s i]
                   [s minimizer]))        
        (finally (return s))))

(defun insertion-sort (s &optional (n (length s)) (compare '<))
  (iter (for i from 1 below n)
        (iter (for j from i above 0 by 1)
              (while (funcall compare
                              [s j]
                              [s (1- j)]))
              (rotatef [s j] [s (1- j)]))))

(defun find-match (p txt) ; p - pattern
  (let ((m (length p))
        (n (length txt)))
    (iter (for i below (- n m))
          (iter (for j below m)
                (while (char= [txt (+ i j)]
                              [p j]))
                (finally (when (= j m)
                           (return-from find-match i))))
          (finally (return-from find-match -1)))))

(defun matrix-mult (a b)
  (let ((x (array-dimension a 0))
        (y (array-dimension a 1))
        (z (array-dimension b 1)))
    (let ((c (make-array (list x z) :initial-element 0)))
      (iter (for i below x)
            (iter (for j below y)
                  (iter (for k below z)
                        (incf [c i j]
                              (+ [a i k] [b k j]))))))))
```

See, how:

- selection-sort is Θ(n²)
- insertion-sort is O(n²)
- find-match is O(nm)
- matrix-mult is O(xyz)

# 2.6 Logarithms and Their Applications

Binary Search, Trees, Bits, Multiplication, Fast Exponentiation, Summations.
Here's fast exponentiation:

```lisp
(defun power (a n)
  (if (zerop n)
      1
      (let ((x (power a (floor n 2))))
        (if (evenp n)
            (* x x)
            (* a x x)))))
```

Other than them, there's also Criminal Justice - wherein stealing $10,000 five times
would get you more time than $1,000,000 at once. So - 

>*If you are gonna do the crime, make it worth the time!*

:P

# 2.7 Properties of Logarithms

- The base of the logarithm has no real impact on the growth rate.
- Logarithms cut any function down to size.

# 2.8 War Story: Mystery of the Pyramids

- **Waring's Problem:** Can every integer be expressed as the sum of at most four integer squares?
- **Pyramidal Number:** A number of the form (m³ - m)/6.
- **Conjecture:** Every integer can be written as the sum of atmost five pyramidal numbers.

There are O(n^{1/3}) pyramidal numbers. Testing for 3-pyramidal-number-sum for n numbers,
therefore, meant O(n * (n^{1/3})³) = O(n²) time. So, goal: avoid trying all of the triples.

See also the Knapsack Problem.

What we do, therefore, is create the two-table and three-table, and directly
perform binary searches in these tables. We rather test whether a number can be expressed
as the sum of two pyramidal numbers in the two-table, etc.

# 2.9 Advanced Analysis

## 2.9.1 Esoteric Functions

See the book for how each of these arise:

- Inverse Ackerman's function
- log log n
- logn / loglogn: 
- log²n
- sqrt(n)
- n^(1+ε)

# 2.9.2 Limits and Dominance Relations

f(n) dominates g(n) if lim(n→∞) g(n)/f(n) = 0. Also see section 2.3.
