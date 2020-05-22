# A lispy approach to Skiena's Algorithm Design Manual

Well, okay, the only lispy thing about this is we use [iterate](https://digikar99.github.io/common-lisp.readthedocs/iterate/). I feel \*everyone\*
must be aware of iterate - possibly use such "high level" iteration techologies while
writing (pseudo)code.

Besides, making things lispy, another reason this repository is in existence is for notes for myself - and possibly others.

See the book for chapter 10 - which happens to be an approximate algorithm to design an
algorithm.

## Prerequisites

```lisp
(ql:quickload '(:iterate :alexandria :reader :arrows))
(defpackage :skiena
  (:use :cl :iterate :alexandria :arrows))
(reader:enable-reader-syntax 'get-val 'lambda)
(in-package :skiena)
```

Most algorithms / lisp-functions depend on other additional functions for working correctly.
and mostly exist for illustrative purposes only.

No care has been taken to ensure style consistency yet.
