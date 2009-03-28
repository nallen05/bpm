
# BPM: pattern matching for Common Lisp

http://github.com/nallen05/bpm

## Overview

BPM is a simple pattern matching library for Common Lisp.

**STOP -- BPM is depricated in favor of BPM2, BPM's ideological successor. BPM2 is quasi-vapourware slowly developing [here](http://github.com/nallen05/bpm2)**

## Download 

Download the latest gzipped tarball:

* [Version 0.1.1](http://cloud.github.com/downloads/nallen05/bpm/bpm_0.1.1.tar.gz) 3.28.2009 -- Updated documentation for Github, converted to Markdown
* [Version 0.1](http://cloud.github.com/downloads/nallen05/bpm/bpm_0.1.tar.gz) 4.11.2007

or get the latest version from the [Git](http://git-scm.com/) repository:

    git clone git://github.com/nallen05/bpm.git

## License

[BSD](http://en.wikipedia.org/wiki/BSD_License)

## A Bird's Eye View of BPM

`MATCH` is like `CASE' except it uses pattern matching instead of keys and it executes success forms within the [lexical scope](http://www-cgi.cs.cmu.edu/Groups/AI/util/html/cltl/clm/node43.html) of the matched pattern.

`DEF!` and `DEF` allow you to write lisp functions in pattern matching style.

`BPM-LAMBDA` is a pattern matching [lambda](http://www.lisp.org/HyperSpec/Body/sym_lambda.html).

`->` and `-->` are self-evaluating constants provided as syntactic spice to help make pattern matching forms more readable.

`CREATE-BPM-COMPILER` is the low-level reification of everything BPM has to offer. It can safely be ignored if your not implementing your own domain specific syntactic abstractions or logic system on top of BPM.

## Pattern Matching

BPM uses "S-Expression Patterns" to match sexps. S-Expression Patterns are sexps that may or may not contain logic variables.

The following sexps are s-expression patterns:

    5
    nil
    (a . b)

They do not have any logic variables and they match the sexps:

    5
    nil
    (a . b)

respectively.

By default, `CREATE-BPM-COMPILER` recognises any symbol starting with `_` (the underscore) as a logic variable. In the pattern

    (foo . _bar)

`_BAR` is a logic variable. This pattern matches:

    (foo)
    (foo 1)
    (foo 1 2)
    (foo 1 2 4)
    (foo . anything)

In fact it matches any list with `foo` as its first element. Within the respective scope of these matches, `_BAR` would be bound to

    NIL
    (1)
    (1 2)
    (1 2 3)
    anything

The variable `_` (just the underscore) is the "wildcard". It is a special variable that can match anything and never remembers what it's bound to. So the pattern

    (_ _ _)

matches any list of the length 3, and `_` isn't bound to anything within the scope of this match.

BPM also matches/destructure simple vectors, so

    #(_1 _1 _2 _2)

matches a simple vector of length 4 whose first two elements are the same and last two elements are the same.

Caveat: It is not immediately assumed that `_1` != `_2`. So this pattern matches

    #(a a a a)

and

   #(b b b b)

as well as

    #(a a b b)

Note: you can use a `WHERE` or `WHERE-NOT` clauses within `DEF!`/`DEF` or `MATCH` forms to give your matches arbitrary constraints such as `(not (eql _1 _2))`.

## Equality

`CREATE-BPM-COMPULER` uses `EQL` as the default equality test. This can be overwritten by rebinding `*LOGIC-VAR-EQUALITY-TEST*` during bpm-compile-time.

## Changing the Syntax

`CREATE-BPM-COMPILER`'s syntax is slightly flexible. see `*LOGIC-VAR-PREFIX-CHAR*`, `*LOGIC-VAR-PRED*`,`*LOGIC-VAR-WILDCARD-PRED*`, and *DESTRUCTURE-SIMPLE-VECTORS-P*.

## API

- - -

* `MATCH (form &amp clauses)`

   _macro_  `Match` is like [`CASE`](http://www.lisp.org/HyperSpec/Body/mac_casecm_ccasecm_ecase.html) except it uses S-Expression PAtterns instead of keys and success forms re executed within the lexical scope of their respective matches.

    cl-user> (match '(1 2)
    	       ((_h . _t) -&gt; (format nil "list with a head of ~A and a tail of ~A" _h _t))
	        (_x -> (format nil "~A is an atom" _x)))
    "list with a head of 1 and a tail of (2)"

    cl-user> (match 3
 	      ((_h . _t) -&gt; (format nil "list with a head of ~A and a tail of ~A" _h _t))
	      (_x -> (format nil "~A is an atom" _x)))
    "3 is an atom"

   `MATCH` clauses can optionally take an arbitrary number of `WHERE` and `WHERE-NOT` clauses.

- - -

* `DEF! (name pattern &body body)`
  `DEF (name pattern &body body)`

  _macro_ `DEF!` is like `DEFUN` except it takes an S-Expression Pattern instead of a lambda list. The created function takes one argument: if that argument matches `PATTERN`, then the function executes `BODY` within the lexical scope of the match. Otherwise it simply returns `NIL`.

    cl-user> (def! foo (_x . _y)
               -> (cons _y _x))
    foo
    cl-user> (foo '(1 . 2))
    (2 . 1)
    cl-user> (foo 1)
    nil

`DEF` adds additional pattern matching clauses to functions define with `DEF!`. A `DEF!` can be thought of as the first clause to a `MATCH` form where subsequent `DEF` forms are the subsequent `MATCH` clauses.

    cl-user> (def foo _ -> 'not-a-cons)
    foo
    cl-user> (foo '(1 2 3))
    ((2 3) . 1)
    cl-user> (foo 1)
    not-a-cons

Both `DEF!` and `DEF` can optionally have an arbitrary number of `WHERE` and `WHERE-NOT` clauses in `BODY`.

- - -

* `WHERE`
  `WHERE`

  _symbols_ The symbols `WHERE` and `WHERE-NOT` act as special keywords when seen inside `MATCH` clauses or `DEF!`/`DEF` bodies.

  `WHERE` and `WHERE-NOT` can be thought of as functions that take a single argument (`TEST`): when `WHERE` or `WHERE-NOT` forms appear immediatly after a sexp pattern, a match against that pattern is not considered successful unless all the `WHERE` arguments evaluate to non-`NIL` values and the `WHERE-NOT` arguments evaluate to `NIL`.

Note: `WHERE` and `WHERE-NOT` clauses are "short-circuiting" (like `AND`).

    (match request
      (#(_user _a) (where (find-user _user))
               --> (do-user-actions _a))
      (_req       --> (error 'bad-request :request _req)))

    (def! get-username #(_id _node)

      ;; validate node
      (where (node? _node))
      (where (node-exists? _node))

      ;; find username 
      (.get-username _id _node))

    (def get-username #(_id _node)
      (error 'bad-node :node _node))

    (def get-username _req
      (error 'bad-request :request _req))

- - -

* `BPM-LAMBDA (pattern &body body)`

  _macro_ `BPM-LAMBDA` is just like `LAMBDA` except that it takes an S-Expression pattern as a first argument instead of a lambda list.

  The resultant function takes on argument: if that argument matches `PATTERN` then the function executes `BODY` within the lexical scope of the match. Otherwise it simply returns `NIL`.

  Caveat: `BPM-LAMBDA` does not understand `WHERE` or `WHERE-NOT` clauses.

- - -

* `CREATE-BPM-COMPILER (pattern &optional bound)`

  _function_ `CREATE-BPM-COMPILER` takes an S-Expression Pattern and a list of already bound logic variables. It returns a function (`FUNCTION1`) and a new list of logiv variables.

  This new function takes a piece of unevaluated lisp code and returns the source of a new unary function (`FUNCTION2`). If `FUNCTION2`'s argument matches `PATTERN`, then `FUNCTION2` executes the lisp code given to `FUNCTION2` within the lexical scope of the match. Otherwise it simply returns `NIL`.

- - -

`BPM-LAMBDA` can be implemented as:

    (defmacro bpm-lambda (pattern &amp;body body)
      (funcall (create-bpm-compiler pattern) `(progn ,@body)))

- - -

`CREATE-BPM-COMPILER`'s second return value is the accumulated list of logiv variables that will be bound within `FUNCTION2`'s evaluation of the lisp form given as an argument to `FUNCTION1`. This list can be given as the argument to `CREATE-BPM-COMPILER`'s optional `BOUND` argument in recursive calls to `CREATE-BPM-COMPILER`: in this way it is possible to create nested pattern matchers that are lexically aware of the pattern matching environment around them.

    cl-user> (create-bpm-compiler '(1 _1 #(_1 _2) _2))
    #<Closure (:internal create-bpm-compiler 1) @ #x10719da2>
    (_2 _1)
    
    cl-user> (funcall * '(progn (print _1) (print _2)))
    (lambda (#:g6249)
      (declare (dynamic-extent #:g6249))
      (and (listp #:g6249)
           (eql (first #:g6249) '1)
           (let ((#:g6252 (rest #:g6249)))
             (declare (dynamic-extent #:g6252))
             (if (listp #:g6252)
                 (let ((_1 (first #:g6252))
    		   (#:g6255 (rest #:g6252)))
                   (declare (dynamic-extent #:g6255))
                   (if (listp #:g6255)
                       (let ((#:g6257 (first #:g6255)))
                         (declare (dynamic-extent #:g6257))
                         (and (simple-vector-p #:g6257)
                              (= (length #:g6257) 2)
                              (eql (svref #:g6257 0) _1)
                              (let ((_2 (svref #:g6257 1))
                                    (#:g6261 (rest #:g6255)))
                                (declare (dynamic-extent #:g6261))
                                (and (listp #:g6261)
                                     (eql (first #:g6261) _2)
                                     (not (rest #:g6261))
                                     (progn (print _1)
    					(print _2))))))))))))
    
    cl-user> (funcall (coerce * 'function) '(1 one #(one two) two))
    "one"
    "two"

- - -

* `*LOGIV-VAR-PREFIX-CHAR*`

  _variable_ The character that indicates logic and wildcard variables. Initially set to `#\_`.

- - -

* `*LOGIV-VAR-PRED*`

  _variable_ A unary predicate that returns if its argument is a logic variable. Initially set to a function that looks for symbols whose names start with `*LOGIC-VAR-PREFIX-CHAR*

- - -

* `*LOGIV-VAR-WILDCARD*`

  _variable_ A unary predicate that returns `T` if its argument is a logic var wildcard. Initially set to a function that looks for symbols with the name `*LOGIC-VAR-PREFIX-CHAR*

- - -

* `*DESTRUCTURE-SIMPLE-VECTORS-P*`

  _variable_ Initially set to T. Bind this to NIL if you want vectors to be seen as atomic objects in your S-Expression Patterns.

- - -

* `*LOGIC-VAR-EQUALITY-TEST*`

  _variable_ Lambda form or name of function that tests whether or not S-Expressions match parts of an S-Expression pattern. Initially set to `EQL`.

- - -

* `->' / `-->`

  _constants_ Syntactic spice. These self-evaluating constants exist only to help make your code more visually intuitive.
