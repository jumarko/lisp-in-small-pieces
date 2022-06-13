(ns ch02-lisp2
  "Chapter 2 exercises related to Lisp-2 style lisps, like Common Lisp.
  It presents an alternative version of evaluate (`f-evaluate`)
  that has a special function `evaluate-application` for evaluating function forms.
  There are also `f-make-function`, `f-evlis` and `f-eprogn`.

  Notice, that there's also a separate environment, `fenv`, dedicated functions;
  it doesn't contain normal variables."
  (:require [ch01-evaluator-final :as e]))


;;; In Lisp-2, we will restrict the first element of every form to be a symbol.
;;; That simplifies  the evaluation for the term in function position a lot.
;;; We no longer needs all the complexity of `evaluate`.

;; Let's copy `evaluate` but modify the function invocation at the end
;; - call `evaluate-application` instead of using `invoke` directly
(declare evaluate-application f-eprogn f-evlis f-make-function)
(defn f-evaluate [exp env fenv]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (e/lookup exp env)
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (f-evaluate (second exp) env fenv)
           (f-evaluate (nth exp 2) env fenv)
           (f-evaluate (nth exp 3) env fenv))
      begin (f-eprogn (rest exp) env fenv)
      set! (e/update! (second exp) env (f-evaluate (nth exp 2) env fenv))
      lambda (f-make-function (second exp) (nnext exp) env fenv)
      ;; CHANGE: call `evaluate-application` instead of `invoke`
      (evaluate-application (first exp)
                            (f-evlis (rest exp) env fenv)
                            env
                            fenv))))

;; ... we need to re-define the helper functions like f-eprogn and f-evlis
;; to make sure their propagate the function environment `fenv`
(defn f-evlis [exps env fenv]
  (if (list? exps)
    (map #(f-evaluate % env fenv) exps)
    ()))

(defn f-eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env fenv]
  (if (list? exps)
    (let [[fst & rst] exps
          fst-val (f-evaluate fst env fenv)]
      (if (list? rst)
        (f-eprogn rst env fenv) 
        fst-val))
    ()))

;; ... now we redefine `make-function` to leverage `fenv`
(defn f-make-function [variables body env fenv]
  (fn [values]
    ;; notice that `fenv` isn't extended since it's only for functions,
    ;; not for the variables inside the body of the function
    (f-eprogn body (e/extend env variables values) fenv)))

;; ... and finally we can implement `evaluate-application`
(defn evaluate-application [f args env fenv]
  (cond
    ;; function symbol -> invoke it right away
    (symbol? f)
    (e/invoke (e/lookup f fenv) args) ; notice we lookup the function symbol in `fenv`

    ;; lambda forms are also supported - notice they don't produce a function object (via `f-make-function`)
    ;; but instead are evaluated directly
    (and (list? f) (= 'lambda (first f)))
    ;; this is the same thing as body of the function produced by `f-make-function`
    ;; - using `(nnext f)` to skip lambda symbol and its arglist
    ;; - using `(second f)` to get the arg list, that is the list of symbols that should be bound to their values (`args`)
    (f-eprogn (nnext f) (e/extend env (second f) args) fenv)

    :else (e/wrong "Incorrect functional term" f {:f f :args args :fenv fenv :env env})))

;; we define our own + and * functions to be able to pass them
;; inside `fenv` - normally, they are defined  in `e/env-global`.
;; the reason why we cannot simply use clojure.core functions
;; is that we need to call `apply` since the arguments are wrapped within a list.
(defn my+ [args] (apply + args))
(defn my* [args] (apply * args))
(assert (= 7
           (f-evaluate '(+ 3 4) {} {'+ my+})))

;; now compare `evaluate` and `f-evaluate`
;; - without special support, with `f-evaluate` you shouldn't be able to use more complicated forms in a function position
;; (this example using `(if condition + *)` comes from p. 37)
(assert (= 7 (e/evaluate '((if condition + *) 3 4)
                         {'condition true '+ my+ '* my*})))
(assert (= 12 (e/evaluate '((if condition + *) 3 4)
                          {'condition false '+ my+ '* my*})))

;; f-evaluate doesn't know how to evaluate non-function forms in the function position
(try
  (f-evaluate '((if condition + *) 3 4)
              {'condition true}
              {'+ my+ '* my*})
  (assert false "f-evaluate should fail when called with a non-function form in the function position")
  (catch Exception e
    (assert (= "Incorrect functional term" (ex-message e)))))


;; Now, the other difference between `f-evaluate` and `e/evaluate` is that
;; `f-evaluate` can invoke lambda forms directly without creating a function object.
;; Consider this example and enable tracing for `f-evaluate` and `e/evaluate`:

(f-evaluate '((lambda (x) (* x x)) 3)
            {'x 2}
            {'* my*})
;; TRACE t11240: (ch02-lisp2/f-evaluate ((lambda (x) (* x x)) 3) {x 2} {* #function[ch02-lisp2/my*]})
;; TRACE t11241: | (ch02-lisp2/f-evaluate 3 {x 2} {* #function[ch02-lisp2/my*]})
;; TRACE t11241: | => 3
;; TRACE t11242: | (ch02-lisp2/f-evaluate (* x x) {x 3} {* #function[ch02-lisp2/my*]})
;; TRACE t11243: | | (ch02-lisp2/f-evaluate x {x 3} {* #function[ch02-lisp2/my*]})
;; TRACE t11243: | | => 3
;; TRACE t11244: | | (ch02-lisp2/f-evaluate x {x 3} {* #function[ch02-lisp2/my*]})
;; TRACE t11244: | | => 3
;; TRACE t11242: | => 9
;; TRACE t11240: => 9

(e/evaluate '((lambda (x) (* x x)) 3)
            {'x 2 '* my*})
;; TRACE t11247: (ch01-evaluator-final/evaluate ((lambda (x) (* x x)) 3) {x 2, * #function[ch02-lisp2/my*]})
;; TRACE t11248: | (ch01-evaluator-final/evaluate (lambda (x) (* x x)) {x 2, * #function[ch02-lisp2/my*]})
;; TRACE t11248: | => #function[ch01-evaluator-final/make-function/fn--10567]
;; TRACE t11249: | (ch01-evaluator-final/evaluate 3 {x 2, * #function[ch02-lisp2/my*]})
;; TRACE t11249: | => 3
;; TRACE t11250: | (ch01-evaluator-final/evaluate (* x x) {x 3, * #function[ch02-lisp2/my*]})
;; TRACE t11251: | | (ch01-evaluator-final/evaluate * {x 3, * #function[ch02-lisp2/my*]})
;; TRACE t11251: | | => #function[ch02-lisp2/my*]
;; TRACE t11252: | | (ch01-evaluator-final/evaluate x {x 3, * #function[ch02-lisp2/my*]})
;; TRACE t11252: | | => 3
;; TRACE t11253: | | (ch01-evaluator-final/evaluate x {x 3, * #function[ch02-lisp2/my*]})
;; TRACE t11253: | | => 3
;; TRACE t11250: | => 9
;; TRACE t11247: => 9

;; We can see that `e/evaluate` has more work to do because it creates a function object,
;; that is `#function[ch01-evaluator-final/make-function/fn--10567]` in the output above


;; Finally, when would `f-evaluate` still call `f-make-function`
;; instead of executing the lambda form right away?
;; One example is when we don't apply the function immediately
(assert (fn?
         (f-evaluate '(lambda (x) (* x x))
                     {'x 2}
                     {'* my*})))
;; TRACE t11284: (ch02-lisp2/f-evaluate (lambda (x) (* x x)) {x 2} {* #function[ch02-lisp2/my*]})
;; TRACE t11284: => #function[ch02-lisp2/f-make-function/fn--11194]

;; that returns a function object to the host environment.
;; we can apply it in Clojure though
(assert (= 100
           ((f-evaluate '(lambda (x) (* x x))
                        {'x 2}
                        {'* my*})
            [10])))
;; TRACE t11287: (ch02-lisp2/f-evaluate (lambda (x) (* x x)) {x 2} {* #function[ch02-lisp2/my*]})
;; TRACE t11287: => #function[ch02-lisp2/f-make-function/fn--11194]
;; TRACE t11288: (ch02-lisp2/f-evaluate (* x x) {x 10} {* #function[ch02-lisp2/my*]})
;; TRACE t11289: | (ch02-lisp2/f-evaluate x {x 10} {* #function[ch02-lisp2/my*]})
;; TRACE t11289: | => 10
;; TRACE t11290: | (ch02-lisp2/f-evaluate x {x 10} {* #function[ch02-lisp2/my*]})
;; TRACE t11290: | => 10
;; TRACE t11288: => 100



;;; funcall and function (special form) - p. 36,37
;;; - `funcall` allows us to take a result of an expression evaluated in the 'parametric world'
;;;   and treat is as a function
;;; - `function` allows us to get a functional value of the variable from the function world
;;;   (like * treated as a function, not variable denoting the last expression in Scheme / Common Lisp REPL.)

;; our first attempt to define funcall might look like this (at the bottom of the page 36)
(defn funcall [[f & args]]
  (if (> (count args) 1)
    (e/invoke f args)
    (e/wrong "Incorrect arity" 'funcall)))

;; a naive attempt to use `funcall` to fix our earlier problem
;; (not being able to call the result of an expression as an function)
#_(f-evaluate '(funcall (if condition + *) 3 4)
            {'condition true}
            ;; Note: I'm not sure this is how they meant to use funcall but it should be possible
            {'+ my+ '* my* 'funcall funcall})
;; => we get an error that + isn't defined in the 'parametric world'
;; (that is in the variable environment, as opposed to the functional environment, where it _is_ defined)
;; No such binding
;; {:expression +, :extra-info nil}

;; ... so we also need a new special form `function`
;; that returns the functional value/object

(defn f-evaluate [exp env fenv]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (e/lookup exp env)
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (f-evaluate (second exp) env fenv)
           (f-evaluate (nth exp 2) env fenv)
           (f-evaluate (nth exp 3) env fenv))
      begin (f-eprogn (rest exp) env fenv)
      set! (e/update! (second exp) env (f-evaluate (nth exp 2) env fenv))
      lambda (f-make-function (second exp) (nnext exp) env fenv)
      ;; CHANGE: new special form: `function` (see p.37 and p. 41)
      function (if
                 (symbol? (second exp)) (e/lookup (second exp) fenv)
                 (e/wrong "Incorrect function" (second exp)))
      (evaluate-application (first exp)
                            (f-evlis (rest exp) env fenv)
                            env
                            fenv))))
;; now we can use `function` to get the functional values out of `+` and `*` symbols
(f-evaluate '(funcall (if condition (function +) (function *)) 3 4)
            {'condition true}
            ;; Note: I'm not sure this is how they meat to use funcall but it should be possible
            {'+ my+ '* my* 'funcall funcall})
;; => 7


;;; 2.2.3 - Using Lisp2
;;; We are still limited to using only function explicitly specified in `fenv`.

;; this doesn't work:
#_(f-evaluate '(car '(1 2 3)) {} {'funcall funcall})
;; No such binding
;; {:expression car, :extra-info nil}



;;; TO fix that, we introduce `f-env-global` and helper macros to define global functions.
(def fenv-global {})
(defmacro definitial-function
  "Defines a new symbol in the global environment bound to given value
  or 'ch01-evaluator/void if no value is provided."
  ([name]
   `(definitial-function ~name :ch01-evaluator/void))
  ([name value]
   `(alter-var-root #'fenv-global #(e/extend % ['~name] [~value]))))
(defmacro f-defprimitive
  [name f arity]
  `(definitial-function ~name (e/lift ~f ~arity)))

(defn f-stdlib []
  (f-defprimitive car first 1)
  (f-defprimitive cons cons 2)
  (f-defprimitive + + 2)
  (f-defprimitive * * 2))
(f-stdlib)

;; ... and now we can call `car`!
(f-evaluate '(car '(1 2 3)) {} fenv-global)
;; => 1


;;; 2.3.4 Enriching the function environment with `flet` 
;;; Note: it reminds me clojure.core/letfn

;; flet is a new special form that's implemented in f-evaluate
(defn f-evaluate [exp env fenv]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (e/lookup exp env)
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (f-evaluate (second exp) env fenv)
           (f-evaluate (nth exp 2) env fenv)
           (f-evaluate (nth exp 3) env fenv))
      begin (f-eprogn (rest exp) env fenv)
      set! (e/update! (second exp) env (f-evaluate (nth exp 2) env fenv))
      lambda (f-make-function (second exp) (nnext exp) env fenv)
      function (if
                 (symbol? (second exp)) (e/lookup (second exp) fenv)
                 (e/wrong "Incorrect function" (second exp)))
      ;; CHANGE: add `flet` special form
      flet (let [flet-bindings (second exp)
                 flet-body (nnext exp)
                 fn-names (map first flet-bindings)
                 fn-impls (map (fn [def]
                                 ;; here is the core of what we do - notice that we use existing fenv
                                 ;; so that means we can't cross-reference functions defined in `flet` earlier
                                 (f-make-function (second def) (nnext def) env fenv))
                               flet-bindings)
                 fenv-with-fns (e/extend fenv fn-names fn-impls)]
             (f-eprogn flet-body env fenv-with-fns))
      (evaluate-application (first exp)
                            (f-evlis (rest exp) env fenv)
                            env
                            fenv))))

;; use it like this
(f-evaluate '(flet ((square (x) (* x x)))
                   (lambda (x) (square (square x))))
            {}
            fenv-global)
;; => #function[ch02-lisp2/eval12146/f-make-function--12147/fn--12148]

;; what if we evaluate the function to get an interesting result?
(f-evaluate '(flet ((square (x) (* x x)))
                   ((lambda (x) (square (square x)))
                    2))
            {}
            fenv-global)
;; => 16

;; But we cannot cross-reference functions inside flet bindings - that would be `flet*`
;; - note that you only get the error when you try to invoke that lambda in flet's body.
;; - for this to work, we need `labels` which is shown in the book on p. 42 but without any commentary (see p.57 for that)
(comment
 (f-evaluate '(flet ((square (x) (* x x))
                      (quad (x) (square (square x))))
                     ((lambda (x) (quad (quad x)))
                      2))
              {}
              fenv-global)
         .)
;; 1. Unhandled clojure.lang.ExceptionInfo
;; No such binding
;; {:expression square, :extra-info nil}



;;; 2.4 Comparing Lisp1 and Lisp2
;;; Implement the f-evaluate again, this time also adding `labels` special form
;;; and making `function` special form handling a bit more clever (using `f-lookup`)
(defn f-lookup [id fenv]
  (if-let [[_ f] (find fenv id)]
    f
    ;; CHANGE: we return a function that calls `wrong` instead of calling `wrong` immediately
    (fn [_values] (e/wrong "No such binding" id))))

;; use f-lookup in evaluate-application:
(defn evaluate-application [f args env fenv]
  (cond
    ;; function symbol -> invoke it right away
    (symbol? f)
    (e/invoke (f-lookup f fenv) args)

    ;; lambda forms are also supported - notice they don't produce a function object (via `f-make-function`)
    ;; but instead are evaluated directly
    (and (list? f) (= 'lambda (first f)))
    ;; this is the same thing as body of the function produced by `f-make-function`
    ;; - using `(nnext f)` to skip lambda symbol and its arglist
    ;; - using `(second f)` to get the arg list, that is the list of symbols that should be bound to their values (`args`)
    (f-eprogn (nnext f) (e/extend env (second f) args) fenv)

    :else (e/wrong "Incorrect functional term" f {:f f :args args :fenv fenv :env env})))

;; now use f-lookup in f-evaluate and also update `function` form handling and add `labels` form handling
(defn f-evaluate [exp env fenv]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (e/lookup exp env)
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (f-evaluate (second exp) env fenv)
           (f-evaluate (nth exp 2) env fenv)
           (f-evaluate (nth exp 3) env fenv))
      begin (f-eprogn (rest exp) env fenv)
      set! (e/update! (second exp) env (f-evaluate (nth exp 2) env fenv))
      lambda (f-make-function (second exp) (nnext exp) env fenv)
      function (cond
                 (symbol? (second exp))
                 ;; CHANGE: use `f-lookup` instead of `e/lookup` to search in the functions env
                 (f-lookup (second exp) fenv)

                 (and (list? (second exp)) (= 'lambda (first (second exp))))
                 (f-make-function (second (second exp)) ; lambda's arglist
                                  (nnext (second exp)) ; lambda's body
                                  env
                                  fenv)

                 :else (e/wrong "Incorrect function" (second exp)))
      ;; add `flet` special form as before
      flet (let [flet-bindings (second exp)
                 flet-body (nnext exp)
                 fn-names (map first flet-bindings)
                 fn-impls (map (fn [def]
                                 ;; here is the core of what we do - notice that we use existing fenv
                                 ;; so that means we can't cross-reference functions defined in `flet` earlier
                                 (f-make-function (second def) (nnext def) env fenv))
                               flet-bindings)
                 fenv-with-fns (e/extend fenv fn-names fn-impls)]
             (f-eprogn flet-body env fenv-with-fns))
      ;; CHANGE: implement `labels` for cross-references between functions (flet doesn't allow that)
      labels (let [bindings (second exp)
                   body (nnext exp)
                   fn-names (map first bindings)
                   ;; First, extend fenv with dummy bindings for all local functions...
                   fenv-with-fn-names (e/extend fenv fn-names (map (constantly `void) bindings))
                   ;; ... then update fenv to with actual implementations
                   ;; Notice how this is different from the mutable version used in the book
                   ;; - here we use `reduce` and simply accumulate fenv changes as go through all the fns
                   fenv-with-fn-impls (reduce (fn [new-fenv def]
                                                (e/update! (first def)
                                                           new-fenv
                                                           (f-make-function (second def) (nnext def) env new-fenv)))
                                              fenv-with-fn-names
                                              bindings)]
               (f-eprogn body env fenv-with-fn-impls))
      (evaluate-application (first exp)
                            (f-evlis (rest exp) env fenv)
                            env
                            fenv))))

;; What previously failed, now returns the proper result
;; (2^4)^4 = 2^16 = 65536
(assert (= 65536
           (f-evaluate '(labels ((square (x) (* x x))
                                 (quad (x) (square (square x))))
                                ((lambda (x) (quad (quad x)))
                                 2))
                       {}
                       fenv-global)))

;; But if we flip the function definitions (quad is now first and it uses square defined later)
;; it's broken again!
;; Note: this works with the implementation from the book
;; because they are mutating the mapping in the env
;; With immutable environment, it's gonna be harder => let's deal with this in section 2.6 (Recursion)
(comment
 (f-evaluate '(labels ((quad (x) (square (square x)))
                       (square (x) (* x x)))
                      (quad 2))
             {}
             fenv-global)
  ;; => this unfortunately throws:
 ;; clojure.lang.ExceptionInfo: Not a function {:expression ch02-lisp2/void, :extra-info ({:args (2)})}
 ;; - this is because `square` function called inside `quad` is resolved to void.


        .)
