(ns ch02-recursion
  "Section 2.6 is all about recursion.
  It's only loosly related to previous code that deals with Lisp2 characteristics
  and dynamic variables.
  That is why we create a separate namespace for Recursion."
  (:require [ch01-evaluator-final :as e]))


;; a canonical example of recursive function
(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))))

(fact 5)
;; => 120
(fact 20)
;; => 2432902008176640000

;; Note: Clojure does not implement tail-call optimization (TCO)
;; In this case, the fact isn't tail-recursive anyway
;; and it also doesn't matter for anything but a very large results


;; large numbers
(comment
  ;; ... much earlier we hit the max range of longs supported by `*`
  (fact 21)
  ;; 1. Unhandled java.lang.ArithmeticException
  ;; long overflow

  ;; ... we can use bigintegers
  (fact 100N)
  ;; => 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000N

  ;; ... or we have to use `'*`
  (defn fact' [n]
    (if (zero? n)
      1
      (*' n (fact' (dec n)))))
  (fact' 100)
  ;; => 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000N

 .)



;;; 2.6.2 - Mutual recursion example (p. 56)

;; First, forward declaration is needed to be able to use `my-odd?` inside `my-even?`
(declare my-odd?)

(defn my-even? [n]
  (or (zero? n)
      (my-odd? (dec n))))

(defn my-odd? [n]
  (if (zero? n)
    false
    (my-even? (dec n))))

(my-even? 10)
;; => true
(my-even? 9)
;; => false
(my-odd? 9)
;; => true



;;; 2.6.3 Local Recursion in Lisp2
;;; here they discuss `flet` and how it cannot be used to define recursive functions
;;; Check Common Lisp docs:
;;; - flet: https://jtra.cz/stuff/lisp/sclr/flet.html
;;; - labels: https://jtra.cz/stuff/lisp/sclr/labels.html
;;;
;;; This is only used as a demonstration - it's not valid Clojure
;;; and it might not even be a valid Common Lisp.
(comment

  ;; flet bindings aren't recursive and thus the `fact` function used in `flet`'s body
  ;; is referring to the global `fact` function not the local function being defined
  ;; => we get _no_ recursion as a result (if the global fact function isn't recursive)
  ;; Note: this is Common Lisp - not Clojure!
  (flet ((fact (n) (if (= n 0) 1
                       (* n (fact (- n 1))))))
        (fact 6))

  ;; Therefore, in Lisp 1.5 they had `label` - this returns an anonymous function
  ;; - however, it's not able to handle _mutual_ recursion
  (label fact (lambda (n) (if (= n 0) 1
                              (* n (fact (- n 1))))))
  
  ;; For (mutual) recursion, Common Lisp has `labels`:
  (labels ((fact (n) (if (= n 0) 1
                         (* n (fact (- n 1))))))
          (fact 6))
  ;; #t and #f must be commented otherwise the Clojure reader fails
  (funcall (labels ((even? (n) (if (= n 0)
                                 ;#t
                                 (odd? (- n 1))))
                    (odd? (n) (if (= n 0)
                                ;#f
                                (even? (- n 1)))))
                   (function even?))
           4)

  .)


;;; 2.6.4 Local Recursion in Lisp1

;; p. 58 - try to reproduce letrec with simple let and using 'void
;; - the very first, simplest attempt is doomed to fail because we cannot reference
;;   undefined symbols in a let variable's value
(comment 
  (let [local-even? (fn [n] (or (zero? n) (local-odd? (dec n))))
        local-odd? (fn [n] (if (zero? n) false (local-even? (dec n))))]
    (local-even? 4))
  ;;    Unable to resolve symbol: local-odd? in this context

  .)

;; - let's try to bind them to `void first and then redefine
(let [local-even? `void
      local-odd? `void
      local-even? (fn [n] (or (zero? n) (local-odd? (dec n))))
      local-odd? (fn [n] (if (zero? n) false (local-even? (dec n))))]
  (local-even? 4))
;; => nil
;; as we see that didn't work because it returns nil because we didn't really redefined them in a good way
;; - in Clojure, we cannot use `set!` to modify local bindings

;; - let's try again with thread-local vars (see Joy of Clojure, p.258)
;;   Surprisingly, this works without any extra effort!
;;   `with-local-vars` is kinda special - check it's source code!
;;   - it takes the names and binds each name to a Var object via `let`
;;     these Var objects are unnamed and are only initialized in the let's body
;;     so they can even refer to each other
(with-local-vars
 [local-even? (fn [n] (or (zero? n) (local-odd? (dec n))))
  local-odd? (fn [n] (or (= n 1) (local-even? (dec n))))]
  (local-even? 4))
;; => true



;;; 2.6.5 Creating Unitialized Bindings (p.60)
;;; This shows a couple of attempts define uninitialized vars
;;; without a special form and then uses a new special form `let`
;;; Without a special form, we cannot avoid checks to avoid programmers
;;; to use our special unitialized marker.


;; Implement `let` as a special form -> add it to our interpreter
;; This version supports the following form `(let ( variable ...))`
;; where the variables can be just symbols in which case they are uninitialized

(def ^:private the-uninitialized-marker `non-initialized)

;; enhance `lookup` to make sure we fail fast when somebody tries to use an uninitialized value
(defn lookup [id env]
  (if-let [[_k v] (find env id)]
    (if (= v the-uninitialized-marker)
      (e/wrong "Uninitialized binding" id {:env env})
      v)
    (e/wrong "No such binding" id)))

;; need to also redefine `eprogn` if we want to support nested `let`s in our language
(declare evaluate)
(defn eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; first expression is always evaluated
          fst-val (evaluate fst env)]
      (if (list? rst)
        (eprogn rst env) ; process the rest of the forms by calling `eprogn` recursively
        ;; the final term in the sequence
        fst-val))
    ;; here we could also return `nil` or other value - see discussion on p.10
    ()))

(defn evaluate [exp env]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (lookup exp env)
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
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (e/update! (second exp) env (evaluate (nth exp 2) env))
      lambda (e/make-function (second exp) (nnext exp) env)
      ;; CHANGE: implement a new special form `let` including the support for uninitialized bindings
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) env)))
                             bindings)]
            (eprogn body (e/extend env variables values)))

;; it's not a special form, just ordinary function => call it!
      (e/invoke (evaluate (first exp) env)
                (e/evlis (rest exp) env)))))

;; firs try if it can create uninitialized bindings
(evaluate '(let (local-even? local-odd?)
             local-even?)
          {})
;; => ch02-recursion/non-initialized

;; Can we now define even? and odd?
(assert
 (= 't
    (evaluate '(let (local-even? local-odd?)
                 (let ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                       (local-odd? (lambda (n) (if (= n 1) 'f (local-even? (- n 1))))))
                   (local-even? 4)))
              (assoc e/env-global '= =))))
;; => t


;; To avoid using nested lets it would be nice if we implemented `letrec` (p. 62)
;; - for that we need to add it to our interpreter again
;; Also, they show this in section 2.6.6 Recursion without assignmen

(defn evaluate [exp env]
  (if (e/atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (lookup exp env)
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
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (e/update! (second exp) env (evaluate (nth exp 2) env))
      lambda (e/make-function (second exp) (nnext exp) env)
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) env)))
                             bindings)]
            (eprogn body (e/extend env variables values)))
      ;; CHANGE: let's implement letrec!
      letrec (let [body (nnext exp)
                   bindings (second exp)
                   ;; first add variables with uninitialized values
                   variables (mapv first bindings)
                   new-env (e/extend env variables (mapv (constantly the-uninitialized-marker) bindings))
                   ;; then update variables to their proper values
                   values (mapv (fn [[_fn-name fn-def :as _binding]]
                                  (e/evaluate fn-def new-env))
                                bindings)
                   updated-env (e/extend env variables values)]
               (eprogn body updated-env))

      ;; it's not a special form, just an ordinary function => call it!
      (e/invoke (evaluate (first exp) env)
                (e/evlis (rest exp) env)))))

(assert
 (= 't (evaluate '(letrec ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                           (local-odd? (lambda (n) (if (= n 1) 'f (local-even? (- n 1))))))
                          (local-even? 4))
                 (assoc e/env-global '= =))))
;; => t
