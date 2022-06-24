(ns ch02-recursion-with-mutable-env
  "This is a modification of `ch02-recursion` with a twist - we are using _mutable_ environment
  to work around problems with `let` / `letrec` and immutable environments.
  In particular, mutually recursive functions did not work (see `local-even?` and `local-odd?` example)
  because anonymous functions created in `e/make-function` (used for 'lambda forms)
  close over the environment passed to `make-function` - that is they use the (lexical) environment
  as it was at the time the function was defined, not at the time it's being called.

  In this version, we introduce a mutable version of our environment backed by an atom.
  The relevant functions like `lookup` and `extend` have to be modified because
  they rely on env's structure (assume that it's a hashmap).
  We also need to modify `evaluate`, in particular how `let` and `letrec` are interpreted;
  these special forms wraps immutable env (a hashmap) in a mutable reference (atom)
  and pass it further to make sure the bindings' values and bodies are evaluated
  using this mutable env."
  (:require [ch01-evaluator-final :as e]))


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

;; ... evlis must use the new evaluate too
(defn evlis [exps env]
  (if (list? exps)
    (map #(evaluate % env) exps)
    ()))


;;; MUTABLE ENVIRONMENT
;;; -------------------
;;; To fix the problem mentioned above,
;;; let's introduce a mutable environment (see also the ns docstring)
;;; ------------------------------------------------------------------

;; To be able to mutate it, we must use an _identity_ instead of a simple value
;; Clojure has several "identities" (reference types), including
;; - vars
;; - atoms
;; - refs
;; - agents
;; refs and agents are not useful for us at all
;; we want atoms or vars and since environment is a "local value" passed around as a parameter,
;; we don't really need a var (although we could use `with-local-vars` but that looks more complicated)
;; => use an atom

;; This atom, will be created _when_ we need it, that is inside `evaluate`,
;; when interpreting `let` or `letrec`.
;; We'll simply introduce `*mutable-env?*` variable - when set to true,
;; the ananonymous function created by `make-function` can assume `env` is an atom,
;; and will have to dereference it.

;; don't use this because we don't need it!
;; (def ^:dynamic *mutable-env?* false)

;; once more, we need to modify `lookup` to check if the env is mutable or not
(defn- mutable-env? [env]
  (instance? clojure.lang.Atom env))

(defn- mutable-env [env]
  (if (mutable-env? env)
    env
    (atom env)))

(defn lookup [id env]
  (if-let [[_k v] (find (cond-> env (mutable-env? env) deref) id)]
    (if (= v the-uninitialized-marker)
      (e/wrong "Uninitialized binding" id {:env env})
      v)
    (e/wrong "No such binding" id)))

;; ... and we also need to modify `extend` which is the other operation working
;; with the envrionment structure directly
(defn extend [env variables values]
  ;; we do not yet support special variables like `& args` capturing all the remaining values
  (let [mutable? (mutable-env? env)
        env-val (cond-> env mutable? deref)
        updated-env (if (= (count variables) (count values))
                      (into env-val (zipmap variables values))
                      (e/wrong "The number of variables does not match the number of values"
                               {:var-count (count variables) :val-count (count values)}
                               {:env env-val :variables variables :values values}))]
    (if mutable?
      (reset! env updated-env) ; mutate the environment!
      updated-env)))

;; ... also need to update  `make-function` because it uses `extend`
(defn make-function [variables body env]
  (fn [values]
    ;; use `env`, not `env-global` here:
    (eprogn body (extend env variables values))))


;; ... and maybe also `update!`


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
      ;; CHANGE: using our new `make-function` implementation
      lambda (make-function (second exp) (nnext exp) env)
      ;; CHANGEs: let and letrec will bind `*mutable-env?*` to true
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                ;; CHANGE: use mutable env to allow redefinition of formerly unitialized variables
                mut-env (mutable-env env)
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) mut-env)))
                             bindings)]
            ;; CHANGE: use custom `extend`
            (eprogn body (extend mut-env variables values)))
      ;; CHANGE: let's implement letrec!
      letrec (let [body (nnext exp)
                   bindings (second exp)
                   ;; first add variables with uninitialized values
                   variables (mapv first bindings)
                   ;; CHANGE: use custome `extend`
                   new-env (extend env variables (mapv (constantly the-uninitialized-marker) bindings))
                   ;; CHANGE: use mutable env to allow redefinition of formerly unitialized variables
                   mut-env (mutable-env new-env)
                   ;; then update variables to their proper values
                   values (mapv (fn [[_fn-name fn-def :as _binding]]
                                  (evaluate fn-def mut-env))
                                bindings)]
               ;; CHANGE: use custom `extend`
               (eprogn body (extend mut-env variables values)))

      ;; it's not a special form, just ordinary function => call it!
      (e/invoke (evaluate (first exp) env)
                ;; CHANGE: use own `evlis`
                (evlis (rest exp) env)))))

;; define minimal enviroinment used to invoke our interpreter
;; to avoid problems with `e/global-env`, in particular when using `letrec`
;; (because it would eventually use the old functions from `ch01-evaluator-final` ns)
(def minimal-env
  {'= #(apply = %)
   '- #(apply - %)
   '+ #(apply + %)
   'list #(apply list %)})

;; Now try our example again
(evaluate '(let (local-even? local-odd?)
             (let ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                   (local-odd? (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
               (local-even? 4)))
          minimal-env)
;; YES!
;; => t


;; more complex `let` example:
(assert
 (= '(t f f t)
    (evaluate '(let (local-even? local-odd?)
                 (let ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                       (local-odd? (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                   (list (local-even? 4)
                         (local-odd? 4)
                         (local-even? 5)
                         (local-odd? 5))))
              minimal-env)))

;; And some `letrec` to get rid of the nested let-s (p. 62)
(assert
 (= '(t t)
    (evaluate '(letrec ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                        (local-odd? (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                       (list (local-even? 6)
                             (local-odd? 7)))
              minimal-env)))

(assert
 (= '(t f f t)
    (evaluate '(letrec ((local-even? (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                        (local-odd? (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                       (let ((x (+ 2 3)))
                         (list (local-even? 4)
                               (local-odd? (+ 2 2))
                               (local-even? x)
                               (local-odd? x))))
              minimal-env)))
;; => t



