(ns ch02-recursion-final
  "Same as `ch02-recursion-with-mutable-env` but uses mutable environment for everything,
  since there's no point in using it partially (it makes it hard to reason about when it is in what state).

  Again, mutable environment is just an atom passed around.
  We still strive for immutability by prefering `extend` over `update!`.
  `extend` creates a new copy (atom) of the env while `update!` mutates the atom.
  `update!` is so far only needed to implement mutual recursion via `let` and `letrec`."
  (:require [ch01-evaluator-final :as e]))

;;; copy all the required definitions from chapter 1,
;;; in particular update! and invoke!
;;; - also use `update!` as appropriate (check `extend` calls)

;; define minimal environment used to invoke our interpreter
;; to avoid problems with `e/global-env`, in particular when using `letrec`
;; (because it would eventually use the old functions from `ch01-evaluator-final` ns)
(def minimal-env
  (atom {'= #(apply = %)
         '- #(apply - %)
         '+ #(apply + %)
         'list #(apply list %)}))

(def ^:private the-uninitialized-marker `non-initialized)

(declare evaluate)

(defn eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; CHANGE: use our new evaluate function
          fst-val (evaluate fst env)]
      (if (list? rst)
        (eprogn rst env)
        fst-val))
    ()))

;; enhance `lookup` to make sure we fail fast when somebody tries to use an uninitialized value
(defn lookup [id env]
  (if-let [[_k v] (find @env id)]
    ;; CHANGE: check for unitialized binding
    (if (= v the-uninitialized-marker)
      (e/wrong "Uninitialized binding" id {:env env})
      v)
    (e/wrong "No such binding" id)))

(defn evlis [exps env]
  (if (list? exps)
    ;; CHANGE: use our new evaluate function
    (map #(evaluate % env) exps)
    ()))

;; - invoke is actually the same as in ch1 but it's here for completeness
(defn invoke [f args]
  (if (fn? f)
    (f args)
    (e/wrong "Not a function" f {:args args})))

(defn- mutable-env? [env]
  (and (instance? clojure.lang.Atom env)
       (map? @env)))

(defn update! [id env value]
  (if (mutable-env? env)
    ;; CHANGE: update! needs to swap atom's value instead of simple assoc
    (doto env (swap! assoc id value))
    (e/wrong "Can't understand environment" env {:id id
                                                 :env env
                                                 :value value})))

;; ... modify `extend` which is the other operation working
(defn extend [env variables values]
  (let [env-val @env
        updated-env (if (= (count variables) (count values))
                      (into env-val (zipmap variables values))
                      (e/wrong "The number of variables does not match the number of values"
                               {:var-count (count variables) :val-count (count values)}
                               {:env env-val :variables variables :values values}))]
    ;; we return new atom because `extend` is actually immutable in the book too ?
    (atom updated-env)))

;; ... also need to update  `make-function` because it uses `extend`
(defn make-function [variables body env]
  (fn [values]
    (eprogn body (extend env variables values))))

(defn evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (update! (second exp) env (evaluate (nth exp 2) env))
      ;; CHANGE: using our new `make-function` implementation
      lambda (make-function (second exp) (nnext exp) env)
      ;; Implement `let` as a special form -> add it to our interpreter
      ;; This version supports the following form `(let ( variable ...))`
      ;; where the variables can be just symbols in which case they are uninitialized
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                ;; CHANGE: use mutable env to allow redefinition of formerly unitialized variables
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) env)))
                             bindings)]
            ;; TODO CHANGE: here I think, I need to use update! 
            (eprogn body (extend env variables values)))
      ;; CHANGE: let's implement letrec!
      letrec (let [body (nnext exp)
                   bindings (second exp)
                   ;; first add variables with uninitialized values
                   variables (mapv first bindings)
                   ;; CHANGE: use custom `extend`
                   new-env (extend env variables (mapv (constantly the-uninitialized-marker) bindings))]
               ;; Side effect: update mutable environment
               (mapv (fn [[fn-name fn-def :as _binding]]
                       (update! fn-name
                                new-env
                                (evaluate fn-def new-env)))
                     bindings)
               ;; CHANGE: use custom `extend`
               (eprogn body new-env))

      ;; it's not a special form, just ordinary function => call it!
      (e/invoke (evaluate (first exp) env)
                ;; CHANGE: use own `evlis`
                (evlis (rest exp) env)))))

;; check that our `let` forms work properly with nested lexical scopes
;; because we introduced mutable environments
(assert (= 1110
           (evaluate '(let ((a 10))
                        (+ a
                           (let ((a 100))
                             (+ a (let ((a 1000))
                                    a)))))
                     minimal-env)))
;; => 1110

;; nested `letrec` with duplicate names should still work
(assert (= 211
           (evaluate '(letrec ((inc (lambda (n) (+ n 1))))
                              (+ (inc x)
                                 (letrec ((inc (lambda (n) (+ n 10))))
                                         (inc x))))

                     (atom (assoc @minimal-env 'x 100)))))
;; should correspond to this clojure code:
(let [x 100]
  (letfn [(myinc [n] (+ n 1))]
    (+ (myinc x)
       (letfn [(myinc [n] (+ n 10))]
         (myinc x)))))
;; => 211

;; make sure no bindings are left after the block is executed
(assert (= "No such binding"
           (try (evaluate '(+ (letrec ((inc (lambda (n) (+ n 1))))
                                      0)
                              (inc 1))
                          (atom (assoc @minimal-env 'x 100)))
                (catch Exception e (ex-message e)))))


;; In the book p. 61 they show this example of `let` used to implement mutual recursion.
;; However, this doesn't work for me.
;; I believe this is the same problem as with immutable letrec implementation.
;; If I create a new environment (atom) inside `let` fragment in `evaluate`,
;; I simply cannot reset the update the original env
(comment
  ;; DOES NOT WORK!
  (assert
   (= '(t f f t)
      (evaluate '(let (local-even? local-odd?)
                   (let ((tmp1 (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                         (tmp2 (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                           ;; here's where the mutation is supposed to happen to support mutual recursion
                           ;; BUT: it doesn't work because the environment where the bindings of the above (inner) let
                           ;; are evaluated is not the one in which the _body_ is evaluated - when `let` special form
                           ;; implementation is unchanged and uses `extend` instead of `update!` like this:
                           #_(let [body (nnext exp)
                                   bindings (second exp)
                                   variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                                   bindings)
                                   values (mapv (fn [binding] (if (symbol? binding)
                                                                the-uninitialized-marker
                                                                ;; HERE! values are evaluated in the parent/outer let's env
                                                                (evaluate (second binding) env)))
                                                bindings)]
                               ;; ... BUT the body is evaluated in a new env produced by `extend`
                               (eprogn body (extend env variables values)))
                           ;; ... AND here we mutate the outer let's env, not the inner let's env
                     (set! local-even? tmp1)
                     (set! local-odd? tmp2)
                     (list (local-even? 4)
                           (local-odd? 4)
                           (local-even? 5)
                           (local-odd? 5))))
                minimal-env)))
      .)

;; the above means we had to implement `let` with `update!`, not `extend`
(defn evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (update! (second exp) env (evaluate (nth exp 2) env))
      lambda (make-function (second exp) (nnext exp) env)
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                new-env (atom @env)
                ;; CHANGE: bindings are already evaluated using a new instance of the env
                ;; to make sure the unitialized bindings can be updated (mutated) later
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) new-env)))
                             bindings)]
            ;; CHANGE: use `update!` (similar to `letrec`) instead of `extend`
            (mapv (fn [var val] (update! var new-env val))
                  variables values)
            (eprogn body new-env))
      letrec (let [body (nnext exp)
                   bindings (second exp)
                   ;; first add variables with uninitialized values
                   variables (mapv first bindings)
                   new-env (extend env variables (mapv (constantly the-uninitialized-marker) bindings))]
               ;; Side effect: update mutable environment
               (mapv (fn [[fn-name fn-def :as _binding]]
                       (update! fn-name
                                new-env
                                (evaluate fn-def new-env)))
                     bindings)
               (eprogn body new-env))

      ;; it's not a special form, just ordinary function => call it!
      (e/invoke (evaluate (first exp) env)
                (evlis (rest exp) env)))))

(assert
 (= '(t f f t)
    (evaluate '(let (local-even? local-odd?)
                 (let ((tmp1 (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                       (tmp2 (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                   ;; here's where the mutation happens to support mutual recursion
                   ;; and now it works because we use mutable environment already when evaluating bindings
                   (set! local-even? tmp1)
                   (set! local-odd? tmp2)
                   (list (local-even? 4)
                         (local-odd? 4)
                         (local-even? 5)
                         (local-odd? 5))))
              minimal-env)))

;; And some `letrec` to get rid of the nested let-s (p. 62)
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

