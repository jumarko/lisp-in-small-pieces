(ns ch02-recursion-final
  "Same as `ch02-recursion-with-mutable-env` but uses mutable environment for everything,
  since there's no point in using it partially (it makes it hard to reason about when it is in what state)"
  (:require [ch01-evaluator-final :as e]))

;;; TODO: implement it fully - copy all the required definitions from chapter 1,
;;; in particular update! and invoke!
;;; - also use `update!` as appropriate (check `extend` calls)

;; define minimal enviroinment used to invoke our interpreter
;; to avoid problems with `e/global-env`, in particular when using `letrec`
;; (because it would eventually use the old functions from `ch01-evaluator-final` ns)
(def minimal-env
  (atom {'= #(apply = %)
         '- #(apply - %)
         '+ #(apply + %)
         'list #(apply list %)}))

;; Implement `let` as a special form -> add it to our interpreter
;; This version supports the following form `(let ( variable ...))`
;; where the variables can be just symbols in which case they are uninitialized

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
    ;; we return new atom because `extend` is actually immutable in the book too
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
      ;; CHANGEs: let and letrec will bind `*mutable-env?*` to true
      let (let [body (nnext exp)
                bindings (second exp)
                variables (mapv (fn [binding] (if (symbol? binding) binding (first binding)))
                                bindings)
                ;; CHANGE: use mutable env to allow redefinition of formerly unitialized variables
                values (mapv (fn [binding] (if (symbol? binding)
                                             the-uninitialized-marker
                                             (evaluate (second binding) env)))
                             bindings)]
            ;; CHANGE: use custom `extend`
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

;; TODO: doesn't work - why? 
;; `let` example:
(assert
 (= '(t f f t)
    (evaluate '(let (local-even? local-odd?)
                 (let ((tmp1 (lambda (n) (if (= n 0) 't (local-odd? (- n 1)))))
                       (tmp2 (lambda (n) (if (= n 0) 'f (local-even? (- n 1))))))
                   ;; here's where the mutation of the env happens to support mutual recursion
                   ;; this is the reason why we don't have to use `update!`
                   ;; in the let's implementation inside `evaluate`
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

