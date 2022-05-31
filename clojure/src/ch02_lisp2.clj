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
    (f-eprogn body (extend env variables values) fenv)))

;; ... and finally we can implement `evaluate-application`
(defn evaluate-application [f args env fenv]
  (cond
    ;; function symbol -> invoke it right away
    (symbol? f)
    (e/invoke f args)

    ;; lambda forms are also supported - notice they don't produce a function object (via `f-make-function`)
    ;; but instead are evaluated directly
    (and (list? f) (= 'lambda (first f)))
    ;; this is the same thing as body of the function produced by `f-make-function`
    ;; - using `(nnext f)` to skip lambda symbol and its arglist
    ;; - using `(second f)` to get the arg list, that is the list of symbols that should be bound to their values (`args`)
    (f-eprogn (nnext f) (extend env (second f) args) fenv)

    :else (e/wrong "Incorrect functional term" f {:f f :args args :fenv fenv :env env})))

;; now compare `evaluate` and `f-evaluate`
;; - without special support, with `f-evaluate` you shouldn't be able to use more complicated forms in a function position
;; (this example using `(if condition + *)` comes from p. 37)
(assert (= 7 (e/evaluate '((if condition + *) 3 4) (assoc e/env-global 'condition true))))
(assert (= 12 (e/evaluate '((if condition + *) 3 4) (assoc e/env-global 'condition false))))

;; f-evaluate doesn't know how to evaluate non-function forms in the function position
(try
  (f-evaluate '((if condition + *) 3 4)
              (assoc e/env-global 'condition true)
              {})
  (assert false "f-evaluate should fail when called with a non-function form in the function position")
  (catch Exception e
    (assert (= "Incorrect functional term" (ex-message e)))))


