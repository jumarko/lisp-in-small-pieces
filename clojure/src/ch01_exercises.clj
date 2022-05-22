(ns ch01-exercises
  "Chapter 1 exercises listed on the p.28-29."
  (:require
   [ch01-evaluator :as e]
   [clojure.edn :as edn]
   [clojure.string :as str]))


;;; Exercise 1.1: Modify the function `evaluate` so it becomes a tracer.
;;; - all function calls should display their arguments and results.
;;; You can imagine extending such a rudimentary tracer
;;; to a step-by-step debugger that could modify the running program.

;; Solution:
;; We probably need to redefine make-function to make it work;
;; and similarly to `ch01-evaluator.d-evaluate`,
;; you also need to redefine a bunch of other functions
(declare trace-evaluate)

(defn trace-eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; CHANGE:
          fst-val (trace-evaluate fst env)]
      (if (list? rst)
        ;; CHANGE:
        (trace-eprogn rst env)
        fst-val))
    ()))

(defn trace-make-function [variables body env]
  (fn [values]
    (prn "fn args: " values)
    (let [result (trace-eprogn body (e/extend env variables values))]
      (prn "fn ret: " result)
      result)))

(defn trace-evlis [exps env]
  (if (list? exps)
    ;; CHANGE:
    (map #(trace-evaluate % env) exps)
    ()))

;; use `trace-make-function` inside `trace-evaluate`
(defn trace-evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (symbol? exp) (e/lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot trace-evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (trace-evaluate (second exp) env)
           (trace-evaluate (nth exp 2) env)
           (trace-evaluate (nth exp 3) env))
      begin (e/eprogn (rest exp) env)
      set! (e/update! (second exp) env (trace-evaluate (nth exp 2) env))
      ;; Special version of  `make-function` implements the tracing
      lambda (trace-make-function (second exp) (nnext exp) env)
      (e/invoke (trace-evaluate (first exp) env)
              (e/evlis (rest exp) env)))))

(assert (= 50
           (trace-evaluate '((lambda (a b) (+ a b))
                             30 20)
                           e/env-global)))
;; It should print this in the REPL:
;;   "fn args: " (30 20)
;;   "fn ret: " 50

(def my-env (assoc e/env-global
                   ;; see ch01-evaluate (env-global definition on the line 308)
                   'list identity))
(assert (= '(1 2)
           (trace-evaluate '(((lambda (a)
                                      (lambda (b) (list a b)))
                              1)
                             2)
                           my-env)))
;; It prints:
;; "fn args: " (1)
;; "fn ret: " #function[ch01-exercises/trace-make-function/fn--14593]
;; "fn args: " (2)
;; "fn ret: " (1 2)

(assert (= '(1 3)
           (trace-evaluate '((lambda (a)
                                     ((lambda (b) (list a b))
                                      (+ 2 a)))
                             1)
                           my-env)))
;; It prints:
;; "fn args: " (1)
;; "fn args: " (3)
;; "fn ret: " (1 3)
;; "fn ret: " (1 3)



(defn trace-make-function
  "A very simple variant of `trace-make-function` which still prints the args and the return value,
  but also stops the execution whenever a function is being called
  and reads the updated environment definition from standard input via `clojure.edn/read-string`.
  This updated environment is then merged into the current execution environment."
  [variables body env]
  (fn [values]
    (prn "fn args: " (zipmap variables values))
    (println "You can redefine the environment however you like - use well-formed Clojure code.")
    (let [extended-env (e/extend env variables values)
          _ (prn "Current env: " extended-env)
          user-input (read-line)
          new-env (when-not (str/blank? user-input) (edn/read-string user-input))
          modified-env (merge extended-env new-env)
          result (trace-eprogn body modified-env)]
      (prn "fn ret: " result)
      result)))

;; Now try to modify the bindings - for example,
;; enter '{a 10}' for the first prompt and just empty input for the second prompt
(comment 
  (trace-evaluate '((lambda (a)
                            ((lambda (b) (list a b))
                             (+ 2 a)))
                    1)
                  e/env-global)
  ;;=> (10 12)
  .)



;;; Ex. 1.2: Eliminate needless recursion in `e/evlis`  when the input list contains only one expression.

;; I used `map` to simplify definition of `e/evlis` before.
;; Let's now reimplement it using recursion
;; note: tail recursion makes it a bit more complicated;
;; as an alternative, see https://github.com/Chouser/lisp-in-small-pieces-clj/blob/main/src/us/chouser/LISP/ch1a.clj#L68-L74
(defn evlis
  ([exps env]
   (evlis [] exps env))
  ([result exps env]
   (println "evlis called with exps:" exps)
   (if-let [s (seq exps)]
     (recur (conj result (e/evaluate (first exps) env))
            (rest s)
            env)
     result)))

;; check that it works:
(assert (= [1 '(2 3) [10 20] '(1 2 3)]
           (evlis '(1
                    (quote (2 3))
                    [10 20]
                    (cons 1 [2 3]))
                  e/env-global)))

;; does it still recur when evaluating only one item?
(evlis '((cons 1 [2 3]))
       e/env-global)
;; evlis called with exps: ((cons 1 [2 3]))
;; evlis called with exps: ()

;; eliminating unnecessary recursion
(defn evlis
  ([exps env] (evlis [] exps env))
  ([result exps env]
   (println "evlis called with exps:" exps)
   (let [s (seq exps)]
     (cond
       (not s) result

       (= 1 (bounded-count 2 s)) (conj result (e/evaluate (first exps) env))

       :else (recur (conj result (e/evaluate (first exps) env))
                    (rest s)
                    env)))))
(assert (= ['(1 2 3)]
           (evlis '((cons 1 [2 3]))
                  e/env-global)))
;; evlis called with exps: ((cons 1 [2 3]))



;;; Ex. 1.5: modify `<` to return a boolean value of the language being defined,
;;; instead of the implementation language (Clojure)
;;; => Abandon this exercise because it doesn't make much sense.
;;; When you look at the answers at the book, they simply define
;;; macro `defpredicate` used to define boolean functions.

;; so I want to return `t` or `f` but how do I do that?
(e/evaluate '(< 1 2) e/env-global)
;; => true

;; Fix it: TODO: how?
;; ... this only returns the symbol `t`
(e/evaluate '(< 1 2) (assoc e/env-global
                            '< (fn [[a b]] (if (< a b)
                                           't
                                           'f))))
;; => t

;; ... so maybe I need to tweak `invoke`?
(declare t-evaluate)
(defn t-invoke [f args env]
  (if (fn? f)
    ;; CHANGE: call evaluate on the result of the function application
    (t-evaluate (f args) env)
    (e/wrong "Not a function" f args)))

(defn t-evaluate [exp env]
  (if (e/atom? exp)
    (cond
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
      if (if (t-evaluate (second exp) env)
           (t-evaluate (nth exp 2) env)
           (t-evaluate (nth exp 3) env))
      begin (e/eprogn (rest exp) env)
      set! (e/update! (second exp) env (t-evaluate (nth exp 2) env))
      lambda (e/make-function (second exp) (nnext exp) env)
      ;; it's not a special form, just ordinary function => call it!
      (t-invoke (t-evaluate (first exp) env)
              (e/evlis (rest exp) env)
              env))))

(t-evaluate '(< 1 2) (assoc e/env-global
                            '< (fn [[a b]] (if (< a b)
                                             't
                                             'f))))
;; => true

;; sanity checks
(t-evaluate '(quote (1 2 3 )) e/env-global)
;; => (1 2 3)
;; TODO:
#_(t-evaluate '(cons 1 [2 3]) e/env-global)
;; Cannot evaluate - unknown atomic expression?
;; {:expression (1 2 3), :args nil}

;; ... would I have to update definitions of `cons` and some other primitives too?




;;; Ex. 1.6 Define function `list`

;; without proper definition, this doesn't work yet
;; (I have already defined `list` above, but only for `my-env`)
#_(e/evaluate '(((lambda (a)
                       (lambda (b) (list a b)))
               1)
              2)
            e/env-global)
;; No such binding
;; {:expression list, :args nil}

;; Define `list` and try again.
;; It looks really simple - just use `list`?
;; - but how do you I define proper arity? (it can be 0 or more arguments)
;; - and should I really return the list implementation offered by Clojure (the hosting language)?

;; here i cheat and say that you can use `list` only for 2 elements:
(e/defprimitive list list 2)
(assert (= '(1 2)
           (e/evaluate '(((lambda (a)
                                  (lambda (b) (list a b)))
                          1)
                         2)
                       e/env-global)))

;; ... but for other number of args you still get the error:
#_(e/evaluate '(list 1 2 3) e/env-global)
;; Incorrect ~arity
;; {:expression [#function[clojure.lang.PersistentList/Primordial] (1 2 3)],
;;  :extra-info ({:expected-arity 2, :actual-arity 3})}

;; ... so do I have to redefine `defprimitive` to support variadic functions?
;; one idea is to support `arity` arg that is a map with the following keys:
;; - `:min-arity`
;; - `:max-arity`
;; both of them would be optional and checked only if arity is a map
;; if it's an empty map, then it would simply accept any number of arguments

(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(e/definitial
     ~name
     (fn [~'values]
       (let [val-count# (count ~'values)]
         (if (or (and (nat-int? ~arity) (= ~arity val-count#))
                 (and (map? ~arity)
                      (<= (:min-arity ~arity -1) val-count#)
                      (>= (:max-arity ~arity Long/MAX_VALUE) val-count#)))
           (apply ~f ~'values)
           (e/wrong "Incorrect ~arity" [~f ~'values] {:expected-arity ~arity :actual-arity val-count#}))))))

(defprimitive list list {:min-arity 0})


(assert (= '(1 2 3)
           (e/evaluate '(list 1 2 3) e/env-global)))
(type (e/evaluate '(list 1 2 3) e/env-global))
;; => clojure.lang.PersistentList

;; Note: alternatively, I could skip `defprimitive` and use `definitial` to define `list`.
;; This is how they did it in the book's answer.
(comment
  ;; Notice they don't use `list`'s implementation in Scheme either - they simply use anonymous
  ;; function to return the input args as they are. This is better approach!
  (e/definitial list (fn [& values] values)))

;; so use an anonymous function instead of `list` directly
(defprimitive list (fn [& values] values) {:min-arity 0})
(assert (= '(1 2 3)
           (e/evaluate '(list 1 2 3) e/env-global)))
;; notice how type is now clojure.lang.Cons instead of clojure.lang.PersistentList
(type (e/evaluate '(list 1 2 3) e/env-global))
;; => clojure.lang.Cons

(assert (= ()
           (e/evaluate '(list) e/env-global)))
(assert (= '(1)
           (e/evaluate '(list 1) e/env-global)))
(assert (= '(1 2 3 4 5 6 7 8 9 10)
           (e/evaluate '(list 1 2 3 4 5 6 7 8 9 10) e/env-global)))

;;; Ex. 1.8 Define `apply`



;;; Ex. 1.9 Define function `end` so you can exit cleanly from the interpreter (REPL) loop


