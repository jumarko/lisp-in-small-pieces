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

(assert (= '(1 2)
           (trace-evaluate '(((lambda (a)
                                      (lambda (b) (list a b)))
                              1)
                             2)
                           e/env-global)))
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
                           e/env-global)))
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