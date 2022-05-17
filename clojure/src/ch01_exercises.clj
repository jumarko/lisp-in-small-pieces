(ns ch01-exercises
  "Chapter 1 exercises listed on the p.28-29."
  (:require
   [ch01-evaluator :as e]))


;;; Exercise 1.1: Modify the function `evaluate` so it becomes a tracer.
;;; - all function calls should display their arguments and results.
;;; You can imagine extending such a rudimentary tracer
;;; to a step-by-step debugger that could modify the running program.

;; Solution:
;; We probably need to redefine make-function to make it work:
(defn make-function [variables body env]
  (fn [values]
    (prn "fn args: " values)
    (let [result (e/eprogn body (e/extend e/env-global variables values))]
      (prn "fn ret: " result)
      result)))

;; use the new `make-function` inside `evaluate`
(defn evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (symbol? exp) (e/lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (e/eprogn (rest exp) env)
      set! (e/update! (second exp) env (evaluate (nth exp 2) env))
      ;; Special version of  `make-function` implements the tracing
      lambda (make-function (second exp) (nnext exp) env)
      (e/invoke (evaluate (first exp) env)
              (e/evlis (rest exp) env)))))

(assert (= 50
           (evaluate '((lambda (a b) (+ a b))
                       30 20)
                     e/env-global)))
;; It should print this in the REPL:
;;   "fn args: " (30 20)
;;   "fn ret: " 50


;; TODO: how about implementing a very simple debugger??
