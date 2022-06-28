(ns ch02-exercises
  (:require
   [ch01-evaluator-final :as e]
   [ch02-lisp2 :as lisp2]
   [clojure.core :as math]))

;;; Ex. 2.1
;;; The following expression written in Common Lisp is equivalent to what in Scheme/Clojure?
(comment 
  (funcall (function funcall) (function funcall) (function cons) 1 2)

  ;; expand it:
  (funcall (function funcall) (function cons) 1 2)
  (funcall (function cons) 1 2)
  (cons 1 2)
  .)


;;; Ex. 2.2
;;; In pseudo-Common Lisp from this chapter, what is the value of this program?
  ;; (defun test (p)
  ;;   (function bar))

  ;; (let ((f (test #f)))
  ;;   (defun bar (x) (cdr x))
  ;;   (funcall f '(1 . 2)))
;;=> should be simply cdr of the input, that is (2)



;;; Ex. 2.3
;;; Incorporate the first two innovations from section 2.3 (p.39) into our interpreter
;;; 1. Integers can be invoked as functions - they are used as list accessors (zero-based indexes)
;;; 2. lists can be used as functions - apply each function to given args (similar to `clojure.core/juxt`)

(lisp2/f-evaluate '(car '(1 2 3)) {} lisp2/fenv-global)
;; => 1

;; 1. Integers invoked as functions
(defn eval-f
  "This is a modified version of `lisp2/evaluate-application`."
  [f args env fenv]
  (cond
    (symbol? f)
    (e/invoke (lisp2/f-lookup f fenv) args)

    (and (list? f) (= 'lambda (first f)))
    (lisp2/f-eprogn (nnext f) (e/extend env (second f) args) fenv)

    ;; CHANGE: use numbers as lookup functions
    ;; numbers serve as lookup functions and expect a single list argument
    (and (number? f) (= 1 (count args)) (list? (first args)))
    (let [coll (first args)]
      (if (neg? f)
        (nthrest coll (math/abs f))
        (nth coll f)))

    :else (e/wrong "Incorrect functional term" f {:f f :args args :fenv fenv :env env})))

;; positive index returns element at given position
;; car should be like using 0 (they say 1 in the book)
(assert
 (= 1
    (lisp2/f-evaluate '(0 '(1 2 3)) {} lisp2/fenv-global eval-f)))

(assert
 (= 'hax
    (lisp2/f-evaluate '(2 '(foo bar hax wok)) {} lisp2/fenv-global eval-f)))

;; negative index returns rest of the elements starting at given position
(assert
 (= '(hax wok)
    (lisp2/f-evaluate '(-2 '(foo bar hax wok)) {} lisp2/fenv-global eval-f)))


;; 2. lists can be used as functions ala `juxt`
(defn eval-f
  "This is a modified version of `lisp2/evaluate-application`."
  [f args env fenv]
  (cond
    (symbol? f)
    (e/invoke (lisp2/f-lookup f fenv) args)

    (and (list? f) (= 'lambda (first f)))
    (lisp2/f-eprogn (nnext f) (e/extend env (second f) args) fenv)

    (and (number? f) (= 1 (count args)) (list? (first args)))
    (let [coll (first args)]
      (if (neg? f)
        (nthrest coll (math/abs f))
        (nth coll f)))

    ;; CHANGE: lists of functions can be applied to the args one by one
    (list? f)
    (map (fn [ff] (eval-f ff args env fenv)) (rest f))

    :else (e/wrong "Incorrect functional term" f {:f f :args args :fenv fenv :env env})))

(assert
 (= '(8 2 15)
    (lisp2/f-evaluate '((list + - *) 5 3)
                      {}
                      lisp2/fenv-global
                      eval-f)))


;;; Ex. 2.4
;;; Improve assoc-de to take a comparer (such as eq?, equal?, etc.) as an argument
