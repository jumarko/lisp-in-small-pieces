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
