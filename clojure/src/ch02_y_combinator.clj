(ns ch02-y-combinator
  "Y-combinator (also called 'Paradoxical combinator') stuff on p. 63-67.

  Resources (see Evernote)
  - [very good!] https://mvanier.livejournal.com/2700.html"
  (:require [clojure.math :as math]))

;;; Y-combinator, is simply a function that can compute a fixed point for another function.
;;; The rule for fixed point says that a 'fixed point' is some x such that x = f(x)
;;; This means that if x = Y(F), then: Y(F) = F(Y(F))


;; That was simple, but here they present a 'transcription' of the definition
;; in Lisp like this:
(let [W (fn [w]
          (fn [f]
            (f ((w w) f))))]
  (W W))
;; ... and that is quite mind-bending ...
;; what does it even mean?

;; if I try to apply it I of course get StackOverflowError O:-)
(comment
 ((let [W (fn [w]
             (fn [f]
               (f ((w w) f))))]
     (W W))
   identity) )

;; ... so in the book they present the following, slightly more complicated version,
;; that works better with call-by-value semantics (like Scheme and Clojure have)
(def fix
  (let [W (fn [w]
            (fn [f]
              (f
               ;; CHANGE: introduce another lambda
               (fn [x]
                 (((w w) f)
                  x)))))]
    (W W)))
(fix identity)
;; => #function[ch02-y-combinator/fn--9275/W--9276/fn--9277/fn--9278]
;; Huh?!


;;; Let's now explore how this stuff works
;;; we'll use `meta-fact` function which is similar to factorial,
;;; but it computes factorial more slowly

;; our good old fact
(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))))

(defn meta-fact [f]
  (fn [n]
    (if (zero? n)
      1
      (* n (f (dec n))))))
(assert (= 120 ((meta-fact fact) 5)))
(assert (= 1 ((meta-fact fact) 1)))


;; by definition, the fixed point of `meta-fact` is function `f` such that:
;; f = (fn [n]
;;       (if (zero? n)
;;         1
;;         (* n (f (dec n)))))
;; this is simply the body of the function

;; ... so what is f?
;; It's our good-old `fact, but It can really be anything else too
;; For example:
(defn another-fact [n]
  (cond
    (< n 1) (- n)
    (= n 1) 1
    :else (* n (another-fact (dec n)))))

;; TODO: for 1 it returns 0. Why?
(assert (= 1 ((meta-fact another-fact) 1)))

(assert
 (= [1 2 6 24 120 720 5040]
    (mapv (meta-fact another-fact)
          [1 2 3 4 5 6 7])))


;;; Digress: https://mvanier.livejournal.com/2700.htm

;; example of calculating fixed point for x = cosine(x)
(def cosines (iterate math/cos 0))
(reduce #(if (= %1 %2) (reduced %1) %2)
        cosines)
;; => 0.7390851332151607

(defn g [f x]
  (f (x x)))


;; here's the version of Y combinator they offer for "strict" languages (like Clojure)
(defn Y [f]
  ((fn [x]
     (f (fn [y] ((x x) y))))
   (fn [x]
     (f (fn [y] ((x x) y))))))
((Y meta-fact)
 5)
;; => 120



