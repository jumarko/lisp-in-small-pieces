(ns demo
  "Just a very quick-and-dirty demo for Clojure beginners.")

(comment
  (first [1 2 3])
  ;; => 1

)

(list? [1 2 3])

(sequential? {1 2})

(let [[a b & rst :as all] [1 2 3 4]]
  [a b rst all])
;; => [1 2 (3 4) [1 2 3 4]]

;; => [1 2 (3 4)]


'(do (1 2 3) (inc 1) (+ 1 2))
;; => (do (1 2 3) (inc 1) (+ 1 2))
