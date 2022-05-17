(ns ch01-evaluator-test
  (:require
   [ch01-evaluator :as evaluator :refer [evaluate]]
   [clojure.test :refer [are deftest is testing]]))


(deftest atom-test
  (testing "atomic values"
    ;; while keywords are not really supported by our language's evaluate function we consider them atomic
    (let [atoms [1 "str" false [1 2 3] :atomic]]
      (is (= (repeat (count atoms) true)
             (mapv evaluator/atom? atoms)))))
  (testing "forms"
    (are [form] (false? (evaluator/atom? form))
      '(1 2 3)
      '(inc 1)
      '(begin (inc 1) (println "ahoj") (map inc (range 10))))))

(deftest evaluate-test
  (testing "evaluate a sequence"
    (is (= '(1 2 3) ; just check that the last value is returned - it's harder to check the "side effects" (that is `(+ 1 2)`)
           ;; functions `+` and `list` are the only ones that are defined as of 1.6 (p. 19)
           (evaluate '(begin (+ 1 2) (list 1 2 3) )
                     evaluator/env-global)))))
