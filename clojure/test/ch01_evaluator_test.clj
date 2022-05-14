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
    (is (= [1 2 3 4 5 6 7 8 9 10] ; just check that the last value is returned - it's harder to check the side effects
           (evaluate '(begin (inc 1) (println "ahoj") (map inc (range 10)))
                     {})))))
