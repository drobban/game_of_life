(ns game-of-life.core-test
  (:require [clojure.test :refer :all]
            [game-of-life.core :refer :all]))

(deftest a-test
  (testing "slice neighbours"
    (is (= [[4 4] [3 5] [5 5] [3 6]] (neighbours 5 4 world)))
    ;; (is (= (n-neighbours 5 4 world) 5))
    ))
