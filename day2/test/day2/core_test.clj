(ns day2.core-test
  (:require [clojure.test :refer :all]
            [day2.core :refer :all]))

(deftest test-sum-scores
  (testing "sum-scores using example input"
    (is (= 15 (sum-scores "A Y\nB X\nC Z")))))
