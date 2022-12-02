(ns day2.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [day2.core :refer [sum-scores-part1 sum-scores-part2]]))

(deftest test-sum-scores-part1
  (testing "sum-scores-part1 using example input"
    (is (= 15 (sum-scores-part1 "A Y\nB X\nC Z")))))

(deftest test-sum-scores-part2
  (testing "sum-scores-part2 using example input"
    (is (= 12 (sum-scores-part2 "A Y\nB X\nC Z")))))
