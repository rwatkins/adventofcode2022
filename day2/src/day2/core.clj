(ns day2.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.match :refer [match]]
            [clojure.string :refer [split split-lines]]))

(s/check-asserts true)
(s/def :day2/shapes #{:rock :paper :scissors})

(defn code-to-keyword [code]
  {:post [(s/assert :day2/shapes %)]}
  (get {"A" :rock
        "B" :paper
        "C" :scissors
        "X" :rock
        "Y" :paper
        "Z" :scissors}
       code))

(defn shape-score [shape]
  {:pre [(s/assert :day2/shapes shape)]}
  (get {:rock 1 :paper 2 :scissors 3} shape))

(defn round-score
  "0 if you lost, 3 if the round was a draw, and 6 if you won"
  [opponent me]
  {:pre [(s/assert :day2/shapes opponent)
         (s/assert :day2/shapes me)]
   :post [(s/assert number? %)]}
  (if (= opponent me) 3
      (match [opponent me]
        [:rock :paper] 6
        [:rock :scissors] 0
        [:paper :rock] 0
        [:paper :scissors] 6
        [:scissors :rock] 6
        [:scissors :paper] 0)))

(defn score [opponent me]
  (+ (shape-score me)
     (round-score opponent me)))

(defn line-to-keywords [s]
  (mapv code-to-keyword (split s #" ")))

(defn sum-scores
  "Take the input as a single string and compute the sum of the scores 
   from each round"
  [input]
  (let [lines (split-lines input)
        rounds (map line-to-keywords lines)
        scores (map #(apply score %) rounds)
        result (reduce + scores)]
    result))

(defn -main []
  (let [input (slurp "day2_input.txt")]
    (println "Part 1:" (sum-scores input))))