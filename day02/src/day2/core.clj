(ns day2.core
  (:require
   [clojure.core.match :refer [match]]
   [clojure.string :refer [split split-lines]]))

(defn code-to-keyword-part1 [code]
  (get {"A" :rock "B" :paper "C" :scissors "X" :rock "Y" :paper "Z" :scissors} code))

(defn code-to-keyword-part2 [code]
  (get {"A" :rock "B" :paper "C" :scissors "X" :lose "Y" :draw "Z" :win} code))

(defn shape-score [shape]
  (get {:rock 1 :paper 2 :scissors 3} shape))

(defn round-score
  "0 if you lost, 3 if the round was a draw, and 6 if you won"
  [opponent me]
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

(defn sum-scores-part1
  "Take the input as a single string and compute the sum of the scores
   from each round"
  [input]
  (let [lines (split-lines input)
        rounds (map #(map code-to-keyword-part1 (split % #" ")) lines)
        scores (map #(apply score %) rounds)
        result (reduce + scores)]
    result))

(defn shape-for-outcome
  "Given the opponent's shape and the outcome we need, returns the shape that
   will produce the outcome"
  [opponent outcome]
  (if (= outcome :draw)
    opponent
    (match [opponent outcome]
      [:rock :win] :paper
      [:rock :lose] :scissors
      [:paper :win] :scissors
      [:paper :lose] :rock
      [:scissors :win] :rock
      [:scissors :lose] :paper)))

(defn sum-scores-part2
  "Take the input as a single string, find the shapes to match the expected
   outcomes, and compute the sum of the scores from each round"
  [input]
  (let [lines (split-lines input)
        outcomes (map #(map code-to-keyword-part2 (split % #" ")) lines)
        rounds (map #(let [shape (apply shape-for-outcome %)]
                       [(first %) shape])
                    outcomes)
        scores (map #(apply score %) rounds)
        result (reduce + scores)]
    result))

(defn -main []
  (let [input (slurp "day2_input.txt")]
    (println "Part 1:" (sum-scores-part1 input))
    (println "Part 2:" (sum-scores-part2 input))))