(ns aoc2021.day7.core
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn parse-input [input]
  (mapv read-string (string/split input #",")))

(def input1
  (->> "src/aoc2021/day7/input1.txt"
       slurp
       string/split-lines
       first
       parse-input))

(def input-test [16 1 2 0 4 2 7 1 2 14])

(defn calculate-fuel1 [crabs x]
  (reduce + (map #(math/abs (- % x)) crabs)))

(defn part1 [crabs]
  (apply min (map (partial calculate-fuel1 crabs)
                  (range 0 (inc (apply max crabs))))))


(defn triangular-number [n]
  (/ (* n (inc n)) 2))

(defn calculate-fuel2 [crabs x]
  (reduce + (map #(triangular-number (math/abs (- % x))) crabs)))

(defn part2 [crabs]
  (apply min (map (partial calculate-fuel2 crabs)
                  (range 0 (inc (apply max crabs))))))



(comment
  (int (/ (reduce + input-test) (count input-test)))
  ;; every increment costs 1 less fuel for each value greater, and 1 more fuel for each value lesser
  ;; every decrement costs 1 more fuel for each value greater, and 1 less fuel for each value lesser

  (calculate-fuel1 input-test 2)

  (part1 input-test)
  ;; => 37

  (part1 input1)
  ;; => 337488

  (part2 input-test)
  ;; => 168

  (part2 input1)
  ;; => 89647695

  input1)
