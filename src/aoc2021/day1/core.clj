(ns aoc2021.day1.core
  (:require [clojure.string :as string]))

(def input1
  (->> "src/aoc2021/day1/input1.txt"
       slurp
       string/split-lines
       (map read-string)))

(def input-test [199 200 208 210 200 207 240 269 260 263 262])

(defn part1 [input]
  (->> input
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))


(defn part2 [input]
  (->> input
       (partition 3 1)
       (map #(reduce + %))
       (partition 2 1)
       (filter (fn [[a b]] (< a b)))
       count))

(comment
  input1

  (part1 input1)

  (part2 input1))
