(ns aoc2021.day2.core
  (:require [clojure.string :as string]))

(def input1
  (->> "src/aoc2021/day2/input1.txt"
       slurp
       string/split-lines))

(def input-test ["forward 5"
                 "down 5"
                 "forward 8"
                 "up 3"
                 "down 8"
                 "forward 2"])

(defn parse-instruction [s]
  (let [[instruction sval] (string/split s #" ")
        n (read-string sval)]
    [instruction n]))

(defn combine-instructions1 [[x y] [instruction n]]
  (case instruction
    "forward" [(+ x n) y]
    "down" [x (+ y n)]
    "up" [x (- y n)]
    :else (throw "other instruction found")))

(defn part1 [input]
  (->> input
       (map parse-instruction)
       (reduce combine-instructions1 [0 0])
       (reduce *)))

(defn combine-instructions2 [[x y aim] [instruction n]]
  (case instruction
    "forward" [(+ x n) (+ y (* n aim)) aim]
    "down" [x y (+ aim n)]
    "up" [x y (- aim n)]
    :else (throw "other instruction found")))

(defn part2 [input]
  (->> input
       (map parse-instruction)
       (reduce combine-instructions2 [0 0 0])
       ((fn [[x y _]] (* x y)))))

(comment
  (parse-instruction "forward 5")

  (reduce combine-instructions1 [0 0] (map parse-instruction input-test))

  (part1 input-test)
  ;; => 150

  (part1 input1)
  ;; => 1714680

  (reduce combine-instructions2 [0 0 0] (map parse-instruction input-test))

  (part2 input-test)
  ;; => 900

  (part2 input1)
  ;; => 1963088820
  )
