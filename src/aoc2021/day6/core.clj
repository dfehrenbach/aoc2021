(ns aoc2021.day6.core
  (:require [clojure.string :as string]))

(defn parse-input [input]
  (mapv read-string (string/split input #",")))

(def input1
  (->> "src/aoc2021/day6/input1.txt"
       slurp
       string/split-lines
       first
       parse-input))

(def input-test [3 4 3 1 2])

(defn create-fish-map [fish]
  (merge
   {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0, 8 0}
   (frequencies fish)))

(defn progress-day [fish-map]
  (let [spawning-fish (get fish-map 0)]
    {0 (get fish-map 1)
     1 (get fish-map 2)
     2 (get fish-map 3)
     3 (get fish-map 4)
     4 (get fish-map 5)
     5 (get fish-map 6)
     6 (+ (get fish-map 7) spawning-fish)
     7 (get fish-map 8)
     8 spawning-fish}))

(defn both-parts [input x]
  (reduce + (vals (first (drop x (iterate progress-day (create-fish-map input)))))))


(comment
  input1
  (both-parts input-test 18)
  ;; => 26
  (both-parts input-test 80)
  ;; => 5934
  (both-parts input1 80)
  ;; => 349549

  (both-parts input-test 256)
  ;; => 26984457539

  (both-parts input1 256)
  ;; => 1589590444365
  )
