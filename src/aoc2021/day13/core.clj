(ns aoc2021.day13.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn split-input [input]
  [(take-while (comp not empty?) input) (drop 1 (drop-while (comp not empty?) input))])

(defn parse-point [s]
  (let [[[_ x y]] (re-seq #"(\d+),(\d+)" s)]
    [(read-string x) (read-string y)]))

(defn parse-fold [s]
  (let [[[_ direction n]] (re-seq #"fold along ([yx])=(\d+)" s)]
    [(keyword direction) (read-string n)]))

(defn parse-input [[coords folds]]
  {:coords (set (map parse-point coords))
   :folds (mapv parse-fold folds)})

(defn parse [path]
  (->> path
       slurp
       string/split-lines
       split-input
       parse-input))

(def input1
  (parse "src/aoc2021/day13/input1.txt"))

(def input-test
  (parse "src/aoc2021/day13/input-test.txt"))


(defn foldx [coords foldloc]
  (clojure.set/union
   (->> coords
        (filter #(< foldloc (first %)))
        (map (fn [[x y]] [(Math/abs (- x (* 2 foldloc))) y]))
        set)
   (->> coords
        (remove #(< foldloc (first %)))
        set)))

(defn foldy [coords foldloc]
  (set/union
   (->> coords
        (filter #(< foldloc (second %)))
        (map (fn [[x y]] [x (Math/abs (- y (* 2 foldloc)))]))
        set)
   (->> coords
        (remove #(< foldloc (second %)))
        set)))

(defn part1 [input]
  (let [{coords :coords folds :folds} input
        [direction location] (first folds)]
    (count (if (= :x direction)
             (foldx coords location)
             (foldy coords location)))))

(defn fold-all [{coords :coords folds :folds}]
  (reduce (fn [acc [direction location]]
            (if (= :x direction)
              (foldx acc location)
              (foldy acc location)))
          coords folds))

(defn create-matrix [coords]
  (vec (repeat
        (inc (apply max (map second coords)))
        (vec (repeat (inc (apply max (map first coords))) ".")))))

(defn fill-in-matrix [coords matrix]
  (reduce (fn [m [x y]]
            (assoc-in m [y x] "#"))
          matrix coords))
(defn part2 [input]
  (let [resulting-coords (fold-all input)]
    (->> resulting-coords
         create-matrix
         (fill-in-matrix resulting-coords)
         (map (comp prn string/join)))))

(comment
  (clojure.set/union
   (->> input-test
        :coords
        (filter #(< 7 (second %)))
        (map (fn [[x y]] [x (Math/abs (- y (* 2 7)))]))
        set)
   (->> input-test
        :coords
        (remove #(< 7 (second %)))
        set))
  ;; => #{[4 3] [0 0] [8 4] [3 4] [3 0] [9 0] [4 1] [1 4] [10 2] [6 4] [0 3] [10 4] [2 0] [9 4] [6 2] [6 0] [0 1]}

  (part1 input-test)
  ;; => 17

  (part1 input1)
;; => 735

  (fold-all input-test)
  ;; => #{[4 3] [0 0] [1 0] [3 4] [4 2] [3 0] [4 1] [1 4] [0 3] [2 4] [0 2] [2 0] [0 4] [4 4] [0 1] [4 0]}

  (part2 input-test)
  ;; => (nil nil nil nil nil)
  ;; "#####"
  ;; "#...#"
  ;; "#...#"
  ;; "#...#"
  ;; "#####"

  (part2 input1)
  ;; => (nil nil nil nil nil nil)
  ;; "#..#.####.###..####.#..#..##..#..#.####"
  ;; "#..#.#....#..#....#.#.#..#..#.#..#....#"
  ;; "#..#.###..#..#...#..##...#..#.#..#...#."
  ;; "#..#.#....###...#...#.#..####.#..#..#.."
  ;; "#..#.#....#.#..#....#.#..#..#.#..#.#..."
  ;; ".##..#....#..#.####.#..#.#..#..##..####"
  ;;   UFRZKAUZ
  )
