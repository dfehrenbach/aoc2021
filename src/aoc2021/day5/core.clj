(ns aoc2021.day5.core
  (:require [clojure.string :as string]))

(defn parse-line [input-line]
  (let [[[_ x1 y1 x2 y2]] (re-seq #"(\d+),(\d+) -> (\d+),(\d+)" input-line)]
    {:x1 (read-string x1) :y1 (read-string y1) :x2 (read-string x2) :y2 (read-string y2)}))

(def input1
  (->> "src/aoc2021/day5/input1.txt"
       slurp
       string/split-lines
       (map parse-line)))

(def input-test
  (->> "src/aoc2021/day5/input-test.txt"
       slurp
       string/split-lines
       (map parse-line)))

(defn vertical? [{x1 :x1 x2 :x2}]
  (= x1 x2))
(defn horizontal? [{y1 :y1 y2 :y2}]
  (= y1 y2))


(defn build-points [start stop xfn yfn]
  (conj (take-while #(not= % stop)
                    (iterate (fn [[x y]] (vector (xfn x) (yfn y)))
                             start))
        stop))

(build-points [0 1] [4 5] inc inc)

(defn identify-cardinal-points [{x1 :x1 x2 :x2 y1 :y1 y2 :y2 :as line}]
  (cond
    (horizontal? line)  (if (< x1 x2)
                          (for [x (range x1 (inc x2))]
                            [x y1])
                          (for [x (range x2 (inc x1))]
                            [x y1]))
    (vertical? line)  (if (< y1 y2)
                        (for [y (range y1 (inc y2))]
                          [x1 y])
                        (for [y (range y2 (inc y1))]
                          [x1 y]))
    :else []))

(defn part1 [input]
  (let [filtered-lines (filter #(or (vertical? %) (horizontal? %)) input)
        all-points (mapcat identify-cardinal-points filtered-lines)]
    (count (filter (fn [[_ n]] (<= 2 n)) (frequencies all-points)))))


(defn diagonal-points [{x1 :x1 x2 :x2 y1 :y1 y2 :y2}]
  (let [points-fn (partial build-points [x1 y1] [x2 y2])]
    (condp = [(< x1 x2) (< y1 y2)]
      [true true] (points-fn inc inc)
      [true false] (points-fn inc dec)
      [false true] (points-fn dec inc)
      [false false] (points-fn dec dec))))

(defn identify-diagonal-points [line]
  (if (or (horizontal? line) (vertical? line)) []
      (diagonal-points line)))

(defn part2 [input]
  (let [filtered-lines (filter #(or (vertical? %) (horizontal? %)) input)
        all-points (concat (mapcat identify-cardinal-points filtered-lines)
                           (mapcat identify-diagonal-points input))]
    (count (filter (fn [[_ n]] (<= 2 n)) (frequencies all-points)))))


(comment
  (re-seq #"(\d),(\d) -> (\d),(\d)" (first input-test))

  (for [x (range (:x1 {:x1 0, :y1 9, :x2 5, :y2 9}) (inc (:x2 {:x1 0, :y1 9, :x2 5, :y2 9})))]
    [x (:y1 {:x1 0, :y1 9, :x2 5, :y2 9})])

  (part1 input-test)
  ;; => 5

  (part1 input1)
  ;; => 7297

  (part2 input-test)
  ;; => 12

  (part2 input1)
  ;; => 21038
  )
