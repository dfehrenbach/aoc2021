(ns aoc2021.day9.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input1
  (->> "src/aoc2021/day9/input1.txt"
       slurp
       string/split-lines
       (map #(string/split % #""))
       (mapv #(mapv read-string %))))

(def input-test
  (->> "src/aoc2021/day9/input-test.txt"
       slurp
       string/split-lines
       (map #(string/split % #""))
       (mapv #(mapv read-string %))))

(defn get-2d-val [arr [x y]]
  (-> arr (nth y []) (nth x 10)))

(defn get-adjacent-coords [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn low-point? [arr point]
  (let [cur-point (get-2d-val arr point)
        adjacents (map (partial get-2d-val arr) (get-adjacent-coords point))]
    (every? #(< cur-point %) adjacents)))

(defn part1 [input]
  (let [low-point-values (for [y (range 0 (count input))
                               x (range 0 (count (first input)))
                               :when (low-point? input [x y])]
                           (-> input (nth y 10) (nth x 10)))]
    (->> low-point-values
         (map inc)
         (reduce +))))

(defn dfs-basin [arr ipoint]
  (loop [discovered #{}
         search-space #{}
         point ipoint]
    (let [adjacent-coords (get-adjacent-coords point)
          basin-adjacent (filter (fn [[x y]]
                                   (and (< (get-2d-val arr [x y]) 9)
                                        (not (some #{[x y]} discovered))))
                                 adjacent-coords)
          new-search-space (-> (set basin-adjacent)
                               (set/union search-space)
                               (set/difference #{point}))]
      (if (empty? new-search-space) (conj discovered point)
          (recur (conj discovered point)
                 new-search-space
                 (first new-search-space))))))

(defn basin-size [arr point]
  (count (dfs-basin arr point)))

(defn part2 [input]
  (let [low-points (for [y (range 0 (count input))
                         x (range 0 (count (first input)))
                         :when (low-point? input [x y])
                         :when (< (-> input (nth y) (nth x)) 9)]
                     [x y])
        basin-sizes (map (partial basin-size input) low-points)]
    (->> basin-sizes
         sort
         reverse
         (take 3)
         (reduce *))))

(comment
  (for [y (range 0 (count input-test))
        x (range 0 (count (first input-test)))
        :when (low-point? input-test [x y])]
    [x y (-> input-test (nth y 10) (nth x 10))])
  ;; => ([1 0 1] [9 0 0] [2 2 5] [6 4 5])

  (part1 input-test)
  ;; => 15
  (part1 input1)
  ;; => 508

  (dfs-basin input-test [1 0])
  ;; => #{[0 0] [1 0] [0 1]}

  (part2 input-test)
  ;; => 1134
  (part2 input1)
  ;; => 1564640
  )
