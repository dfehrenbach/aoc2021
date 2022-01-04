(ns aoc2021.day11.core
  (:require [clojure.string :as string]))


(defn matrix->map [matrix]
  (apply merge (for [y (range (count matrix))
                     x (range (count (first matrix)))]
                 {[x y] (-> matrix (nth y) (nth x))})))


(def input1
  (->> "src/aoc2021/day11/input1.txt"
       slurp
       string/split-lines
       (mapv (fn [line] (mapv read-string (string/split line #""))))
       matrix->map))


(def input-test
  (->> "src/aoc2021/day11/input-test.txt"
       slurp
       string/split-lines
       (mapv (fn [line] (mapv read-string (string/split line #""))))
       matrix->map))


(defn adjacent-coords [[x y :as _point]]
  [[(inc x) (inc y)]
   [(inc x) y]
   [(inc x) (dec y)]
   [x (inc y)]
   [x (dec y)]
   [(dec x) (inc y)]
   [(dec x) y]
   [(dec x) (dec y)]])

(defn find-all-9s [pointmap]
  (->> pointmap
       (filter (fn [[_k v]] (< 9 v)))
       (map first)))

(defn incmap [pointmap]
  (zipmap (keys pointmap)
          (map inc (vals pointmap))))

(defn flash [pointmap [_x _y :as point]]
  (let [adjacent (adjacent-coords point)
        flashed-map (assoc pointmap point -1000)]
    (reduce (fn [pmap point]
              (if (pmap point)
                (update pmap point inc)
                pmap))
            flashed-map
            adjacent)))

(defn flash-step [pointmap]
  (let [nines (find-all-9s pointmap)]
    (reduce flash pointmap nines)))


(defn rec-flashes [pointmap]
  (if (empty? (find-all-9s pointmap)) pointmap
      (recur (flash-step pointmap))))

(defn reset-flashes [pointmap]
  (zipmap (keys pointmap)
          (map (fn [v] (if (neg? v) 0 v)) (vals pointmap))))

(defn record-flashes [pointmap flashn]
  (let [flashes (->> pointmap
                     vals
                     (filter zero?)
                     count)]
    {:pointmap pointmap
     :flashes (+ flashn flashes)}))


(defn step [{pointmap :pointmap flashn :flashes}]
  (-> pointmap
      incmap
      rec-flashes
      reset-flashes
      (record-flashes flashn)))

(defn part1
  ([input]
   (part1 input 10))
  ([input nstep]
   (let [init {:pointmap input :flashes 0}]
     (->> init
          (iterate step)
          (drop nstep)
          first
          :flashes))))

(defn not-yet-all-zeros [{pointmap :pointmap}]
  (not (every? zero? (vals pointmap))))

(defn part2 [input]
  (let [init {:pointmap input :flashes 0}]
    (->> init
         (iterate step)
         (take-while not-yet-all-zeros)
         count)))

(comment

  (map input-test (adjacent-coords [1 2]))
  ;; => (4 6 4 1 7 6 5 2)

  (find-all-9s (incmap input-test))
  ;; => ([5 4] [4 7] [2 9] [4 1] [4 6] [5 7] [4 8] [1 8] [1 7] [6 8] [2 7] [2 0] [9 4])

  (flash-step (incmap input-test))
  ;; => {[8 8] 6, [7 6] 8, [8 7] 4, [9 8] 5, [7 1] 8, [8 9] 3, [4 3] 4, [2 2] 7, [0 0] 6, [3 9] 4, [7 7] 2, [2 8] 5, [1 0] 5, [8 4] 8, [2 3] 5, [2 5] 7, [7 2] 2, [6 7] 2, [7 4] 5, [8 3] 5, [0 6] 3, [3 3] 2, [5 4] 9, [1 1] 8, [6 3] 7, [0 5] 5, [3 4] 8, [7 3] 2, [8 6] 3, [4 2] 6, [7 8] 6, [3 0] 4, [9 0] 4, [6 6] 2, [9 6] 2, [1 9] 3, [5 3] 4, [9 9] 7, [9 3] 7, [4 7] 9, [4 9] 8, [2 9] 9, [6 5] 5, [0 9] 6, [8 0] 3, [4 1] 9, [5 2] 6, [4 6] 9, [1 4] 4, [5 7] 9, [8 2] 8, [1 3] 2, [4 8] 9, [1 5] 2, [1 8] 9, [1 7] 9, [6 4] 6, [8 1] 2, [0 3] 7, [5 1] 6, [6 1] 5, [5 6] 5, [5 8] 5, [8 5] 5, [0 7] 7, [6 8] 9, [5 5] 3, [7 9] 6, [2 7] 9, [5 9] 6, [2 4] 6, [3 6] 7, [9 2] 4, [4 5] 6, [9 1] 2, [9 7] 5, [7 0] 3, [0 2] 6, [6 9] 2, [2 0] 9, [0 4] 7, [3 1] 6, [2 1] 5, [9 5] 6, [3 8] 7, [9 4] 9, [1 6] 2, [4 4] 4, [3 7] 3, [7 5] 7, [2 6] 8, [5 0] 5, [6 2] 7, [6 0] 4, [1 2] 3, [3 5] 8, [0 8] 5, [3 2] 5, [0 1] 3, [4 0] 2}

  (reset-flashes (rec-flashes (incmap input-test)))
  ;; => {[8 8] 6, [7 6] 8, [8 7] 4, [9 8] 5, [7 1] 8, [8 9] 3, [4 3] 4, [2 2] 7, [0 0] 6, [3 9] 4, [7 7] 2, [2 8] 5, [1 0] 5, [8 4] 8, [2 3] 5, [2 5] 7, [7 2] 2, [6 7] 2, [7 4] 5, [8 3] 5, [0 6] 3, [3 3] 2, [5 4] 9, [1 1] 8, [6 3] 7, [0 5] 5, [3 4] 8, [7 3] 2, [8 6] 3, [4 2] 6, [7 8] 6, [3 0] 4, [9 0] 4, [6 6] 2, [9 6] 2, [1 9] 3, [5 3] 4, [9 9] 7, [9 3] 7, [4 7] 9, [4 9] 8, [2 9] 9, [6 5] 5, [0 9] 6, [8 0] 3, [4 1] 9, [5 2] 6, [4 6] 9, [1 4] 4, [5 7] 9, [8 2] 8, [1 3] 2, [4 8] 9, [1 5] 2, [1 8] 9, [1 7] 9, [6 4] 6, [8 1] 2, [0 3] 7, [5 1] 6, [6 1] 5, [5 6] 5, [5 8] 5, [8 5] 5, [0 7] 7, [6 8] 9, [5 5] 3, [7 9] 6, [2 7] 9, [5 9] 6, [2 4] 6, [3 6] 7, [9 2] 4, [4 5] 6, [9 1] 2, [9 7] 5, [7 0] 3, [0 2] 6, [6 9] 2, [2 0] 9, [0 4] 7, [3 1] 6, [2 1] 5, [9 5] 6, [3 8] 7, [9 4] 9, [1 6] 2, [4 4] 4, [3 7] 3, [7 5] 7, [2 6] 8, [5 0] 5, [6 2] 7, [6 0] 4, [1 2] 3, [3 5] 8, [0 8] 5, [3 2] 5, [0 1] 3, [4 0] 2}

  (:flashes (first (drop 10 (iterate step {:pointmap input-test :flashes 0}))))
  ;; => 204

  (part1 input-test 100)
  ;; => 1656

  (part1 input1 100)
  ;; => 1613

  (part2 input-test)
  ;; => 195

  (part2 input1)
  ;; => 510

  0)
