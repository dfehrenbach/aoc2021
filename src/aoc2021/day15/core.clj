(ns aoc2021.day15.core
  (:require [clojure.string :as string]
            [clojure.data.priority-map :as pm]))

;; Djikstra's OR A*

(defn matrix->map [matrix]
  (apply merge (for [y (range (count matrix))
                     x (range (count (first matrix)))]
                 {[x y] (-> matrix (nth y) (nth x))})))

(def input1
  (->> "src/aoc2021/day15/input1.txt"
       slurp
       string/split-lines
       (mapv (fn [line] (mapv read-string (string/split line #""))))
       matrix->map))


(def input-test
  (->> "src/aoc2021/day15/input-test.txt"
       slurp
       string/split-lines
       (mapv (fn [line] (mapv read-string (string/split line #""))))
       matrix->map))

(defn manhatten-distance [node destination]
  (+ (Math/abs (- (first destination) (first node)))
     (Math/abs (- (second destination) (second node)))))

(defn priority-map-init [start finish]
  (pm/priority-map {:pos start :cost 0}
                   (manhatten-distance start finish)))

(defn neighbors [[maxx maxy] [x y]]
  (for [[dx dy] [[0 1] [1 0] [-1 0] [0 -1]]
        :let [x' (+ x dx)
              y' (+ y dy)]
        :when (and (<= 0 x' maxx)
                   (<= 0 y' maxy))]
    [x' y']))

(defn step [finish neighborsfn costfn]
  (fn [{:keys [visited search-space]}]
    (let [[{:keys [pos cost]}] (peek search-space)]
      (if (= pos finish) {:solution cost}
          {:visited (conj visited pos)
           :search-space (into (pop search-space)
                               (for [pos' (neighborsfn pos)
                                     :when (not (visited pos'))
                                     :let [cost' (+ cost (costfn pos'))]]
                                 [{:pos pos' :cost cost'} (+ cost' (manhatten-distance pos' finish))]))}))))

(defn part1 [input]
  (let [start [0 0]
        finish [(apply max (map first (keys input)))
                (apply max (map second (keys input)))]
        priority-map (priority-map-init start finish)
        initial-search-state {:visited #{} :search-space priority-map}
        neighborsfn (partial neighbors finish)
        costfn (partial get input)]
    (->> initial-search-state
         (iterate (step finish neighborsfn costfn))
         (drop-while (complement :solution))
         first
         :solution)))

(defn five-times-costfn [input [x y]]
  (let [[boundx boundy] [(count (set (map first (keys input))))
                         (count (set (map second (keys input))))]
        [x' y'] [(mod x boundx) (mod y boundy)]
        cost' (+ (get input [x' y'])
                 (quot x boundx)
                 (quot y boundy))]
    (inc (mod (dec cost') 9))))

(defn part2 [input]
  (let [start [0 0]
        finish [(dec (* 5 (count (set (map first (keys input))))))
                (dec (* 5 (count (set (map second (keys input))))))]
        priority-map (priority-map-init start finish)
        initial-search-state {:visited #{} :search-space priority-map}
        neighborsfn (partial neighbors finish)
        costfn (partial five-times-costfn input)]
    (->> initial-search-state
         (iterate (step finish neighborsfn costfn))
         (drop-while (complement :solution))
         first
         :solution)))


(comment
  (priority-map-init [0 0] [1 1])
  (part1 input-test)
  ;; => 40

  (part1 input1)
  ;; => 652

  (part2 input-test)
  ;; => 315

  (part2 input1)
  ;; => 2938
  )
