(ns aoc2021.day20.core
  (:require [clojure.string :as string]
            [clojure.core.matrix :as matrix]))

(defn split-input [input]
  (let [[image-enhancement-algorithm image] (split-with (comp not empty?) input)]
    [(first image-enhancement-algorithm) (drop 1 image)]))

(defn parse-input [[algorithm image]]
  {:algorithm (mapv {\# 1 \. 0} algorithm)
   :image (matrix/emap {\# 1 \. 0} (map seq image))})

(defn parse [path]
  (->> path
       slurp
       string/split-lines
       split-input
       parse-input))

(def input1
  (parse "src/aoc2021/day20/input1.txt"))

(def input-test
  (parse "src/aoc2021/day20/input-test.txt"))

(defn get-neighboring-coords [[x y]]
  (for [a [-1 0 1], b [-1 0 1]]
    [(+ b x) (+ a y)]))

(defn subimage->binary [sub-image]
  (->> sub-image
       (apply str)
       (str "2r")
       read-string))

(defn select-subimage [infinite-val image point]
  (map (fn [[x y]] (get-in image [y x] infinite-val))
       (get-neighboring-coords point)))

(defn enhance [algorithm {:keys [infinite-val image]}]
  {:infinite-val (if (zero? infinite-val) 1 0)
   :image (mapv (fn [yi]
                  (mapv (fn [xi]
                          (->> [xi yi]
                               (select-subimage infinite-val image)
                               subimage->binary
                               (nth algorithm)))
                        (range -1 (inc (count image)))))
                (range -1 (inc (count image))))})

(defn part1 [input]
  (->> {:infinite-val 0 :image (:image input)}
       (iterate (partial enhance (:algorithm input)))
       (drop 2)
       first
       :image
       matrix/esum))

(defn part2 [input]
  (->> {:infinite-val 0 :image (:image input)}
       (iterate (partial enhance (:algorithm input)))
       (drop 50)
       first
       :image
       matrix/esum))

(comment
  (:image input-test)
  ;; => {[4 3] 0, [2 2] 0, [0 0] 1, [1 0] 0, [2 3] 1, [3 3] 0, [1 1] 0, [3 4] 1, [4 2] 1, [3 0] 1, [4 1] 0, [1 4] 0, [1 3] 0, [0 3] 0, [2 4] 1, [0 2] 1, [2 0] 0, [0 4] 0, [3 1] 0, [2 1] 0, [4 4] 1, [1 2] 1, [3 2] 0, [0 1] 1, [4 0] 0}

  ((:algorithm input-test) 34)
  ;; => 1

  (part1 input-test)
  ;; => 31

  (part1 input1)
  ;; => 5687

  (part2 input-test)
  ;; => 5097

  (part2 input1)
  ;; => 18723
  )
