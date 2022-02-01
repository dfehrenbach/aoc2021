(ns aoc2021.day12.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [input-line]
  (let [[[_ a b]] (re-seq #"([a-zA-Z]+)-([a-zA-Z]+)" input-line)]
    [a b]))

(defn build-map [m [a b]]
  (let [combine (fn [m [a b]]
                  (if (m a)
                    (-> m (update a conj b))
                    (-> m (assoc a [b]))))]
    (combine (combine m [a b]) [b a])))

(defn read-file [filepath]
  (->> filepath
       slurp
       string/split-lines
       (map parse-line)
       (reduce build-map {})))

(def input1
  (read-file "src/aoc2021/day12/input1.txt"))

(def input-test1
  (read-file "src/aoc2021/day12/input-test1.txt"))

(def input-test2
  (read-file "src/aoc2021/day12/input-test2.txt"))

(def input-test3
  (read-file "src/aoc2021/day12/input-test3.txt"))

(defn is-lower? [s]
  (= s (string/lower-case s)))

(defn path-find [cave-system path visited-small-caves current]
  (if (= "end" current) {:path (conj path current)}
      (let [valid-branches (remove visited-small-caves (cave-system current))]
        (for [branch valid-branches]
          (path-find cave-system
                     (conj path current)
                     (if (is-lower? current)
                       (conj visited-small-caves current)
                       visited-small-caves)
                     branch)))))

(defn part1 [input]
  (count (flatten (path-find input [] #{} "start"))))

(defn remove-small-caves [cave-system visited-small-caves double-visit current]
  (if (< (first (vals double-visit)) 2)
    (remove (set (remove #{(first (keys double-visit))} visited-small-caves))
            (cave-system current))
    (remove visited-small-caves (cave-system current))))

(defn path-find2 [cave-system path visited-small-caves double-visit current]
  (if (= "end" current) {:path (conj path current)}
      (let [valid-branches (remove-small-caves cave-system visited-small-caves double-visit current)]
        (for [branch valid-branches]
          (path-find2 cave-system
                      (conj path current)
                      (if (is-lower? current)
                        (conj visited-small-caves current)
                        visited-small-caves)
                      (if (= current (first (keys double-visit)))
                        (update double-visit current inc)
                        double-visit)
                      branch)))))

(defn part2 [input]
  (count (apply set/union
                (for [smol-cave (remove #{"start" "end"} (filter is-lower? (keys input)))]
                  (set (map :path (flatten (path-find2 input [] #{} {smol-cave 0} "start"))))))))

(comment
  input-test1

  (part1 input-test1)
  ;; => 10

  (part1 input-test2)
  ;; => 19

  (part1 input-test3)
  ;; => 226

  (part1 input1)
  ;; => 4573 YAY

  (count (set (map :path (flatten (path-find2 input-test1 [] #{} {"b" 0} "start")))))
  ;; => 30

  (remove #{"start" "end"} (filter is-lower? (keys input1)))
  ;; => ("b" "c" "d")

  (part2 input-test1)
  ;; => 36

  (part2 input-test2)
  ;; => 103

  (part2 input-test3)
  ;; => 3509

  (part2 input1)
  ;; => 117509 YAY
  )
