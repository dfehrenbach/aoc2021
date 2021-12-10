(ns aoc2021.day8.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-line [input-line]
  (let [[signals output-vals] (string/split input-line #" \| ")]
    {:signals (string/split signals #" ") :output-vals (string/split output-vals #" ")}))

(def input1
  (->> "src/aoc2021/day8/input1.txt"
       slurp
       string/split-line
       (map parse-line)))

(def input-test
  (->> "src/aoc2021/day8/input-test.txt"
       slurp
       string/split-lines
       (map parse-line)))

(defn part1 [input]
  (->> input
       (mapcat :output-vals input)
       (filter (fn [output-val] (or (= 2 (count output-val))
                                    (= 3 (count output-val))
                                    (= 4 (count output-val))
                                    (= 7 (count output-val)))))
       count))


(def signal->output
  {#{:top-left :top :top-right :bottom-left :bottom-right :bottom} "0"
   #{:top-right :bottom-right} "1"
   #{:top :top-right :middle :bottom-left :bottom} "2"
   #{:top :top-right :middle :bottom-right :bottom} "3"
   #{:top-left :top-right :middle :bottom-right} "4"
   #{:top :top-left :middle :bottom-right :bottom} "5"
   #{:top :top-left :middle :bottom-left :bottom-right :bottom} "6"
   #{:top :top-right :bottom-right} "7"
   #{:top :top-left :top-right :middle :bottom-left :bottom-right :bottom} "8"
   #{:top :top-left :top-right :middle :bottom-right :bottom} "9"})


;; First
(defn bottom-right-seg [signals]
  (let [one-sig (first (filter #(= 2 (count %)) signals))
        notfound (set one-sig)
        signals-without-both-notfound (remove #(empty? (set/difference notfound (set %))) signals)
        six-sig (first (filter #(= 6 (count %)) signals-without-both-notfound))]
    (set/intersection (set six-sig) notfound)))

;; Second
(defn top-right-seg [signals found-signals]
  (let [one-sig (first (filter #(= 2 (count %)) signals))]
    (set/difference (set one-sig) found-signals)))

;; Third
(defn top-signal [signals found-signals]
  (let [seven-sig (first (filter #(= 3 (count %)) signals))]
    (set/difference (set seven-sig) found-signals)))

;; Fourth
(defn top-left-signal [signals found-signals]
  (let [four-sig (first (filter #(= 4 (count %)) signals))
        notfound (set (remove found-signals four-sig))
        signals-without-both-notfound (remove #(empty? (set/difference notfound (set %))) signals)
        zero-sig (first (filter #(= 6 (count %)) signals-without-both-notfound))]
    (set/intersection (set zero-sig) notfound)))

;; Fifth
(defn middle-signal [signals found-signals]
  (let [four-sig (first (filter #(= 4 (count %)) signals))]
    (set/difference (set four-sig) found-signals)))

;; Sixth
(defn bottom-signal [signals found-signals]
  (let [eight-sig (first (filter #(= 7 (count %)) signals))
        notfound (set (remove found-signals eight-sig))
        signals-without-both-notfound (remove #(empty? (set/difference notfound (set %))) signals)
        nine-sig (first (filter #(= 6 (count %)) signals-without-both-notfound))]
    (set/intersection (set nine-sig) notfound)))

(defn bottom-left-signal [signals found-signals]
  (let [eight-sig (first (filter #(= 7 (count %)) signals))]
    (set/difference (set eight-sig) found-signals)))

(defn build-configuration [signals]
  (let [bottom-right (bottom-right-seg signals)
        top-right (top-right-seg signals bottom-right)
        top (top-signal signals (set/union bottom-right top-right))
        top-left (top-left-signal signals (set/union bottom-right top-right top))
        middle (middle-signal signals (set/union bottom-right top-right top top-left))
        bottom (bottom-signal signals (set/union bottom-right top-right top top-left middle))
        bottom-left (bottom-left-signal signals (set/union bottom-right top-right top top-left middle bottom))]
    {(first bottom-right) :bottom-right
     (first top-right) :top-right
     (first top) :top
     (first top-left) :top-left
     (first middle) :middle
     (first bottom) :bottom
     (first bottom-left) :bottom-left}))

(defn output-val->n [configuration output-val]
  (signal->output (set (map configuration output-val))))

(defn solve-line [{signals :signals output-vals :output-vals}]
  (let [configuration (build-configuration signals)]
    (read-string (string/join (drop-while #(= % "0") (map (partial output-val->n configuration) output-vals))))))

(defn part2 [input]
  (reduce + (map solve-line input)))

(comment
  (first input-test)
  (part1 input-test)
  ;; => 26

  (def signals (:signals (first input-test)))
  (def bottom-right (bottom-right-seg signals))
  (def top-right (top-right-seg signals bottom-right))
  (def top (top-signal signals (set/union bottom-right top-right)))
  (def top-left (top-left-signal signals (set/union bottom-right top-right top)))
  (def middle (middle-signal signals (set/union bottom-right top-right top top-left)))
  (def bottom (bottom-signal signals (set/union bottom-right top-right top top-left middle)))
  (def bottom-left (bottom-left-signal signals (set/union bottom-right top-right top top-left middle bottom)))

  (part1 input1)
  ;; => 532

  (part2 input-test)
  ;; => 61229

  (part2 input1)
  ;; => 1011284
  )
