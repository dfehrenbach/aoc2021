(ns aoc2021.day8.core
  (:require [clojure.string :as string]))

(defn parse-line [input-line]
  (let [[signals output-vals] (string/split input-line #" \| ")]
    {:signals (string/split signals #" ") :output-vals (string/split output-vals #" ")}))

(def input1
  (->> "src/aoc2021/day8/input1.txt"
       slurp
       string/split-lines
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


(def freqs->output
  {{7 2, 8 2, 6 1, 9 1} "9"
   {7 2, 8 2, 6 1, 9 1, 4 1} "8"
   {7 2, 8 1, 6 1, 9 1} "5"
   {7 2, 8 2, 4 1} "2"
   {7 2, 8 2, 9 1} "3"
   {7 2, 8 1, 6 1, 9 1, 4 1} "6"
   {8 1, 9 1} "1"
   {8 1, 6 1, 9 1, 7 1} "4"
   {8 2, 9 1} "7"
   {7 1, 8 2, 6 1, 9 1, 4 1} "0"})

(defn char->freq [signals]
  (frequencies (flatten (map seq signals))))

(defn global-freq->local-freq [signals output-vals]
  (map #(frequencies (map (char->freq signals) %)) output-vals))

(defn solve-line [{signals :signals output-vals :output-vals}]
  (->> (global-freq->local-freq signals output-vals)
       (map freqs->output)
       (drop-while #(= % "0"))
       string/join
       read-string))

(defn part2 [input]
  (reduce + (map solve-line input)))

(comment
  (part1 input-test)
  ;; => 26

  (part1 input1)
  ;; => 532

  (def signals (:signals (first input-test)))
  (def output-vals (:output-vals (first input-test)))

  (map freqs->output (map #(frequencies (map (char->freq signals) %)) output-vals))
  ;; => ("8" "3" "9" "4")

  (frequencies (map (char->freq signals) (first output-vals)))
  ;; => {7 2, 8 2, 6 1, 4 1, 9 1}

  (part2 input-test)
  ;; => 61229

  (part2 input1)
  ;; => 1011284
  )
