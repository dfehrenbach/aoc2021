(ns aoc2021.day3.core
  (:require [clojure.string :as string]))

(defn transpose [m] (apply mapv vector m))

(def input1
  (->> "src/aoc2021/day3/input1.txt"
       slurp
       string/split-lines
       (map #(string/split % #""))))

(def input-test '(["0" "0" "1" "0" "0"]
                  ["1" "1" "1" "1" "0"]
                  ["1" "0" "1" "1" "0"]
                  ["1" "0" "1" "1" "1"]
                  ["1" "0" "1" "0" "1"]
                  ["0" "1" "1" "1" "1"]
                  ["0" "0" "1" "1" "1"]
                  ["1" "1" "1" "0" "0"]
                  ["1" "0" "0" "0" "0"]
                  ["1" "1" "0" "0" "1"]
                  ["0" "0" "0" "1" "0"]
                  ["0" "1" "0" "1" "0"]))

(defn most-common-bit [bit-freq]
  (key (apply max-key val bit-freq)))

(defn least-common-bit [bit-freq]
  (key (apply min-key val bit-freq)))

(defn strings->bin->int [strings]
  (->> strings
       (into ["2r"])
       string/join
       read-string))

(defn part1 [input]
  (let [bit-freqs (map frequencies (transpose input))
        most-common-bits (map most-common-bit bit-freqs)
        least-common-bits (map least-common-bit bit-freqs)]
    (* (strings->bin->int most-common-bits)
       (strings->bin->int least-common-bits))))

(let [arr [0 0 0 0]]
  (every? #(= % (first arr)) arr))

(defn apply-default-bit [bit-freq bit-criteria-fn default]
  (if (every? #(= % (first (vals bit-freq))) (vals bit-freq))
    default
    (bit-criteria-fn bit-freq)))

(apply-default-bit {"1" 5, "0" 5} most-common-bit "0")

(defn apply-criteria [bit-arrs bit-criteria-fn default pos]
  (if (= (count bit-arrs) 1)
    (first bit-arrs)
    (let [bit-freqs (mapv frequencies (transpose bit-arrs))
          criteria-bit (apply-default-bit (nth bit-freqs pos) bit-criteria-fn default)
          filtered-bit-arr (filter (fn [bit-arr] (= (nth bit-arr pos) criteria-bit)) bit-arrs)]
      (recur filtered-bit-arr
             bit-criteria-fn
             default
             (inc pos)))))


(defn part2 [input]
  (let [oxygen-generator-rating (apply-criteria input most-common-bit "1" 0)
        co2-scrubber-rating (apply-criteria input least-common-bit "0" 0)]
    (* (strings->bin->int oxygen-generator-rating)
       (strings->bin->int co2-scrubber-rating))))

(comment
  (map frequencies (transpose input-test))

  (key (apply max-key val {"0" 5, "1" 7}))

  (strings->bin->int ["1" "0" "1" "1" "0"])

  (part1 input-test)
  ;; => 198

  (part1 input1)
  ;; => 3923414

  (apply-criteria input-test least-common-bit "0" 0)

  (part2 input-test)
  ;; => 230

  (part2 input1)
  ;; => 5852595
  )
