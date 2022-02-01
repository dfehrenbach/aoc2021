(ns aoc2021.day14.core
  (:require [clojure.string :as string]))

(defn split-input [input]
  [(take-while (comp not empty?) input) (drop 1 (drop-while (comp not empty?) input))])

(defn parse-pair-insertion-rules [m s]
  (let [[[_ pair insert]] (re-seq #"(\w+) -> (\w+)" s)]
    (assoc m pair insert)))

(defn parse-input [[polymer-template rules]]
  {:template (first polymer-template)
   :rules (reduce parse-pair-insertion-rules {} rules)})

(defn parse [path]
  (->> path
       slurp
       string/split-lines
       split-input
       parse-input))


(def input1
  (parse "src/aoc2021/day14/input1.txt"))

(def input-test
  (parse "src/aoc2021/day14/input-test.txt"))

(defn break-into-pair-strings [template]
  (map string/join (partition 2 1 template)))

(defn one-less-interleave
  "Interleaves a string of length n with a string of length n-1. 
   Example: ABC XY -> AXBYC"
  [s inserts]
  (string/join (conj (vec (interleave s inserts)) (last s))))

(defn perform-insertions [rules template]
  (let [inserts (->> template break-into-pair-strings (map rules))]
    (one-less-interleave template inserts)))

(defn run-n-insertions [rules n template]
  (first (drop n (iterate (partial perform-insertions rules) template))))

(defn part1 [input]
  (let [{rules :rules template :template} input
        final-template (run-n-insertions rules 10 template)
        freqs (frequencies final-template)]
    (- (apply max (vals freqs))
       (apply min (vals freqs)))))

(defn init-freq-map [{rules :rules template :template}]
  (let [elements (set (concat template
                              (flatten (map seq (keys rules)))
                              (flatten (map seq (vals rules)))))
        all-pairs (for [e1 elements
                        e2 elements]
                    (str e1 e2))
        base-map (reduce #(assoc %1 %2 0) {} all-pairs)]
    (merge-with + base-map (frequencies (break-into-pair-strings template)))))

(defn calculate-new-pairs [rules pair-map pair]
  (let [pair-count (pair-map pair)
        insert (rules pair)
        [new-pair-a new-pair-b] [(str (first pair) insert)
                                 (str insert (second pair))]]
    [pair new-pair-a new-pair-b pair-count]))

(defn pair-map-step [rules pair-map]
  (let [pairs-with-new-vals (pmap (partial calculate-new-pairs rules pair-map)
                                  (keys (remove (comp zero? second) pair-map)))]
    (reduce (fn [acc [old-pair paira pairb n]]
              (-> acc (update old-pair - n) (update paira + n) (update pairb + n)))
            pair-map pairs-with-new-vals)))

(defn quot-round-up [num div]
  (if (pos? (rem num div))
    (inc (quot num div))
    (quot num div)))

(defn pair-map-freqs->frequencies [pair-map]
  (let [elements (set (flatten (map seq (keys pair-map))))
        init-element-map (reduce #(assoc %1 %2 0) {} elements)
        first-pass (reduce (fn [acc [k v]]
                             (-> acc (update (first k) + v) (update (second k) + v)))
                           init-element-map pair-map)]
    (zipmap (keys first-pass)
            (map #(quot-round-up % 2) (vals first-pass)))))

(defn run-n-steps [rules n pair-map]
  (first (drop n (iterate (partial pair-map-step rules) pair-map))))

(defn part2 [input]
  (let [pair-map (init-freq-map input)
        final-pair-map (run-n-steps (:rules input) 40 pair-map)
        freqs (pair-map-freqs->frequencies final-pair-map)]
    (- (apply max (vals freqs))
       (apply min (vals freqs)))))

(comment
  input-test
  ;; => {:template "NNCB", :rules {"CH" "B", "HH" "N", "BH" "H", "BN" "B", "NH" "C", "NB" "B", "HB" "C", "BC" "B", "CN" "C", "CC" "N", "BB" "N", "CB" "H", "HN" "C", "HC" "B", "NC" "B", "NN" "C"}}

  (perform-insertions (:rules input-test) (:template input-test))
  ;; => "NCNBCHB"

  (first (drop 1 (iterate (partial perform-insertions (:rules input-test)) (:template input-test))))
  ;; => "NCNBCHB"
  (first (drop 2 (iterate (partial perform-insertions (:rules input-test)) (:template input-test))))
  ;; => "NBCCNBBBCBHCB"
  (first (drop 3 (iterate (partial perform-insertions (:rules input-test)) (:template input-test))))
  ;; => "NBBBCNCCNBBNBNBBCHBHHBCHB"
  (first (drop 4 (iterate (partial perform-insertions (:rules input-test)) (:template input-test))))
  ;; => "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"

  (part1 input-test)
  ;; => 1588

  (part1 input1)
  ;; => 2703

  (init-freq-map input-test)
  ;; => {"CH" 0, "BH" 0, "BN" 0, "NH" 0, "NB" 0, "HB" 0, "BC" 0, "CN" 0, "CB" 1, "HN" 0, "HC" 0, "NC" 1, "NN" 1}
  (pair-map-freqs->frequencies (init-freq-map input-test))
  ;; => {\B 1, \C 1, \H 0, \N 2}

  (pair-map-step (:rules input-test) (init-freq-map input-test))
  ;; => {"CH" 1, "BH" 0, "BN" 0, "NH" 0, "NB" 1, "HB" 1, "BC" 1, "CN" 1, "CB" 1, "HN" 0, "HC" 0, "NC" 2, "NN" 1}
  (pair-map-freqs->frequencies (pair-map-step (:rules input-test) (init-freq-map input-test)))
  ;; => {\B 2, \C 2, \H 1, \N 2}

  (part2 input-test)
  ;; => 2188189693529

  (part2 input1)
  ;; => 2984946368465
  )
