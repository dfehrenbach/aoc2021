(ns aoc2021.day10.core
  (:require [clojure.string :as string]))

(def input1
  (->> "src/aoc2021/day10/input1.txt"
       slurp
       string/split-lines))

(def input-test
  (->> "src/aoc2021/day10/input-test.txt"
       slurp
       string/split-lines))

(def  matching {\{ \}
                \[ \]
                \( \)
                \< \>})
(def  opening? (set (keys matching)))

(defn update-stack [stack c]
  (cond (opening? c) (conj stack c)
        (= (matching (peek stack)) c) (pop stack)
        :else (reduced {:corrupted c})))

(defn valid? [s]
  (->> s (reduce update-stack [])))

(def corruption->points {\) 3
                         \] 57
                         \} 1197
                         \> 25137})

(defn part1 [input]
  (->> input
       (map valid?)
       (filter :corrupted)
       (map :corrupted)
       (map corruption->points)
       (reduce +)))

(def completion->points {\) 1
                         \] 2
                         \} 3
                         \> 4})
(defn score-completion-str [s]
  (reduce #(->> %1
                (* 5)
                (+ (completion->points %2)))
          0 s))

(defn get-middle-val [arr]
  (let [middle-index (-> (count arr) (/ 2) int)]
    (nth (vec arr) middle-index :uh-oh)))

(defn part2 [input]
  (->> input
       (map valid?)
       (remove :corrupted)
       (map reverse)
       (map #(map matching %))
       (map string/join)
       (map score-completion-str)
       sort
       get-middle-val))


(comment
  (def corrupted "{([(<{}[<>[]}>{[]{[(<()>")
  (valid? corrupted)
  ;; => {:corrupted \}}

  (valid? (first input-test))
  ;; => [\[ \( \{ \( \[ \[ \{ \{]

  (part1 input-test)
  ;; => 26397

  (part1 input1)
  ;; => 319233

  (part2 input-test)
  ;; => 288957

  (part2 input1)
  ;; => 1118976874
  )
