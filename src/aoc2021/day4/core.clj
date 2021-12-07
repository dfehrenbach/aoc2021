(ns aoc2021.day4.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn transpose [m] (apply mapv vector m))

(def input1
  (->> "src/aoc2021/day4/input1.txt"
       slurp
       string/split-lines))

(def input-test
  (->> "src/aoc2021/day4/input-test.txt"
       slurp
       string/split-lines))

(defn parse-board [board-str-arr]
  (mapv (fn [s] (->> (string/split s #" ")
                     (remove empty?)
                     (mapv read-string)))
        board-str-arr))

(defn split-board [boards input]
  (if (empty? input) boards
      (let [next-board (parse-board (take-while seq input))
            remaining-boards (drop 1 (drop-while seq input))]
        (recur (conj boards next-board)
               remaining-boards))))

(defn parse-input [input-lines]
  (let [[bingo-nums _ & rest] input-lines
        bingo-num-arr (map read-string (string/split bingo-nums #","))]
    {:bingo-nums bingo-num-arr
     :boards (split-board [] rest)}))

(defn break-board-into-bingos [board]
  (let [rows (map set board)
        columns (map set (transpose board))]
    {:board board
     :bingos (concat rows columns)}))

(defn bingo? [winning-nums bingo]
  (= bingo (set/intersection bingo winning-nums)))

(defn r-call-number1 [called-nums bingo-nums current-num boards]
  (if (->> boards
           (map #(some (partial bingo? called-nums) (:bingos %)))
           (some true?))
    {:winning-board (->> boards
                         (filter #(some (partial bingo? called-nums) (:bingos %))))
     :winning-nums called-nums
     :last-num-called current-num}
    (recur (conj called-nums (first bingo-nums))
           (rest bingo-nums)
           (first bingo-nums)
           boards)))


(defn r-call-number2 [called-nums bingo-nums current-num boards]
  (if (= 1 (count boards))
    {:last-winning-board (first boards)
     :winning-nums called-nums
     :remaining-nums bingo-nums
     :last-num-called current-num}
    (recur (conj called-nums (first bingo-nums))
           (rest bingo-nums)
           (first bingo-nums)
           (->> boards
                (remove #(some (partial bingo? called-nums) (:bingos %)))))))


(defn part1 [input]
  (let [{bingo-nums :bingo-nums boards :boards}
        (parse-input input)

        {[winning-board] :winning-board winning-nums :winning-nums last-num-called :last-num-called}
        (r-call-number1 (conj #{} (first bingo-nums))
                        (rest bingo-nums)
                        (first bingo-nums)
                        (map break-board-into-bingos boards))

        unmarked-num-sum (reduce + (set/difference
                                    (set (flatten (:board winning-board)))
                                    winning-nums))]

    (* unmarked-num-sum last-num-called)))

(defn part2 [input]
  (let [{bingo-nums :bingo-nums boards :boards}
        (parse-input input)

        {last-winning-board :last-winning-board winning-nums :winning-nums last-num-called :last-num-called remaining-nums :remaining-nums}
        (r-call-number2 (conj #{} (first bingo-nums))
                        (rest bingo-nums)
                        (first bingo-nums)
                        (map break-board-into-bingos boards))

        {[winning-board] :winning-board winning-nums :winning-nums last-num-called :last-num-called}
        (r-call-number1 winning-nums
                        remaining-nums
                        last-num-called
                        [last-winning-board])

        unmarked-num-sum (reduce + (set/difference
                                    (set (flatten (:board last-winning-board)))
                                    winning-nums))]

    (* unmarked-num-sum last-num-called)))


(comment

  (parse-input input-test)

  (parse-board '("22 13 17 11  0" " 8  2 23  4 24" "21  9 14 16  7" " 6 10  3 18  5" " 1 12 20 15 19"))

  (def board [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]])
  (break-board-into-bingos board)

  (part1 input-test)
  ;; => 4512

  (part1 input1)
  ;; => 11536

  (part2 input-test)
  ;; => 1924

  (part2 input1)
  ;; => 1284

  0)
