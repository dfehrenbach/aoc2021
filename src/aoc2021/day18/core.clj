(ns aoc2021.day18.core
  (:require [clojure.string :as string]
            [clojure.zip :as zip]
            [clojure.walk :as walk]))

(def input-test1
  [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
   [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
   [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
   [7,[5,[[3,8],[1,4]]]]
   [[2,[2,2]],[8,[8,1]]]
   [2,9]
   [1,[[[9,3],9],[[9,0],[0,7]]]]
   [[[5,[7,4]],7],1]
   [[[[4,2],2],6],[8,7]]])

(def input-test2
  [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
   [[[5,[2,8]],4],[5,[[9,9],0]]]
   [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
   [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
   [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
   [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
   [[[[5,4],[7,7]],8],[[8,3],8]]
   [[9,3],[[9,9],[6,[4,9]]]]
   [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
   [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

(def input1
  (->> "src/aoc2021/day18/input1.txt"
       slurp
       string/split-lines
       (mapv read-string)))

(defn add-pairs [left right]
  (vec (concat [left] [right])))

(defn rezip [z]
  (zip/vector-zip (zip/root z)))

(defn z-value? [z]
  (number? (zip/node z)))

(defn z-value-vec? [z]
  (and (vector? (zip/node z))
       (number? (first (zip/node z)))
       (number? (second (zip/node z)))))

(defn splittable? [z]
  (and (z-value? z)
       (<= 10 (zip/node z))))

(defn explodable? [z]
  (and (z-value-vec? z)
       (<= 4 (count (:pnodes (second z))))))

(defn search-for-reduction [pred z]
  (when-not (zip/end? z)
    (if (pred z) z
        (recur pred (zip/next z)))))

(defn search-val [z fmove]
  (loop [z z dist 1]
    (when (not= (zip/node z) (zip/root z))
      (if (z-value? z) [z dist]
          (recur (fmove z) (inc dist))))))

(defn search-update-and-return [z additive move-fn return-fn]
  (if-let [[foundz dist] (search-val (move-fn z) move-fn)]
    (reduce #(%2 %1) (zip/edit foundz + additive) (repeat dist return-fn))
    z))

(defn explode [exploding-z]
  (let [[[leftv rightv] _z] exploding-z]
    (-> exploding-z
        (zip/replace 0)
        (search-update-and-return leftv zip/prev zip/next)
        (search-update-and-return rightv zip/next zip/prev))))

(defn split [splitting-z]
  (let [[v _z] splitting-z
        leftv (int (Math/floor (/ v 2)))
        rightv (int (Math/ceil (/ v 2)))]
    (zip/replace splitting-z [leftv rightv])))

(defn reduction-step [z]
  (condp search-for-reduction z
    explodable? :>> (comp rezip explode)
    splittable? :>> (comp rezip split)
    nil))

(defn perform-reductions [z]
  (if-let [next-step (reduction-step z)]
    (recur next-step)
    (zip/root z)))

(defn add-list-of-snail-numbers [nums]
  (reduce (comp perform-reductions zip/vector-zip add-pairs) nums))

(defn magnitude-multiplication [node]
  (if (vector? node)
    (+ (* 3 (first node)) (* 2 (second node)))
    node))

(defn calculate-magnitude [v]
  (walk/postwalk magnitude-multiplication v))

(defn part1 [input]
  (->> input
       add-list-of-snail-numbers
       calculate-magnitude))

(defn part2 [input]
  (apply max (for [sfn1 input
                   sfn2 (remove #{sfn1} input)]
               (part1 [sfn1 sfn2]))))

(comment
  (def exploding-example (zip/vector-zip [[[[[9,8],1],2],3],4]))
  (def splitting-example (zip/vector-zip [[[[0,7],4],[15,[0,13]]],[1,1]]))

  (search-for-reduction explodable? exploding-example)
  ;; => [[9 8] {:l [], :pnodes [[[[[[9 8] 1] 2] 3] 4] [[[[9 8] 1] 2] 3] [[[9 8] 1] 2] [[9 8] 1]], :ppath {:l [], :pnodes [[[[[[9 8] 1] 2] 3] 4] [[[[9 8] 1] 2] 3] [[[9 8] 1] 2]], :ppath {:l [], :pnodes [[[[[[9 8] 1] 2] 3] 4] [[[[9 8] 1] 2] 3]], :ppath {:l [], :pnodes [[[[[[9 8] 1] 2] 3] 4]], :ppath nil, :r (4)}, :r (3)}, :r (2)}, :r (1)}]

  (search-for-reduction splittable? splitting-example)
  ;; => [15 {:l [], :pnodes [[[[[0 7] 4] [15 [0 13]]] [1 1]] [[[0 7] 4] [15 [0 13]]] [15 [0 13]]], :ppath {:l [[[0 7] 4]], :pnodes [[[[[0 7] 4] [15 [0 13]]] [1 1]] [[[0 7] 4] [15 [0 13]]]], :ppath {:l [], :pnodes [[[[[0 7] 4] [15 [0 13]]] [1 1]]], :ppath nil, :r ([1 1])}, :r nil}, :r ([0 13])}]

  (zip/root (explode (search-for-reduction explodable? exploding-example)))
  ;; => [[[[0 9] 2] 3] 4]

  (zip/root (split (search-for-reduction splittable? splitting-example)))
  ;; => [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]

  (zip/root (reduction-step exploding-example))
  ;; => [[[[0 9] 2] 3] 4]

  (zip/root (reduction-step splitting-example))
  ;; => [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]

  (perform-reductions exploding-example)
  ;; => [[[[0 9] 2] 3] 4]
  (perform-reductions splitting-example)
  ;; => [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]

  (add-list-of-snail-numbers [[[[[4,3],4],4],[7,[[8,4],9]]], [1 1]])
  ;; => [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]

  (add-list-of-snail-numbers [[1 1] [2 2] [3 3] [4 4]])
  ;; => [[[[1 1] [2 2]] [3 3]] [4 4]]
  (add-list-of-snail-numbers [[1 1] [2 2] [3 3] [4 4] [5 5]])
  ;; => [[[[3 0] [5 3]] [4 4]] [5 5]]
  (add-list-of-snail-numbers [[1 1] [2 2] [3 3] [4 4] [5 5] [6 6]])
  ;; => [[[[5 0] [7 4]] [5 5]] [6 6]]

  (add-list-of-snail-numbers input-test1)
  ;; => [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]

  (calculate-magnitude [9 1])
  ;; => 29
  (calculate-magnitude [1 9])
  ;; => 21
  (calculate-magnitude [[9 1] [1 9]])
  ;; => 129
  (calculate-magnitude [[1,2],[[3,4],5]])
  ;; => 143
  (calculate-magnitude [[[[0,7],4],[[7,8],[6,0]]],[8,1]])
  ;; => 1384
  (calculate-magnitude [[[[1,1],[2,2]],[3,3]],[4,4]])
  ;; => 445
  (calculate-magnitude [[[[3,0],[5,3]],[4,4]],[5,5]])
  ;; => 791
  (calculate-magnitude [[[[5,0],[7,4]],[5,5]],[6,6]])
  ;; => 1137

  (calculate-magnitude (add-list-of-snail-numbers input-test1))
  ;; => 3488
  (calculate-magnitude (add-list-of-snail-numbers input-test2))
  ;; => 4140

  (part1 input-test1)
  ;; => 3488
  (part1 input-test2)
  ;; => 4140
  (part1 input1)
  ;; => 3691

  (part2 input-test1)
  ;; => 3805
  (part2 input-test2)
  ;; => 3993
  (part2 input1)
  ;; => 4756
  )
