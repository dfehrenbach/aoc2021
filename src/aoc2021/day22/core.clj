(ns aoc2021.day22.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-input [line]
  (let [[_ op xmin xmax ymin ymax zmin zmax]
        (re-find #"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" line)]
    {:operation (keyword op)
     :xmin (read-string xmin) :xmax (read-string xmax)
     :ymin (read-string ymin) :ymax (read-string ymax)
     :zmin (read-string zmin) :zmax (read-string zmax)}))

(defn parse [path]
  (->> path
       slurp
       string/split-lines
       (map parse-input)))

(def input1
  (parse "src/aoc2021/day22/input1.txt"))
(def input-test1
  (parse "src/aoc2021/day22/input-test1.txt"))
(def input-test2
  (parse "src/aoc2021/day22/input-test2.txt"))

(defn build-cube-set [{:keys [xmin xmax ymin ymax zmin zmax]}]
  (into #{} (for [x (range xmin (inc xmax))
                  y (range ymin (inc ymax))
                  z (range zmin (inc zmax))]
              [x y z])))

(defn in-bound-cuboid? [cuboid]
  (every? (set (range -50 51))
          (vals (dissoc cuboid :operation))))

(defn part1 [input]
  (let [clean-cuboids (filter in-bound-cuboid? input)]
    (count (reduce (fn [cube-set cuboid]
                     (prn (count cube-set))
                     (if (= :on (:operation cuboid))
                       (set/union cube-set (build-cube-set cuboid))
                       (set/difference cube-set (build-cube-set cuboid))))
                   #{} clean-cuboids))))

(defn one-d-overlap [[amin amax] [bmin bmax]]
  (if (<= (max amin bmin) (min amax bmax))
    (letfn [(overlap [somemax somemin]
              ;; incrememnt for inclusive ends
              {:range [somemin somemax]
               :res (inc (- somemax somemin))})]
      (min-key :res (overlap amax amin) (overlap amax bmin) (overlap bmax bmin) (overlap bmax amin)))
    {:range [0 0] :res 0}))


(defn cube-overlap [cube-a cube-b]
  (let [{[xmin xmax] :range x-overlap :res} (one-d-overlap [(:xmin cube-a) (:xmax cube-a)]
                                                           [(:xmin cube-b) (:xmax cube-b)])
        {[ymin ymax] :range y-overlap :res} (one-d-overlap [(:ymin cube-a) (:ymax cube-a)]
                                                           [(:ymin cube-b) (:ymax cube-b)])
        {[zmin zmax] :range z-overlap :res} (one-d-overlap [(:zmin cube-a) (:zmax cube-a)]
                                                           [(:zmin cube-b) (:zmax cube-b)])]
    (when-not (some zero? [x-overlap y-overlap z-overlap])
      {:xmin xmin :xmax xmax :ymin ymin :ymax ymax :zmin zmin :zmax zmax
       :overlapped-operation (:operation cube-a)})))

(defn new-overlaps [cubes new-cube]
  (vec (remove nil? (map #(cube-overlap % new-cube) cubes))))

(defn flip-operation [op]
  (case op
    :on :off
    :off :on))

(defn collect-newly-seen-cubes [cubes c]
  (let [overlaps (new-overlaps cubes c)
        operation-flips (map #(assoc % :operation (flip-operation (:overlapped-operation %))) overlaps)]
    (case (:operation c)
      :on  (conj operation-flips c)
      :off operation-flips)))

(defn build-cube-list-with-overlaps [cubes]
  (loop [cubes cubes
         seen-cubes []]
    (let [[c & cs] cubes]
      (if (nil? c) seen-cubes
          (recur cs (concat seen-cubes (collect-newly-seen-cubes seen-cubes c)))))))

(defn cube-volume [cube]
  (bigint (* (inc (- (:xmax cube) (:xmin cube)))
             (inc (- (:ymax cube) (:ymin cube)))
             (inc (- (:zmax cube) (:zmin cube))))))

(defn add-cube-volume [total cube]
  (let [volume (cube-volume cube)]
    (if (= :on (:operation cube)) (+ total volume) (- total volume))))

(defn part2 [cubes]
  (reduce add-cube-volume 0 (build-cube-list-with-overlaps cubes)))

(comment
  (part1 input-test1)
  ;; => 39
  (part1 input-test2)
  ;; => 590784
  (part2 (filter in-bound-cuboid? input-test2))
  ;; => 590784N
  (part1 input1)
  ;; => 650099
  (part2 (filter in-bound-cuboid? input1))
  ;; => 650099N
  (part2 input1)
  ;; => 1254011191104293N

  ;;    Add
  ;;+7  |+++++|
  ;;+5      |+++|
  ;;    
  ;;    Adjust
  ;;+7  |+++++|
  ;;-3      |^|
  ;;+5      |+++|
  ;;    
  ;;    Add
  ;;+7  |+++++|
  ;;-3      |^|
  ;;+5      |+++|
  ;;+12 |++++++++++|
  ;;    
  ;;    Adjust
  ;;+7  |+++++|
  ;;-3      |^|
  ;;+5      |+++|
  ;;-7  |^^^^^|
  ;;+3      |+|
  ;;-5      |^^^|
  ;;+12 |++++++++++|
  ;;    
  ;;    Subtract
  ;;+7  |+++++|
  ;;-3      |^|
  ;;+5      |+++|
  ;;-7  |^^^^^|
  ;;+3      |+|
  ;;-5      |^^^|
  ;;+12 |++++++++++|
  ;;-3  |^|
  ;;    
  ;;    Adjust
  ;;+7  |+++++|
  ;;-3      |^|
  ;;+5      |+++|
  ;;-7  |^^^^^|
  ;;+3      |+|
  ;;-5      |^^^|
  ;;+12 |++++++++++|
  ;;-3  |^|
  ;;+3  |^|
  ;;-3  |^|
  )
