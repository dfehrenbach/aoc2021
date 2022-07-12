(ns aoc2021.day23.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

;; EXAMPLE:
;;#############
;;#...........#
;;###B#C#B#D###
;;  #A#D#C#A#
;;  #########
(def input-test {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
                 :A-room [:A :B]
                 :B-room [:D :C]
                 :C-room [:C :B]
                 :D-room [:A :D]
                 :next-moves []
                 :accumulated-energy 0})

;; INPUT:
;;#############
;;#...........#
;;###D#B#C#A###
;;  #C#A#D#B#
;;  #########
(def input1 {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
             :A-room [:C :D]
             :B-room [:A :B]
             :C-room [:D :C]
             :D-room [:B :A]
             :next-moves []
             :accumulated-energy 0})

(def amphipod-energy {:A 1 :B 10 :C 100 :D 1000})
(def room-locations {:A-room 2 :B-room 4 :C-room 6 :D-room 8})

(defn open-hallway-locations [starting-location hallway & {:keys [with-rooms]}]
  (let [[left right] (partition-by #(< starting-location (first %)) (map-indexed vector hallway))
        lowbound (apply max (concat [0] (map (comp inc first) (filter second left))))
        highbound (apply min (concat (map first (filter second right)) [(count hallway)]))]
    (if with-rooms (range lowbound highbound)
        (remove (set (vals room-locations)) (range lowbound highbound)))))

(defn egress-amphipod [state room]
  (let [[bot top] (room state)]
    (if top
      [top 1]
      [bot 2])))

(defn egress-energy [room hallway-dest amphipod room-exit-distance]
  (* (amphipod-energy amphipod)
     (+ (Math/abs (- (room-locations room) hallway-dest))
        room-exit-distance)))

(defn rooms-needing-egress [state]
  (remove (fn [room]
            (letfn [(check-partial-solve [room-vals amphipod]
                      (or (empty? room-vals) (every? #{amphipod} room-vals)))]
              (case room
                :A-room (check-partial-solve (state room) :A)
                :B-room (check-partial-solve (state room) :B)
                :C-room (check-partial-solve (state room) :C)
                :D-room (check-partial-solve (state room) :D))))
          [:A-room :B-room :C-room :D-room]))


(defn next-egress-moves [state]
  (for [egressable-room (rooms-needing-egress state)
        hallway-loc (open-hallway-locations (room-locations egressable-room) (:hallway state))]
    (let [[amphipod exit-distance] (egress-amphipod state egressable-room)]
      (-> state
          (assoc-in [:hallway hallway-loc] amphipod)
          (assoc egressable-room (vec (pop (egressable-room state))))
          (update :accumulated-energy + (egress-energy egressable-room hallway-loc amphipod exit-distance))))))

(defn home-room-open? [amphipod state]
  (letfn [(open? [room val] (or (empty? (room state)) (= val (first (room state)))))]
    (case amphipod
      :A (open? :A-room :A)
      :B (open? :B-room :B)
      :C (open? :C-room :C)
      :D (open? :D-room :D))))

(defn home-room-accessible? [starting-location amphipod state]
  (let [accessible-locations (open-hallway-locations starting-location (:hallway state) :with-rooms true)]
    (letfn [(accessible? [room] (some? (some #{(room-locations room)} accessible-locations)))]
      (case amphipod
        :A (accessible? :A-room)
        :B (accessible? :B-room)
        :C (accessible? :C-room)
        :D (accessible? :D-room)))))

(defn enter-home-room [hallway-loc amphipod state]
  (letfn [(energy [room]
            (* (amphipod-energy amphipod)
               (+ (Math/abs (- hallway-loc (room-locations room)))
                  (- 2 (count (room state))))))]
    (case amphipod
      :A [:A-room (energy :A-room)]
      :B [:B-room (energy :B-room)]
      :C [:C-room (energy :C-room)]
      :D [:D-room (energy :D-room)])))

(defn next-ingress-moves [state]
  (for [[hallway-loc amphipod] (remove (comp nil? second) (map-indexed vector (:hallway state)))
        :when (home-room-open? amphipod state)
        :when (home-room-accessible? hallway-loc amphipod state)]
    (let [[room enter-energy] (enter-home-room hallway-loc amphipod state)]
      (-> state
          (assoc-in [:hallway hallway-loc] nil)
          (update room conj amphipod)
          (update :accumulated-energy + enter-energy)))))

(defn puzzle-complete? [state]
  (and (every? nil? (:hallway state))
       (every? #{:A} (:A-room state))
       (every? #{:B} (:B-room state))
       (every? #{:C} (:C-room state))
       (every? #{:D} (:D-room state))))

#_(defn progress [state]
    (letfn [(recursive-progress [state moves-so-far]
              (if (puzzle-complete? state) moves-so-far
                  (for [next-state (concat (next-ingress-moves state) (next-egress-moves state))]
                    (recursive-progress next-state (conj moves-so-far next-state)))))]
      (recursive-progress state [state])))

;; [] -> [[][]] -> []

#_(defn progress [starting-state]
    (loop [[states finished] [[starting-state] []]]
      (prn (count states) (count finished))
      (if (not (empty? finished)) finished
          (recur (reduce (fn [[next finished] state]
                           (let [next-moves (concat (next-ingress-moves state) (next-egress-moves state))]
                             [(concat next next-moves)
                              (if (puzzle-complete? next-moves) (conj finished state) finished)]))
                         [[] []] states)))))

(comment
  (def a (first (next-egress-moves input-test)))
  (next-egress-moves (first (next-egress-moves (first (next-egress-moves (first (next-egress-moves a)))))))
  (first (next-ingress-moves {:hallway [nil nil nil nil nil nil nil nil nil nil nil], :A-room [], :B-room [], :C-room [], :D-room [], :next-moves [], :accumulated-energy 40}))
  (some? (some #{1}
               (open-hallway-locations 0 (:hallway a) :with-rooms true)))
  (concat (next-ingress-moves a) (next-egress-moves a))
  (progress input-test)

  0)
