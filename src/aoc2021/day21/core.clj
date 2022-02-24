(ns aoc2021.day21.core
  (:require [clojure.set :as set]))

(defn init-player [starting-position]
  {:position starting-position
   :score 0
   :turn 0})

(def input-test1 {:player1 (init-player 4)
                  :player2 (init-player 8)})
(def input1 {:player1 (init-player 9)
             :player2 (init-player 4)})

(defn rotate-value [val-max v]
  (inc (mod (dec v) val-max)))

(defn incremented-roll [dice-value]
  (let [next-vals (range dice-value (+ 3 dice-value))]
    [(reduce + (map (partial rotate-value 100) next-vals))
     (inc (rotate-value 100 (last next-vals)))]))

(defn init-game-state1 [player]
  (assoc player :dice 1))

(defn make-normal-turn [{:keys [dice] :as game-state} player-indicator]
  (let [[movement next-dice-val] (incremented-roll dice)
        player (player-indicator game-state)
        new-position (rotate-value 10 (+ movement (:position player)))]
    (-> game-state
        (update-in [player-indicator :turn] inc)
        (assoc-in [player-indicator :position] new-position)
        (update-in [player-indicator :score] + new-position)
        (assoc :dice next-dice-val))))

(defn game-finished? [{:keys [player1 player2]}]
  (or (<= 1000 (:score player1))
      (<= 1000 (:score player2))))

(defn play-game [game-state starting-player]
  (if (game-finished? game-state) game-state
      (recur (make-normal-turn game-state starting-player)
             (if (= starting-player :player1) :player2 :player1))))

(defn part1 [player-init]
  (let [final-state (play-game (init-game-state1 player-init) :player1)
        dice-rolls (* 3 (+ (-> final-state :player1 :turn)
                           (-> final-state :player2 :turn)))
        losing-score (min (-> final-state :player1 :score)
                          (-> final-state :player2 :score))]
    (* dice-rolls losing-score)))

(def mov-prob-map
  (frequencies (for [a [1 2 3]
                     b [1 2 3]
                     c [1 2 3]]
                 (+ a b c))))

(defn init-path-map [x]
  (zipmap (map #(vector (rotate-value 10 (+ x %)))
               (keys mov-prob-map))
          (vals mov-prob-map)))

(def input-test2 {:player1 {:path-map (init-path-map 4) :wins 0}
                  :player2 {:path-map (init-path-map 8) :wins 0}})

(def input2 {:player1 {:path-map (init-path-map 9) :wins 0}
             :player2 {:path-map (init-path-map 4) :wins 0}})


(defn progress-path-map [path->occ]
  (reduce (fn [acc [path n]]
            (into acc (map (fn [[mov mov-n]]
                             [(conj path (rotate-value 10 (+ mov (last path))))
                              (* mov-n n)])
                           mov-prob-map)))
          {} path->occ))

(defn collect-win-loss [path-map win-loss-fn]
  (->> path-map
       (filter (comp win-loss-fn (partial reduce +) key))
       vals
       (reduce +)))

(defn get-wins-for-player [game-state player-indicator]
  (let [other-player (if (= player-indicator :player1) :player2 :player1)]
    (* (collect-win-loss (-> game-state player-indicator :path-map) (partial <= 21))
       (collect-win-loss (-> game-state other-player :path-map) (partial > 21)))))

(defn remove-winning-paths [path-map]
  (->> (keys path-map)
       (filter (comp (partial <= 21) (partial reduce +)))
       (apply dissoc path-map)))

(defn record-for-player [game-state player-indicator]
  (-> game-state
      (update-in [player-indicator :wins] + (get-wins-for-player game-state player-indicator))
      (update-in [player-indicator :path-map] remove-winning-paths)))

(defn make-turn [game-state player-indicator]
  (-> game-state
      (update-in [player-indicator :path-map] progress-path-map)
      (record-for-player player-indicator)))

(defn dirac-game-finished? [{:keys [player1 player2]}]
  (or (empty? (player1 :path-map)) (empty? (player2 :path-map))))

(defn play-dirac-game [game-state starting-player]
  (if (dirac-game-finished? game-state) game-state
      (recur (make-turn game-state starting-player)
             (if (= starting-player :player1) :player2 :player1))))

(defn part2 [input]
  (let [game-results (play-dirac-game input :player1)]
    (max (-> game-results :player1 :wins)
         (-> game-results :player2 :wins))))




(defn init-player2 [starting-position]
  {:scoremap {0 {:locs #{starting-position} :n 1}}
   :turn 0
   :wins 0})

(comment
  (play-game (init-game-state1 input-test1) :player1)
  ;; => {:player1 {:position 10, :score 1000, :turn 166}, :player2 {:position 3, :score 745, :turn 165}, :dice 94}

  (part1 input-test1)
  ;; => 739785

  (part1 input1)
  ;; => 998088

  (play-dirac-game input-test2 :player1)
  ;; => {:player1 {:path-map {}, 
  ;;               :wins 444356092776315}, 
  ;;     :player2 {:path-map {[2 1 4 3 2 1 4 2 1] 9, [1 4 2 1 4 2 1 4 1] 54, [3 2 1 4 2 1 4 2 1] 54, [2 1 4 2 1 4 3 2 1] 9, [2 1 4 1 4 2 1 4 1] 324, [1 4 1 4 2 1 4 2 1] 54, [2 1 4 1 4 1 4 2 1] 324, [1 4 2 1 4 1 4 2 1] 54, [2 1 4 2 1 4 1 4 1] 324}, 
  ;;               :wins 341960390180808}}

  (part2 input-test2)
  ;; => 444356092776315

  (part2 input2)
  ;; => 306621346123766
  )
