(ns aoc2021.day17.core)

(def input-test {:xmin 20 :xmax 30
                 :ymin -10 :ymax -5})

(def input1 {:xmin 150 :xmax 193
             :ymin -136 :ymax -86})

(defn move [{:keys [velocity] :as probe-state}]
  (let [{dx :x dy :y} velocity]
    (-> probe-state
        (update-in [:position :x] + dx)
        (update-in [:position :y] + dy))))

(defn apply-drag [probe-state]
  (let [approach-zero (fn [x] (cond (pos? x) (dec x)
                                    (neg? x) (inc x)
                                    (zero? x) 0))]
    (update-in probe-state [:velocity :x] approach-zero)))

(defn apply-gravity [probe-state]
  (update-in probe-state [:velocity :y] dec))

(defn probe-step [probe-state]
  (->> probe-state
       move
       apply-drag
       apply-gravity))

(def init-probe-state {:position {:x 0 :y 0}
                       :velocity {:x 0 :y 0}})

(defn beyond-target [{:keys [xmax ymin] :as _target} {:keys [position]}]
  (or
   (< (:y position) ymin)
   (< xmax (:x position))))

(defn in-target [{:keys [xmin xmax ymin ymax] :as _target} {:keys [position]}]
  (and
   (<= ymin (:y position) ymax)
   (<= xmin (:x position) xmax)))

(defn scan-steps [target targetfn probe-state]
  (loop [probe-state probe-state
         steps [probe-state]]
    (if (targetfn target probe-state) steps
        (let [next-step (probe-step probe-state)]
          (when ((complement beyond-target) target next-step)
            (recur next-step (conj steps next-step)))))))

(defn check-probe-velocity [target targetfn probe-state]
  (when-let [steps (scan-steps target targetfn probe-state)]
    steps))

(defn part1
  "There is a pattern that the acceptable solutions go from 0 to the lowest y val.
  So, we can simply add [0, (absoluteval lowest-y-val)).
  In truth, because we always start at 0,0 and go UP, we can safetly assume that once it reaches the top, the y values are parabolically symmetrical all the way back to 0.
  So, the we want to hit 0 and go to the far y-bound of the box, which is ymin.
  We can safely sum all values (dec ymin)! that are above the starting y value, 0."
  [input]
  (reduce + (range (* -1 (:ymin input)))))

(defn n-for-consecutive-sum [sum]
  (let [triangle-sum (fn [n] (* n 1/2 (+ n 1)))]
    (loop [x 1]
      (if (= sum (triangle-sum x)) x
          (recur (inc x))))))

(defn part2 [input]
  (let [theory-x-max (:xmax input)
        theory-y-min (:ymin input)
        theory-y-max (n-for-consecutive-sum (part1 input))]
    (count (for [x (range (inc theory-x-max))
                 y (range theory-y-min (inc theory-y-max))
                 :let [path (check-probe-velocity input in-target (assoc init-probe-state :velocity {:x x :y y}))]
                 :when path]
             [x y]))))


(comment
  (part1 input-test)
  ;; => 45

  (part1 input1)
  ;; => 9180

  (part2 input-test)
  ;; => 112

  (part2 input1)
  ;; => 3767
  )
