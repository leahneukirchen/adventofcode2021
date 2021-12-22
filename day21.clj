(ns org.vuxu.aoc2021.day21
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day21")
       str/split-lines
       (map (partial re-find #"\d+$"))
       (mapv parse-long)))

(def freqs
  (frequencies
   (for [a [1 2 3]
         b [1 2 3]
         c [1 2 3]]
     (+ a b c))))

(defn move [roll player state]
  (let [state
        (update-in state [:pos player] #(inc (mod (dec (+ % roll)) 10)))]
    (update-in state [:score player] + (get-in state [:pos player]))))

(def state {:pos data
            :score [0 0]})

(def part1
  (let [game (take-while (fn [state]
                           (every? #(< % 1000) (:score state)))
                         (reductions
                          (fn [state [player val]]
                            (move val player state))
                          state
                          (map vector
                               (cycle [0 1])
                               (map (partial apply +)
                                    (partition 3 (cycle (range 1 101)))))))]
    (* (* 3 (count game))
       (apply min (:score (last game))))))
;; => 713328

(def play
  (memoize
   (fn [player state]
     (cond
       (>= (get-in state [:score 0]) 21) [1 0]
       (>= (get-in state [:score 1]) 21) [0 1]
       :else
       (reduce (fn [result [val freq]]
                 (mapv + result
                         (mapv (partial * freq)
                               (play (- 1 player) (move val player state)))))
               [0 0]
               freqs)))))

(def part2
  (apply max (play 0 state)))
;; => 92399285032143
