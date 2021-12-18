(ns org.vuxu.aoc2021.day17
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day17")
       (re-seq #"-?\d+")
       (map parse-long)))

(def x-range (take 2 data))
(def y-range (drop 2 data))

(def part1        
  (quot (* (Math/abs (first y-range))
           (dec (Math/abs (first y-range)))) 2))
;; => 3655

(defn check [[x y] [vx vy]]
  (cond
    (and (<= (first x-range) x (second x-range))
         (<= (first y-range) y (second y-range)))
    true
    
    (< y (first y-range))
    false

    :else
    (recur [(+ x vx) (+ y vy)]
           [(- vx (Integer/signum vx)) (dec vy)])))

(def part2
  (count
   (filter identity
           (for [vx (range (inc (second x-range)))
                 vy (range (first y-range) (inc (Math/abs (first y-range))))]
             (check [0 0] [vx vy])))))
;; => 1447
