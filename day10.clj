(ns org.vuxu.aoc2021.day10
  (:require [clojure.string :as str]))

(defn fix [f x]
  (loop [x x]
    (let [fx (f x)]
      (if (= x fx) x (recur fx)))))

(let [[incomplete corrupted]
      (->> (slurp "day10")
           (str/split-lines)
           (map (partial fix #(str/replace % #"\(\)|\[\]|\{\}|<>" "")))
           ((juxt filter remove) (partial re-find #"[)}>\]]")))]
  (def incomplete incomplete)
  (def corrupted corrupted))

(def part1
  (->> incomplete
       (map (partial some #{\) \] \} \>}))
       (map {\) 3 \] 57 \} 1197 \> 25137})
       (apply +)))
;; => 216297

(def part2
  (->> corrupted
       (map reverse)
       (map (partial map {\( 1 \[ 2 \{ 3 \< 4}))
       (map (partial reduce #(+ (* %1 5) %2) 0))
       sort
       (#(nth % (bit-shift-right (count corrupted) 1)))))
;; => 2165057169
