(ns org.vuxu.aoc2021.day05
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day05")
       (str/split-lines)
       (map #(str/split % #",| -> "))
       (map (partial map parse-long))))

(use 'criterium.core)

(def part1
  (->> data
       (filter (fn [[x1 y1 x2 y2]]
                 (or (= x1 x2) (= y1 y2))))
       (mapcat (fn [[x1 y1 x2 y2]]
                 (for [x (range (min x1 x2) (inc (max x1 x2)))
                       y (range (min y1 y2) (inc (max y1 y2)))]
                   [x y])))
       frequencies
       vals
       (filter #(< 1 %))
       count))
;; => 5092

(defn points [[x1 y1 x2 y2]]
  (let [dx (Long/signum (- x2 x1))
        dy (Long/signum (- y2 y1))]
    (loop [x x1
           y y1
           r []]
      (if (and (= x2 x)
               (= y2 y))
        (conj r [x y])
        (recur (+ x dx) (+ y dy) (conj r [x y]))))))

(def part2
  (->> data
       (mapcat points)
       frequencies
       vals
       (filter #(< 1 %))
       count))
;; => 20484
