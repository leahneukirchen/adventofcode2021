(ns org.vuxu.aoc2021.day05
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day05")
       (str/split-lines)
       (map #(str/split % #",| -> "))
       (map (partial map parse-long))))

(defn my-range [a b]
  (case (compare a b)
     0 (repeat a)
    -1 (range a (inc b))
    +1 (range a (dec b) -1)))

(defn points [[x1 y1 x2 y2]]
  (map vector (my-range x1 x2) (my-range y1 y2)))

(def part1
  (->> data
       (filter (fn [[x1 y1 x2 y2]]
                 (or (= x1 x2) (= y1 y2))))
       (mapcat points)
       frequencies
       vals
       (filter #(< 1 %))
       count))
;; => 5092

(def part2
  (->> data
       (mapcat points)
       frequencies
       vals
       (filter #(< 1 %))
       count))
;; => 20484
