(ns org.vuxu.aoc2021.day01
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day01")
       (str/split-lines)
       (map parse-long)))

(def part1
  (->> data
       (partition 2 1)
       (filter (partial apply <))
       count))

part1 ;; => 1688

(def part2
  (->> (map vector data (drop 3 data))      ; a+b+c < b+c+d <==> a < d
       (filter (partial apply <))
       count))

part2 ;; => 1728
