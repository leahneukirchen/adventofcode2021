(ns org.vuxu.aoc2021.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (->> (slurp "day08")
       str/split-lines
       (map #(str/split % #" \| " 2))
       (map (partial map #(str/split % #" ")))))
  
(def part1
  (->> data
       (map second)
       flatten
       (map count)
       (filter #{2 4 3 7})   ; LCD 1 4 7 8
       count))
;; => 342

(def part2
  (let [canonical-pattern ["abcefg" "cf" "acdeg" "acdfg" "bdcf"
                           "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"]
        freq (frequencies (apply str canonical-pattern))
        dict (zipmap (map #(sort (map freq %)) canonical-pattern)
                     (range 10))]
    (apply + (for [[digits output] data]
               (let [line-freq (frequencies (apply str digits))]
                 (->> output
                      (map #(sort (map line-freq %)))
                      (map dict)
                      (apply str)
                      parse-long))))))
;; => 1068933
