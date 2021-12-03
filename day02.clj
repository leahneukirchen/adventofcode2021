(ns org.vuxu.aoc2021.day02
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day02")
       (str/split-lines)
       (map (fn [line]
              (let [[command amount] (str/split line #" ")]
                [(keyword command) (parse-long amount)])))))

(def part1
  (->> data
       (reduce (fn [state [command amount]]
                 (case command
                   :forward (update state :pos   + amount)
                   :up      (update state :depth - amount)
                   :down    (update state :depth + amount)))
               {:pos 0 :depth 0})
       vals
       (apply *)))
;; => 1250395

(def part2
  (->> data
       (reduce (fn [state [command amount]]
                 (case command
                   :forward (-> state
                                (update :pos   + amount)
                                (update :depth + (* (:aim state) amount)))
                   :up      (update state :aim - amount)
                   :down    (update state :aim + amount)))
               {:pos 0 :depth 0 :aim 0})
       ((juxt :pos :depth))
       (apply *)))
;; => 1451210346
