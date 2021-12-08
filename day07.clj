(ns org.vuxu.aoc2021.day07
  (:require [clojure.string :as str]))

(def data
  (->> (str/split (str/trim-newline (slurp "day07")) #",")
       (mapv parse-long)))

(defn median [coll]
  ; only works for even lengths but good enough here
  (nth (sort coll) (/ (count coll) 2)))

(defn integer-means [coll]
  (let [mean (/ (apply + coll) (count coll))]
    [(long (Math/floor mean))
     (long (Math/ceil mean))]))

(defn abs [n]
  (Math/abs n))

(defn fuel [n]
  (* 1/2 n (inc n)))

(def part1
  (->> data
       (map (partial - (median data)))
       (map abs)
       (apply +)))
;; => 328187

(def part2
  (apply min (for [m (integer-means data)]
               (->> data
                    (map (partial - m))
                    (map abs)
                    (map fuel)
                    (apply +)))))
;; => 91257582N
