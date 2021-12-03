(ns org.vuxu.aoc2021.day03
  (:require [clojure.string :as str]))

(defn parse-digit [char]
  (Character/digit char 10))

(defn bits->number [bits]
  (Integer/parseInt (apply str bits) 2))

(def data
  (->> (slurp "day03")
       (str/split-lines)
       (mapv (partial mapv parse-digit))))

(def part1
  (let [gamma (->> data       
                   (apply mapv +)
                   (map #(if (>= % (/ (count data) 2)) 1 0)))
        epsilon (map (partial - 1) gamma)]
    (* (bits->number gamma) (bits->number epsilon))))
;; => 2003336

(def part2
  (letfn [(helper [selector]
            (loop [cands data
                   i 0]
              (let [freqs (->> cands
                               (map #(get % i))
                               frequencies)
                    choice (if (selector (freqs 0 0) (freqs 1 0)) 1 0)
                    new-cands (filter #(= (get % i) choice) cands)]
            (if (> (count new-cands) 1)
              (recur new-cands (inc i))
              (bits->number (first new-cands))))))]
    (* (helper <=) (helper >))))
;; => 1877139
