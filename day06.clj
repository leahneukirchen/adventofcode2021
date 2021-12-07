(ns org.vuxu.aoc2021.day06
  (:require [clojure.string :as str]))

(def data
  (let [values (->> (str/split (str/trim-newline (slurp "day06")) #",")
                    (mapv parse-long))]
    (mapv #(count (filter #{%} values)) (range 0 9))))

(defn stepv [fishes]
  (mapv (fn [i]
          (cond (= i 8)    (fishes 0)
                (= i 6) (+ (fishes 0) (fishes (inc i)))
                :else                 (fishes (inc i))))
        (range 0 9)))

(def part1
  (apply + (nth (iterate stepv data) 80)))
;; => 373378

(def part2
  (apply + (nth (iterate stepv data) 256)))
;; => 1682576647495
