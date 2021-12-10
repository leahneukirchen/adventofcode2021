(ns org.vuxu.aoc2021.day09
  (:require [clojure.string :as str]))

(defn parse-digit [char]
  (Character/digit char 10))

(def data
  (->> (slurp "day09")
       (str/split-lines)
       (mapv (partial mapv parse-digit))))

(def datamap
  (zipmap (for [x (range (count data))
                y (range (count (data x)))]
            [x y])
          (flatten data)))

(def low-points 
  (filter (fn [[x y]]
            (< (get datamap [x y])
               (apply min (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
                            (get datamap [(+ x dx) (+ y dy)] 999)))))
          (keys datamap)))

(def part1
  (->> low-points
       (map datamap)
       (map inc)
       (apply +)))
;; => 512

(defn grow [[x y]]
  (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
         [(+ x dx) (+ y dy)])
       (filter (fn [[xx yy]]
                 (not= 9 (get datamap [xx yy] 9))))))

(defn fix [f x]
  (loop [x x]
    (let [fx (f x)]
      (if (= x fx) x (recur fx)))))

(defn measure [p]
  (count (fix (fn [ps] (apply conj ps (mapcat grow ps)))
              (set [p]))))

(def part2
  (->> low-points
       (map measure)
      sort
      (take-last 3)
      (apply *)))
;; => 1600104
