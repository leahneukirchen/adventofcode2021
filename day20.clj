(ns org.vuxu.aoc2021.day20
  (:require [clojure.string :as str]))

(let [[enhance image] (str/split (slurp "day20") #"\n\n")]
  (def enhance (mapv {\# 1 \. 0} enhance))
  (def image (mapv (partial mapv {\# 1 \. 0}) (str/split-lines image))))

(defn neighbour-offsets [x y]
  (let [digits [-1 0 1]]
    (for [dx digits
          dy digits]
      [(+ x dx) (+ y dy)])))

(defn bin2int [bin]
  (Integer/parseInt (apply str bin) 2))

(defn tick [[image i]]
  (let [background (if (zero? (enhance 0))
                     0
                     (mod i 2))]
    [(vec (for [y (range -1 (inc (count image)))]
            (vec (for [x (range -1 (inc (count (first image))))]
                   (enhance (->> (neighbour-offsets y x)
                                 (map #(get-in image % background))
                                 bin2int))))))
     (inc i)]))

(def part1
  (count (filter (complement zero?)
                 (flatten (first (nth (iterate tick [image 0]) 2))))))
;; => 5229

(def part2
  (count (filter (complement zero?)
                 (flatten (first (nth (iterate tick [image 0]) 50)))))
;; => 17009
