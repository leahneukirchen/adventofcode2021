(ns org.vuxu.aoc2021.day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; attribution: https://www.reddit.com/r/adventofcode/comments/rds32p/2021_day_11_solutions/ho4d867/

(defn parse-digit [char]
  (Character/digit char 10))

(def data
  (->> (slurp "day11")
       (str/split-lines)
       (mapv (partial mapv parse-digit))))

(def datamap
  (zipmap (for [x (range (count data))
                y (range (count (data x)))]
            [x y])
          (flatten data)))

(defn neighbours [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (< -1 (+ x dx) (count data))
        :when (< -1 (+ y dy) (count (first data)))]
    [(+ x dx) (+ y dy)]))

(defn trigger [flashed grid]
  (let [flashing (set/difference
                  (set (map first (filter (fn [[k v]] (> v 9)) grid)))
                  flashed)
        new-grid (reduce (fn [g p] (update g p inc))
                         grid
                         (mapcat neighbours flashing))]
    (if (empty? flashing)
      [flashed grid]
      (trigger (set/union flashed flashing) new-grid))))

(defn step [[_ grid]]
  (let [[flashed updated] (trigger #{} (update-vals grid inc))]
    [(count flashed)
     (apply merge updated (map #(vector % 0) flashed))]))

(def part1
  (apply + (take 101 (map first (iterate step [0 datamap])))))
;; => 1713

(def part2
  (count (take-while #(< % (* (count data) (count (first data))))
                     (map first (iterate step [0 datamap])))))
;; => 502
