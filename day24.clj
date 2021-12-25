(ns org.vuxu.aoc2021.day24
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.set :as set]))

(def data
  (->> (slurp "day24")
       str/split-lines
       (map #(str/split % #" "))
       (map (partial mapv #(if (re-matches #"-?\d+" %) (parse-long %) %)))))

(def params
  (for [part (partition (/ (count data) 14) data)]
    [(last (nth part 5))
     (last (nth part 15))]))

(defn run [z d c1 c2]
  (let [con (not= (+ (mod z 26) c1) d)
        z (if (neg? c1) (quot z 26) z)
        z (if con (+ (* 26 z) d c2) z)]
    z))

(def part1
  (->> (reduce (fn [acc [c1 c2]]
                 (prn [c1 c2 (count acc)])
                 (into (sorted-map)
                       (for [[z v] acc
                             d (range 1 10)
                             :when (<= z (* 26 26 26 26 26))]
                         [(run z d c1 c2) (conj v d)]))
                 )
               {0 []}
               params)
       (#(get % 0))
       (apply str)))
;; => "96299896449997"

(def part2
  (->> (reduce (fn [acc [c1 c2]]
                 (prn [c1 c2 (count acc)])
                 (into (sorted-map)
                       (for [[z v] acc
                             :let [z (- z)]
                             d (reverse (range 1 10))
                             :when (<= z (* 26 26 26 26 26))]
                         [(- (run z d c1 c2)) (conj v d)]))
                 )
               (sorted-map 0 [])
               params)
       (#(get % 0))
       (apply str)))
;; => "31162141116841"
