(ns org.vuxu.aoc2021.day12
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day12")
       (str/split-lines)
       (map #(str/split % #"-"))))

(def datamap
  (update-vals
   (group-by first
             (concat
              (map (comp vec reverse) data)
              data))
   #(set (mapv second %))))

(defn dfs [path repeat?]
  (if (= (last path) "end")
    1
    (apply + (for [n (datamap (last path))]
               (cond (not (and (Character/isLowerCase (first n))
                               (some #{n} path)))
                     (dfs (conj path n) repeat?)

                     (and repeat?
                          (= (count (filter #{n} path)) 1)
                          (not= n "start"))
                     (dfs (conj path n) false)

                     :else 0)))))

(def part1
  (dfs ["start"] false))
;; => 4411      

(def part2
  (dfs2 ["start"] true))
;; => 136767
