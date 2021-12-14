(ns org.vuxu.aoc2021.day13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(let [[part1 part2] (str/split (slurp "day13") #"\n\n")]
  (def dots (set (for [line (str/split-lines part1)]
                   (mapv parse-long (str/split line #",")))))
  (def folds (for [line (str/split-lines part2)]
               (let [[xy n] (take-last 2 (str/split line #" |="))]
                 [(symbol xy)
                  (parse-long n)]))))

(defn fold [dots [xy n]]
  (let [sel (case xy
              x 0
              y 1)
        {a false b true} (group-by #(> (get % sel) n) dots)
        a' (for [coord b]
                 (update coord sel #(- n (- % n))))]
    (set/union (set a) (set a'))))

(def part1
  (count (fold dots (first folds))))
;; => 716

(defn show [dots]
  (str/join "\n"
            (reverse
             (for [x (range (inc (apply max (map first dots))))]
               (apply str
                      (for [y (range (inc (apply max (map second dots))))]
                        (if (dots [x y]) "#" ".")))))))

(def part2
  (show (reduce fold dots folds)))

(print "---\n" part2 "\n---\n")
;; RPCKFBLR
