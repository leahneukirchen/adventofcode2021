(ns org.vuxu.aoc2021.day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (->> (slurp "day22")
       str/split-lines
       (map (partial re-seq #"(?:on|off|-?\d+)"))
       (map (fn [[cmd & coords]]
              [(= cmd "on") (mapv parse-long coords)]))))

(defn step [state [cmd [x1 x2 y1 y2 z1 z2]]]
  ((if cmd set/union set/difference)
   state
   (set (for [x (range x1 (inc x2))
              y (range y1 (inc y2))
              z (range z1 (inc z2))]
          [x y z]))))

(def part1
  (->> data 
       (filter (fn [[cmd [x1 x2 y1 y2 z1 z2]]]
                 (and (<= -50 x1 x2 50)
                      (<= -50 y1 y2 50)
                      (<= -50 z1 z2 50))))
       (reduce step #{})
       count))
;; => 647062

(def combined
  (reduce
   (fn [cubes [cmd1 [xmin1 xmax1 ymin1 ymax1 zmin1 zmax1] :as ins1]]
     (let [ncubes
           (for [[cmd2 [xmin2 xmax2 ymin2 ymax2 zmin2 zmax2]] cubes
                 :let [xmin (max xmin1 xmin2)
                       xmax (min xmax1 xmax2)]
                 :when (<= xmin xmax)
                 :let [ymin (max ymin1 ymin2)
                       ymax (min ymax1 ymax2)]
                 :when (<= ymin ymax)
                 :let [zmin (max zmin1 zmin2)
                       zmax (min zmax1 zmax2)]
                 :when (<= zmin zmax)]
             [(not cmd2) [xmin xmax ymin ymax zmin zmax]])]
       (cond-> (concat cubes ncubes)
         cmd1
         (conj ins1))))
   []
   data))

(def part2
  (reduce (fn [acc [cmd [x1 x2 y1 y2 z1 z2]]]
            (+ acc (* (if cmd 1 -1)
                      (- x2 x1 -1)
                      (- y2 y1 -1)
                      (- z2 z1 -1))))
          0
          combined))
;; => 1319618626668022
