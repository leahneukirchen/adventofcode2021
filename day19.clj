(ns org.vuxu.aoc2021.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (for [scanner (str/split (slurp "day19") #"\n\n")]
    (->> (str/split-lines scanner)
         (drop 1)
         (map #(str/split % #","))
         (mapv (partial mapv parse-long)))))

(def axes [[0 1 0] [0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])

(defn transform [[x y z] up rot]
  (let [[rx ry rz] (case up
                     [ 0  1  0] [x y z]
                     [ 0 -1  0] [x (- y) (- z)]
                     [ 1  0  0] [y x (- z)]
                     [-1  0  0] [y (- x) z]
                     [ 0  0  1] [y z x]
                     [ 0  0 -1] [y (- z) (- x)])]
    (case rot
      0 [rx ry rz]
      1 [rz ry (- rx)]
      2 [(- rx) ry (- rz)]
      3 [(- rz) ry rx])))

(defn translate [p v]
  (mapv + p v))

(defn diff [v1 v2]
  (mapv - v1 v2))

(defn align [beacons1 beacons2]
  (first
   (for [axis axes
         rotation (range 4)
         :let [rotated-beacons2 (map #(transform % axis rotation) beacons2)]
         b1 beacons1
         matching-b1-in-b2 rotated-beacons2
         :let [delta (diff b1 matching-b1-in-b2)
               transformed-beacons2 (set (map #(translate % delta) rotated-beacons2))
               intersection (set/intersection transformed-beacons2 beacons1)]
         :when (>= (count intersection) 12)]
     [transformed-beacons2 delta axis rotation])))

(defn reduc [scans scanners]
  (let [[to-remove scans scanners]
        (loop [to-remove #{}
               scans scans
               scanners scanners
               i 0
               j 1]
          (prn [i j])
          (if (>= i (dec (count scans)))
            [to-remove scans scanners]
            (if (>= j (count scans))
              (recur to-remove scans scanners (inc i) (+ i 2))
              (if (to-remove j)
                (recur to-remove scans scanners i (inc j))
                
                (if-let [[agt trs up rot] (align (scans i) (scans j))]
                  (recur (conj to-remove j)
                         (update scans i set/union agt)
                         (update scanners i set/union
                                 (set (map #(translate trs (transform % up rot)) (scanners j))))
                         i
                         (inc j))
                  (recur to-remove scans scanners i (inc j)))))))]
    [(vec (keep-indexed #(when (not (to-remove %1)) %2) scans))
     (vec (keep-indexed #(when (not (to-remove %1)) %2) scanners))]))

;; very slow :/
(def reducd
  (loop [[scans scanners]
         [(mapv set data) (mapv (constantly #{[0 0 0]}) data)]]
    (if (> (count scans) 1)
      (recur (reduc scans scanners))
      [(first scans)
       (first scanners)])))

(def part1
  (count (first reducd)))
;; => 445

(defn manhattan [a b]
  (apply + (map #(Math/abs %) (diff a b))))

(def part2
  (apply max
         (for [b1 (second reducd)
               b2 (second reducd)]
           (manhattan b1 b2))))
;; => 13225
