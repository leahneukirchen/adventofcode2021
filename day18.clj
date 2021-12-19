(ns org.vuxu.aoc2021.day18
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day18")
       str/split-lines
       (map clojure.edn/read-string)))

(defn add-leftmost [n t]
  (if (number? t)
    (+ n t)
    [(add-leftmost n (first t)) (second t)]))

(defn add-rightmost [n t]
  (if (number? t)
    (+ n t)
    [(first t) (add-rightmost n (second t))]))

(defn explode2 [depth t]
  (if (number? t)
    nil
    (let [[l r] t]
      (if (= depth 4)
        [0 l r]
        (if-let [[l' ln rn] (explode2 (inc depth) l)]
          [[l' (add-leftmost rn r)] ln 0]
          (if-let [[r' ln rn] (explode2 (inc depth) r)]
            [[(add-rightmost ln l) r'] 0 rn]))))))

(defn explode [t]
  (first (explode2 0 t)))

(defn split [t]
  (if (number? t)
    (if (>= t 10)
      [(long (Math/floor (/ t 2)))
       (long (Math/ceil (/ t 2)))])
    (let [[l r] t]
      (if-let [l' (split l)]
        [l' r]
        (if-let [r' (split r)]
          [l r'])))))

(defn reduc [t]
  (if-let [t' (explode t)]
    (recur t')
    (if-let [t' (split t)]
      (recur t')
      t)))

(defn add [x y]
  (reduc (conj [x] y)))

(defn mag [t]
  (if (number? t)
    t
    (+ (* 3 (mag (first t)))
       (* 2 (mag (second t))))))

(def part1
  (mag (reduce add data)))
;; => 4173

(def part2
  (apply max
         (for [x data
               y data
               :when (not= x y)]
           (mag (add x y)))))
;; => 4706
