(ns org.vuxu.aoc2021.day25
  (:require [clojure.string :as str]))

(def data
  (->> (slurp "day25")
       str/split-lines
       (mapv (partial mapv identity))))

(defn step-east [data]
  (let [lx (count (first data))]
    (vec (for [y (range (count data))]
           (vec (for [x (range lx)]
                  (cond
                    (and (= (get-in data [y (mod (dec x) lx)]) \>)
                         (= (get-in data [y x]) \.))
                    \>

                    (and (= (get-in data [y x]) \>)
                         (= (get-in data [y (mod (inc x) lx)]) \.))
                    \.

                    :else
                    (get-in data [y x]))))))))

(defn step-south [data]
  (let [ly (count data)]
    (vec (for [y (range ly)]
           (vec (for [x (range (count (first data)))]
                  (cond
                    (and (= (get-in data [(mod (dec y) ly) x]) \v)
                         (= (get-in data [y x]) \.))
                    \v

                    (and (= (get-in data [y x]) \v)
                         (= (get-in data [(mod (inc y) ly) x]) \.))
                    \.

                    :else
                    (get-in data [y x]))))))))

(def part1
  (loop [i 1
         data data]
    (let [data' (step-south (step-east data))]
      (if (= data data')
        i
        (recur (inc i) data')))))
;; => 601
