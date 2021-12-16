(ns org.vuxu.aoc2021.day15
  (:require [clojure.string :as str]))

(defn parse-digit [char]
  (Character/digit char 10))

(def data
  (->> (slurp "day15")
       (str/split-lines)
       (mapv (partial mapv parse-digit))))

(defn put-in! [array [x y] v]
  (assoc! (get array x) y v))

(defn find-path [m]
  (let [d (mapv transient (map (partial mapv (constantly 0)) m))
        l (count m)]
    (put-in! d [0 0] (get-in m [0 0]))
    (loop [q (conj clojure.lang.PersistentQueue/EMPTY [0 1] [1 0])]
      (if (empty? q)
        (- (get-in d [(dec l) (dec l)])
           (get-in d [0 0]))
        (let [[x y] (peek q)
              upper (if (and (> x 0) (not= 0 (get-in d [(dec x) y])))
                      (get-in d [(dec x) y])
                      ##Inf)
              bottom (if (and (< x (dec l)) (not= 0 (get-in d [(inc x) y])))
                       (get-in d [(inc x) y])
                       ##Inf)
              left   (if (and (> y 0) (not= 0 (get-in d [x (dec y)])))
                       (get-in d [x (dec y)])
                       ##Inf)
              right  (if (and (< y (dec l)) (not= 0 (get-in d [x (inc y)])))
                       (get-in d [x (inc y)])
                       ##Inf)
              minpath (+ (get-in m [x y]) (min upper bottom left right))
              ]
          (if (or (zero? (get-in d [x y]))
                  (> (get-in d [x y]) minpath))
            (do
              (put-in! d [x y] minpath)
              (recur (cond-> (pop q)
                       (> x 0) (conj [(dec x) y])
                       (< x (dec l)) (conj [(inc x) y])
                       (> y 0) (conj [x (dec y)])
                       (< y (dec l)) (conj [x (inc y)]))))
            (recur (pop q))))))))
                   

(def part1
  (find-path data))

(def data2
  (let [rows (count data)
        cols (count (first data))]
    (vec
     (for [i (range (* 5 (count data)))]
       (vec (for [j (range (* 5 (count (first data))))]
              (let [qi (quot i rows)
                    qj (quot j cols)]
                (-> (get-in data [(rem i rows) (rem j cols)])
                    dec (+ qi qj) (mod 9) inc))))
       ))))

(def part2
  (find-path data2)
;; => 2966
