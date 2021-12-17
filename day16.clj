(ns org.vuxu.aoc2021.day16
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

(defn hex2bin [hex]
  (map #(Character/digit % 2)
       (mapcat #(cl-format nil "~4,'0B" (Character/digit % 16)) hex)))

(defn bin2int [bin]
  (Integer/parseInt (apply str bin) 2))

(def data
  (->> (slurp "day16")
       str/trim-newline
       hex2bin))

(declare parse-all)
(declare parse-n)

(defn parse [bin]
  (let [[version bin] (split-at 3 bin)
        [type bin] (split-at 3 bin)
        version (bin2int version)
        type (bin2int type)
        
        [content bin] (if (= type 4)
                        (loop [bin bin
                               lit 0]
                          (let [[cont bin] (split-at 1 bin)
                                [data bin] (split-at 4 bin)
                                lit (bit-or (bit-shift-left lit 4)
                                            (bin2int data))]
                            (if (= cont [1])
                              (recur bin lit)
                              [{:literal lit} bin])))
                        (let [[length-type bin] (split-at 1 bin)
                              [inner-packets bin]
                              (if (= length-type [0])
                                (let [[l bin] (split-at 15 bin)
                                      [packets bin]
                                      (split-at (bin2int l) bin)]
                                  [(parse-all packets) bin])
                                (let [[n bin] (split-at 11 bin)]
                                  (parse-n bin (bin2int n))))
                              ]
                          [{:operator inner-packets} bin]))
        ]
    [(into {:version version :type type} content)
     bin]))

(defn parse-all [bin]
  (loop [bin bin
         parsed []]
    (if (seq bin)
      (let [[item bin] (parse bin)]
        (recur bin (conj parsed item)))
      parsed)))

(defn parse-n [bin n]
  (loop [bin bin
         n n
         parsed []]
    (if (zero? n)
      [parsed bin]
      (let [[item bin] (parse bin)]
        (recur bin (dec n) (conj parsed item))))))

(defn sum-version [packet]
  (if (:literal packet)
    (:version packet)
    (+ (:version packet)
       (apply + (map sum-version (:operator packet))))))

(def part1
  (sum-version (first (parse data))))
;; => 981

(defn eval-packet [packet]
  (prn packet)
  (case (:type packet)
    0 (apply + (map eval-packet (:operator packet)))
    1 (apply * (map eval-packet (:operator packet)))
    2 (apply min (map eval-packet (:operator packet)))
    3 (apply max (map eval-packet (:operator packet)))
    4 (:literal packet)
    5 (if (> (eval-packet (first (:operator packet)))
             (eval-packet (second (:operator packet))))
        1
        0)
    6 (if (< (eval-packet (first (:operator packet)))
             (eval-packet (second (:operator packet))))
        1
        0)
    7 (if (= (eval-packet (first (:operator packet)))
             (eval-packet (second (:operator packet))))
        1
        0)))

(def part2
  (eval-packet (first (parse data))))
;; => 299227024091
