(ns org.vuxu.aoc2021.day14
  (:require [clojure.string :as str]))

(let [[[template _] rules] (split-at 2 (str/split-lines (slurp "day14")))]
  (def template template)
  (def rules (into {} (for [rule rules]
                        (str/split rule #" -> ")))))

(defn my-interleave [even odd]
  (drop-last (interleave even (concat odd [nil]))))

(defn step [p]
  (->> (partition 2 1 p)
       (map (partial apply str))
       (map rules)
       (my-interleave p)
       (apply str)))

(def part1
  (apply - (apply (juxt max min)
                  (vals (frequencies (nth (iterate step template) 10))))))
;; => 2375


(def freq-template
  (frequencies (map (partial apply str) (partition 2 1 template))))

(defn freq-step [f]
  (apply merge-with + {}
         (mapcat (fn [[k f]]
                   [{(str (first k) (rules k)) f}
                    {(str (rules k) (second k)) f}])
                 f)))

(defn element-count [final pairs]
  (apply merge-with + {final 1}
         (map (fn [[k f]]
                {(first k) f})
              pairs)))

(def part2
  (apply - (apply (juxt max min)
                  (vals (element-count (last template)
                         (nth (iterate freq-step freq-template) 40))))))
;; => 1976896901756
