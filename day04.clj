(ns org.vuxu.aoc2021.day04
  (:require [clojure.string :as str]))

(let [[draws & cards] (str/split (slurp "day04") #"\n\n")]
  (def draws (->> (str/split draws #",")
                  (map parse-long)))
  (def cards (for [card cards]
               (->> card
                    str/split-lines
                    (map str/trim)
                    (mapv #(->> (str/split % #" +")
                                (mapv parse-long)))))))

(defn find-in [number card]
  (first (for [[lineno line] (map-indexed vector card)
               [rowno cand] (map-indexed vector line)
               :when (= cand number)]
           [lineno rowno])))

(defn cross [number card]
  (if-let [pos (find-in number card)]
    (update-in card pos (constantly true))
    card))

(defn won? [card]
  (when (or (some (partial every? true?) card)
            (some (partial every? true?) (apply mapv vector card)))
    card))

(defn score [card draw]
  (->> card
       flatten
       (filter number?)
       (apply +)
       (* draw)))

(def part1
  (reduce (fn [cards draw]
            (let [new-cards (map (partial cross draw) cards)]
              (if-let [winner (some won? new-cards)]
                (reduced (score winner draw))
                new-cards)))
          cards
          draws))
;; => 33348

(def part2
  (reduce (fn [cards draw]
            (let [new-cards (map (partial cross draw) cards)
                  remaining-cards (filter (complement won?) new-cards)]
              (if (empty? remaining-cards)  ; assume only one last winner
                (reduced (score (first new-cards) draw))
                remaining-cards)))
          cards
          draws))
;; => 8112
