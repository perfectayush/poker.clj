(ns poker.deck
  (:use [clojure.math.combinatorics :only [combinations]])
  (:require [poker.logic :as logic]))

(def ranks "23456789TJQKA")
(def suits "SCHD")

;;reference deck
(def ref-deck (for [x ranks y suits] (str x y)))

(defn deal "shuffle the deck" [] (shuffle (vec ref-deck)))

(defn texas-deal "return a map of a texas hold'em card deal for n players"[n]
  (let [deck (deal)
        [x y] (first (split-at 2 (partition n deck)))
        phands (partition 2 (interleave x y))
        rest-deck (drop (* 2 n) deck)
        flop  (take 3 (drop 1 rest-deck))
        turn  (first  (drop 5 rest-deck))
        river (first  (drop 7 rest-deck))]
    {:deck deck,
     :phands (into {}
             (map vector (map keyword (for [x (rest (range (inc n)))]
                                        (str "player-" x )))
               phands)),
     :rest-deck rest-deck,
     :flop flop, :turn turn, :river river}))

(defn hand-combinations "return all combinations between a players hand and a set of face-up cards" [hands board comb]
  (into {} (for [[k v] hands]
             [k (combinations (apply conj v board) comb)])))

(defn hands-combinations "return all combinations for all player's hand and all face-up cards state" [tmap]
  (let [phands (:phands tmap)
        flop   (:flop tmap)
        turn   (:turn tmap)
        river  (:river tmap)]
  {:flop (hand-combinations phands  flop 5)
   :turn (hand-combinations phands  (conj flop turn) 5)
   :river (hand-combinations phands (conj flop turn river) 5)}))

(defn hand-status-map "return best combination for all player's hand and all face-up cards state" [tmap board-key]
  (let [h (hands-combinations tmap)]
    (into {} (for [[k v] (board-key h)]
      [k (->> v (sort-by logic/poker-value) reverse first)]))))

(defn winner "test function to return a winner for a texas hold'em deal" [tmap board-key]
(let [s (hand-status-map tmap board-key)]
  (->> (sort-by val compare (zipmap (keys s) (map logic/poker-value (vals s)))) reverse first key)))
