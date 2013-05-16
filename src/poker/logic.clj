(ns poker.logic)

;; Rank of Cards in Poker
(def rank-string "--23456789TJQKA")

(def poker-rank {:royal-flush     9
                :straight-flush   8
                :four-of-a-kind   7
                :full-house       6
                :flush            5
                :straight         4
                :three-of-a-kind  3
                :two-pair         2
                :one-pair         1
                :high-card        0})

(defn rank-vector "return rank vector" [hand] (map first hand))

(defn suit-vector "return suit vector" [hand] (map last hand))

;;Quick Analysis of card on basis of group
(def hand-group-analyze
  {'(4,1)       :four-of-a-kind
   '(3,2)       :full-house
   '(3,1,1)     :three-of-a-kind
   '(2,2,1)     :two-pair
   '(2,1,1,1)   :one-pair
   '(1,1,1,1,1) :check-again})

(defn count-group "count-group helper function" [hand]
  (->> hand (partition-by identity) (map count)))

(defn hand-group "retrieve hand group" [hand]
  (->> hand rank-vector sort
       count-group sort reverse))

(defn hand-sort-exceptions "special cases filter" [hand]
  (cond
   (= hand [14 5 4 3 2])           '(5 4 3 2 1)
   (= '(2,3)
      (count-group hand))           (reverse hand) 
   :else  hand))

(defn hand-sort "sort hand based on card rank"[hand]
  (->> hand rank-vector
       (map (fn [s] (.indexOf rank-string (str s))))
       sort reverse hand-sort-exceptions vec))

(defn straight? "hand is straight?" [hand]
  (let [sorted-hand (hand-sort hand)]
    (and (= 5 (count (distinct sorted-hand)))
           (= 4 (- (first sorted-hand)
                   (last sorted-hand))))))

(defn flush? "hand is flush?"[hand]
    (= 1 (->> hand suit-vector distinct count)))

(defn straight-flush? "hand is straight-flush?"[hand]
  (and (flush? hand) (straight? hand)))

(defn royal-flush? "hand is royal-flush?"[hand]
  (and (straight-flush? hand)
       (and (= 14 (->> hand hand-sort first))
            (= 10 (->> hand hand-sort last)))))

(defn poker-hand "return poker hand" [hand]
  (let [analysis (hand-group-analyze `~(hand-group hand))]
    (if (not= :check-again analysis)
      analysis
      (cond
       (royal-flush? hand)    :royal-flush
       (straight-flush? hand) :straight-flush
       (flush? hand)          :flush
       (straight? hand)       :straight
       :else                  :high-card))))

(defn poker-value "returns poker hand value as vector for comparision" [hand]
  (conj (->> hand poker-hand poker-rank list vec)
        (hand-sort hand)))

(defn poker-hands-sort "sort poker hands" [hands]
  (->> hands (sort-by poker-value) reverse))
