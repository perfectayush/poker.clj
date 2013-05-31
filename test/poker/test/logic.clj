(ns poker.test.logic
  (:use clojure.test
        poker.logic))

(deftest test-poker-hand
  (is (= :high-card          (poker-hand ["7H" "1C" "9S" "TS" "JS"])))
  (is (= :one-pair           (poker-hand ["7H" "7C" "9S" "TS" "JS"])))
  (is (= :two-pair           (poker-hand ["7H" "7C" "9S" "9D" "JS"])))
  (is (= :three-of-a-kind    (poker-hand ["7H" "7C" "9S" "7S" "JS"])))
  (is (= :straight           (poker-hand ["7H" "8C" "9S" "TS" "JD"])))
  (is (= :straight           (poker-hand ["AH" "2C" "3S" "4S" "5D"])))
  (is (= :straight           (poker-hand ["AH" "KC" "JS" "QS" "TD"])))
  (is (= :flush              (poker-hand ["7S" "6S" "9S" "TS" "JS"])))
  (is (= :full-house         (poker-hand ["7H" "7C" "9S" "9C" "9D"])))
  (is (= :four-of-a-kind     (poker-hand ["7H" "7C" "7S" "7D" "JS"])))
  (is (= :straight-flush     (poker-hand ["7H" "8H" "9H" "TH" "JH"])))
  (is (= :straight-flush     (poker-hand ["AD" "2D" "3D" "4D" "5D"])))
  (is (= :royal-flush        (poker-hand ["TS" "KS" "QS" "JS" "AS"]))))
   
