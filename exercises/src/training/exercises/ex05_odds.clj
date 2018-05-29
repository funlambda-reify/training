(ns training.exercises.ex05-odds)

;; Write a function (and any helper functions) that calculates the
;; odds of a sum being returned when two n-sided dice are thrown.
;;
;; Examples:
;; (odds 3 6 6) => 1/18
;; (odds 3 4 4) => 1/6
;;
;; In both these examples, 3 is the sum. The other two numbers are the
;; number of sides the dice have.
;;
;; It's definitely doable using just the functions and operators that
;; you've seen so far. Have fun :)

; Q1: Should (odds 3 4 4) return 1/6 or 1/8?
; Q2: Review each version and discuss.

(defn odds
    [sum sides-a sides-b]
    (->> (range 1 (inc sides-a))
         (mapv (fn [a] (mapv (fn [b] [a b]) (range 1 (inc sides-b)))))
         (reduce concat)
         (mapv (fn [[a b]] (+ a b)))
         ((fn [roll-sums] 
            (let [num-correct (count (filterv #(= % sum) roll-sums))
                  num-total (count roll-sums)]
                 (/ num-correct num-total))))))

(odds 3 6 6)
(odds 3 4 4)

(defn odds-2
    [sum sides-a sides-b]
    (let [dice-a-possibilties (range 1 (inc sides-a))
          dice-b-possibilties (range 1 (inc sides-b))
          combined-possibilities (reduce concat (mapv (fn [a] (mapv (fn [b] [a b]) dice-b-possibilties)) dice-a-possibilties))
          sums (mapv (fn [[a b]] (+ a b)) combined-possibilities)
          total-count (count sums)
          correct-count (count (filterv #(= % sum) sums))]
         (/ correct-count total-count)))

(odds-2 3 6 6)
(odds-2 3 4 4)

(defn odds-3
    ([ sum sides-a sides-b [ i j num-matches num-total ] ]
        (let [new-num-matches (+ num-matches (if (= (+ i j) sum) 1 0))
              new-num-total (inc num-total)
              is-done (and (= i sides-a) (= j sides-b))
              new-i (if (= j sides-b) (inc i) i)
              new-j (if (= j sides-b) 1 (inc j))]
              (if is-done 
                (/ new-num-matches new-num-total)
                (recur sum sides-a sides-b [ new-i new-j new-num-matches new-num-total ])
             )))
    ([ sum sides-a sides-b]
        (odds-3 sum sides-a sides-b [ 1 1 0 0 ])))

(odds-3 3 6 6)
(odds-3 3 4 4)

(defn odds-4
    [ sum sides-a sides-b ]
    (loop [ i 1
            j 1
            num-matches 0
            num-total 0 ]
      (let [new-num-matches (+ num-matches (if (= (+ i j) sum) 1 0))
            new-num-total (inc num-total)
            is-done (and (= i sides-a) (= j sides-b))
            new-i (if (= j sides-b) (inc i) i)
            new-j (if (= j sides-b) 1 (inc j))]
            (if is-done 
              (/ new-num-matches new-num-total)
              (recur new-i new-j new-num-matches new-num-total)
             ))))

(odds-4 3 6 6)
(odds-4 3 4 4)
