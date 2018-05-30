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

; TODO: Try implementing with for expression

(defn odds
    [sum sides-a sides-b]
    (let [roll-sums (->> (for [x (range 1 (inc sides-a))
                               y (range 1 (inc sides-b))] [x y])
                         (mapv (partial apply +)))
          num-correct (count (filterv #(= % sum) roll-sums))
          num-total (count roll-sums)]
      (/ num-correct num-total)))

(odds 3 6 6)
(odds 3 4 4)

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
