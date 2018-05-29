(ns training.exercises.ex09-review-core)

;; You try:
;; * Convert a vector to a set
(def v [ 2 :blah "hello" "hello" "hello" 3 2 :blah  ])
v
(def v-set (set v))
v-set

;; * Use assoc on a map
(def some-map { :key-a "value-a" :key-b 32 :key-c :blah })
some-map
(def new-map (assoc some-map :new-key "val..."))
new-map

;; * Implement complement
(defn my-complement 
    [f]
    (fn [ & args ] (not (apply f args))))
(def odd?' (my-complement even?))

(odd?' 1)
(odd?' 2)
  
;; * Implement comp
(defn my-comp [f1 f2]
    (fn [ & params ] (f1 (apply f2 params))))

(defn norm [ x y ] (+ (* x x) (* y y)))
(def inside-unit-circle (my-comp #(<= % 1) norm))
(inside-unit-circle 2 2)
(inside-unit-circle 0.5 0.5)
(inside-unit-circle -0.5 -0.5)
(inside-unit-circle -0.5 -2)

