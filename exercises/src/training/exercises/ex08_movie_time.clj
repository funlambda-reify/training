(ns training.exercises.ex08-movie-time
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; This function includes some stuff we haven't larned yet; don't
;; worry about that for now. We only need to use this to load the
;; movie data.
(defn get-movies
  []
  (->> (io/resource "ex08-movie-time/ratings.list")
       slurp
       str/split-lines
       (map #(str/split % #"\s{2,}"))
       (map (fn [[_ _ votes rating title]]
              {:votes  (Integer. votes)
               :rating (Double. rating)
               :title  (str/replace title #"\"" "")}))))

(def movies (get-movies))
(first movies)
;; Each movie is a map like:
{:votes  4130005
 :rating 8.3
 :title  "Tears of a Clown"}


;; Exercises:
;; * Get the highest ratest movie with more than 1000 votes.
;;   Extra credit: to break a tie, select the title with more votes.
(def test (comp (partial * -1) :votes))
(test (first movies))

(def min-votes 1000)
(->> movies
     (filter #(> (:votes %) min-vote-num))
     (reduce (fn [ highest x ] 
               (if (> (:rating x) (:rating highest))
                  x
                  (if (= (:rating x) (:rating highest))
                    (if (< (:votes x) (:votes highest)) x highest)
                    highest)))))

;; * Get all movies rated above x with at least y votes, sorted by rating.
;    WARNNIG: make sure to use a high minimum vote count or your REPL will freeze
;;   while it tries to  print all the results
(defn filter-and-sort-movies
  [[min-rating min-votes] movies]
    (->> movies
      (filter #(and (> (:rating %) min-rating) 
                    (>= (:votes %) min-votes)))
      (sort-by :rating)))

(take 10 
  (map (fn [x] [ (:title x) (:rating x) ])
    (filter-and-sort-movies [ 8.0 500 ] movies)))

;; * Get a count of all movies rated above x and below y with at least z votes
(defn count-movies
  [[[min-rating max-rating] min-votes] movies]
  (->> movies
    (filter (fn [{ :keys [ rating votes ] }]
               (and (>= rating min-rating) 
                    (<= rating max-rating)
                    (>= votes min-votes)))
    count))

(count-movies [ [ 7.0 8.0 ] 1000 ] movies)


(def add #(+ %1 %2))
(add 2 4)

;; * Calculate the average movie rating across all movies
(defn average-rating
  [movies]
  (let [rating-total (reduce (fn [total { :keys [ rating ] }] (+ total rating)) 0.0 movies)
        movie-count (count movies)]
        (/ rating-total movie-count)))

(average-rating movies)


;; Tips:
;; * Don't just type `movies` in your REPL, it will attempt to
;    print all of them and there are thousands
;; * However, you can interact with the movies data with functions
;;   like `(first movies)`, `(count movies)` 
