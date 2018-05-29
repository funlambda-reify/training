(ns training.exercises.ex01-total-word-count
  (:require [clojure.string :as s]))

;; Given a string, count the total number of words it has

(defn total-words
  [str]
  (count (s/split str #" ")))

(s/split "this has four words" #" ")

(total-words "this has four words")
