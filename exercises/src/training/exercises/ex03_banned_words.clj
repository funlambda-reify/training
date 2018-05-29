(ns training.exercises.ex01-total-word-count
  (:require [clojure.string :as str]))

; (ns training.exercises.ex03-banned-words
;   (:require [clojure.string :as str
;              clojure.set :as set]))

;; Your mission is to write a function that takes a string and returns
;; true if it contains banned words

; Q0: Convention is to just return value from boolean func?
; Q1: Why macro for threading vs. higher-order func?
; Q2: When to use threading macro?
; Q3: Threading macro gotchas: anonymous function needs enclosing parens
; Q4: When to use JVM methods vs clojure built-ins?

(defn contains-banned-words? 
  [banned s]
   (not-empty (clojure.set/intersection (set banned) (set (str/split s #" ")))))

(defn contains-banned-words? 
  [banned s] 
  (let [words (str/split s #" ")]
       (not-empty (clojure.set/intersection (set banned) (set words)))))

(defn contains-banned-words?
  [banned s]
  (->> s
       (#(str/split % #" "))
       (mapv s/lower-case)
       set
       (clojure.set/intersection (->> banned (mapv str/lower-case) set))
       not-empty
  ))

(defn contains-banned-words?
  [banned s]
  (->> s
       ((fn [s] (str/split s #" ")))
       (mapv str/lower-case)
       set
       (clojure.set/intersection (->> banned (mapv str/lower-case) set))
       not-empty
  ))

(contains-banned-words?
  [ "shoot" "crap" ] 
  "What the crap is that?")

(contains-banned-words?
  [ "shoot" "crap" ] 
  "What the heck is that?")
