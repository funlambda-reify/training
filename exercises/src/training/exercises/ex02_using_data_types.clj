(ns training.exercises.ex02-using-data-types
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; The cheatsheet is one of your best friends
;; http://clojure.org/api/cheatsheet

;; ========================================
;; Operations / Expressions
;; ========================================

(str "str is an operator that takes string" " arugments and concatenates them")
(str "testing " "one " " two")

;; ========================================
;; Control flow
;; ========================================

(if true
  "Verily your word is your bond"
  "Knave! Liar! Cur!")

(if nil "true" "false")

(if true
  (do (println "Do wraps up multiple expressions")
      (println "It's useful for expressions like if")))

(if false
  "evaluates to nil when no false expression provided")

;; Anything not false or nil is truthy
(if "bears eat beets"
  "bears beets Battlestar Galactica")

;; Use `when` when you only care about the true condition. There's an
;; implicit `do` so you can evaluate multiple side-effecting
;; expressions.
(when true
  (println "Do wraps up multiple expressions")
  (println "It's useful for expressions like if"))

;; Boolean operations
(= "a" "a" "a")
(= 0 0)

(> 1 0)
(>= 1 1)

;; Or evaluates to the first truthy value, or nil
(or :red)
(or false nil :large_I_mean_venti :why_cant_I_just_say_large)

;; And evalutes to the first falsey value, or the last truthy value
(and :free-wifi :hot-coffee)
(and nil false)

;; cond let's you test multiple conditions
(cond (= 1 0) "wow one equals zero"
      (= 1 1) "one equals one"
      :else   "is nothing true!?!?!?")

(cond (= 1 0) "abc"
      false "what"
      false "abc"
      :else 4)

;; ========================================
;; def and let
;; ========================================

;; bind a symbol to a value
(def failed-protagonist-names
  ["Larry Potter" "Doreen the Explorer" "The Incredible Bulk"])
(first failed-protagonist-names)

;; happens within a namespace
training.exercises.ex02-using-data-types/failed-protagonist-names

;; This throws an exception because the namespace is wrong:
;; clojure.core/failed-protagonist-names

;; bind symbol to a value within a new scope
(let [failed-protagonist-names ["The Human Porch" "Arachnid Man"]]
  (first failed-protagonist-names))

;; useful for 'saving' derived values
(let [x (first failed-protagonist-names)]
  (println "working with" x)
  (str/upper-case x))

;; You try:
;; * Use def, then use let to 'shadow' the def'd symbol

(def x 4)
(let [x 6
      y (+ x 3)]
     y)

;; ========================================
;; Numbers & Arithmetic
;; ========================================

(/ 3 5)
(- 10 9 8 7)

;; You try:
;; * Add 5 numbers
(+ 3 5 6 2 4 2)
;; * Multiply 3 numbers
(* 3 5 6)



;; ========================================
;; Strings
;; ========================================

(str/lower-case "And IIIIeeeIIII will always love yoooou")

;; You try:
;; * upper-case a line from your favorite song
(str/upper-case "no one man should have all that power")


;; ========================================
;; Keywords
;; ========================================
;; * Often used in maps
;; * Can use as a function

:this-is-a-keyword
:? ; <= that's a keyword too
(:first-name {:first-name ""})

(def me {:first-name "Brooks" :last-name "Noble"})
me
(:first-name me)
(-> me :first-name)

;; ========================================
;; Maps
;; ========================================

{:username "blambledirk"
 :email "blambledirk@gmail.com"
 :failed-login-attempts 3},

;; keys can be of any type
{["failures" 1]  #{"blambledirk"}
 ["failures" 10] #{"crundlemink"}}

;; Let's do things to a map
(def personal-info
  {:address {:street "10 Bumbleberry Boulevard"
             :city   "Bananapants"}
   :petname "Rocky"})
(get personal-info :address)
(get-in personal-info [:address :city])
(assoc-in personal-info [:address :city] "Barnaclecrab")
(dissoc personal-info :address)
(keys personal-info)
(vals personal-info)

(hash-map :key1 1 :key2 2)
(into {} [[:key 1] [:key2 2]])
(conj {:a 1} [:b 2]) ;; you're going to see conj a lot

(count {:a 1 :b 2})
(empty? {})
(empty? {:a 1})
(not-empty {})

;; You try:
;; * Create a map that represents someone's name
;; * Now create a nested map with :name as a key and a
;;   map representing someone's name as the value
;; * Use get to retrieve the name map
;; * Use get-in to get just the first name
;; * Use assoc-in to 'change' the first name

(def bob { :name {:first-name "Bob" :last-name "Billy"} :age 42 })
(get bob :name)
(get-in bob [:name :first-name])
(assoc-in bob [:name :first-name])


;; ========================================
;; Vectors
;; ========================================

[0 "foo" :bar]
(vector 1 2 3)
(vec {:a 1})
(into [] '(1 2 3))
(conj [1 2 3] 4) ;; appends element to "business end" of data structure
(nth [:a :b :c] 0)
(cons 1 [2 3 4]) ;; appends arg to front of data structure

(empty? [])
(not-empty [])

(concat [:a :b] [:c :d])


(vec {:a 1 :b 2})
(conj [1 2 3] 4)
(conj '(1 2 3) 4)

;; Try these functions:
;; * first
(first [ 1 2 3 ])
;; * rest
(rest [ 1 2 3 ])

;; * take 
(take 2 [ 1 2 3 ])

;; * assoc


;; * nth

(defn multiply [x y] (* x y))
(def triple (multiply 3))
(def triple #(multiply 3 %))
(triple 2)

(def double-num (fn [x] (* x 2)))
(double-num 4)
(def double-everything (fn [xs] (mapv double-num xs)))
(double-everything [1 2 3])

;; ========================================
;; Exercise: Points
;; ========================================
;; * Represent a point on plane
(def point { :x 4 :y 6 })
point

;; ========================================
;; Lists
;; ========================================

;; For literals, precede with a quote
'(whistle while you work)
'(0 "foo" :bar)
(list 0 "foo" :bar)
(into '(0 "foo" :bar) [:baz])
(conj '(0 "foo" :bar) :baz)

(concat '(:a :b) '(:c :d))

;; Try these functions:
;; * first
(first '(1 2 3))
;; * rest
(rest '(:a :b :c))
;; * empty?
(empty? '())
(empty? '(:a))

;; ========================================
;; Sets
;; ========================================

#{:pizza :fries :cholesterol}
(set [:pizza :fries :cholesterol])
(hash-set :pizza :fries :cholesterol)

(into #{:pizza :fries :cholesterol} [:pizza :burgers])
(conj #{:pizza} :pizza)

(count #{:pizza})
(empty? #{:pizza})

(set/union #{:pizza} #{:fries})
(set/intersection #{:pizza :fries} #{:pizza :burgers})

(#{:pizza :fries} :pizza)
(#{:pizza :fries} :cholesterol)

(filter #{:pizza :fries} [:pizza :fries :cholesterol :pizza])

;; Try this:
;; * Use the set/difference function
(set/difference #{:a :b} #{:a})
(set/difference #{:a} #{:a :b})
;; * Convert a set to a vector
(into [] #{:a :b})
