(ns training.exercises.ex13-multimethods
  (:require [hiccup.core :as h]))

;; ========================================
;; Multimethods
;; ========================================

(defmulti full-moon-behavior
  (fn [were-creature] (:were-type were-creature)))

(defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))

(defmethod full-moon-behavior :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies"))

(full-moon-behavior {:were-type :wolf
                     :name "Rachel from next door"})

(full-moon-behavior {:name "Andy the baker"
                     :were-type :simmons})



(def moderator-usernames ["bob" "divya" "grogthor, intergalactic warlord"])
(defmulti moderator? class)
(defmethod moderator? String
  [username] (some #(= % username) moderator-usernames))
(defmethod moderator? clojure.lang.IPersistentMap
  [m] (moderator? (:username m)))

(moderator? "bob")
(moderator? "chris")


;; can dispatch on any transformation of any or all arguments
;; say what!?
(defmulti enterprise-readiness
  (fn [person briggs-meyer] briggs-meyer))

(defmethod enterprise-readiness ["e" "n" "t" "j"]
  [person briggs-meyer]
  (str (:name person) " is enterprise ready!"))

(defmethod enterprise-readiness ["i" "s" "f" "p"]
  [person briggs-meyer]
  (str (:name person) " is not enterprise ready!"))

(enterprise-readiness {:name "Bubba"} ["i" "s" "f" "p"])



;; ========================================
;; Multimethods are good for data-driven behavior
;; ========================================

;; Hiccup converts Clojure data structures to HTML
(h/html [:input {:value "x"}])
(h/html [:textarea {:value "x"}])

(defmulti input (fn [input-type _] input-type))

(defmethod input :textarea
  [_ opts]
  [:textarea (dissoc opts :value) (:value opts)])

(defmethod input :select
  [_ {:keys [options] :as opts}]
  (let [selected-value (:value opts)]
    (into [:select (dissoc opts :options)]
          (mapv (fn [{:keys [name value]}]
                  [:option {:value value
                            :selected (= selected-value value)} name])
                options))))

(defmethod input :default
  [input-type options]
  [:input (assoc options :type input-type)])

(input :select {:value   "HI"
                :options [{:name "Alaska" :value "Ak"}
                          {:name "Hawaii" :value "HI"}]})

(input :text {:value "abc"})

;; Use data to produce a custom form
(map (fn [form-field]
       (input (:type form-field) (dissoc form-field :type)))
     [{:name "Favorite Color"
       :type :textarea
       :value "blue"}
      {:name "Favorite State"
       :type :select
       :value "HI"
       :options [{:name "Alaska" :value "Ak"}
                 {:name "Hawaii" :value "HI"}]}])

;; You try:
;; * Create a method that handles radio buttons
(defmethod input :radio
  [_ {:keys [options name value]}]
  (let [selected-value value]
    (into [:div {}]
      (mapv (fn [{:keys [value label]}]
                [ :div 
                  [ :input { :type "radio" 
                              :selected (= selected-value value)
                              :name name}]
                  [ :label label ] ])
            options))))

(h/html
  (input :radio {:value "HI"
                 :name "state"
                 :options [{:label "Alaska" :value "Ak"}
                           {:label "Hawaii" :value "HI"}]}))

;; * Create a method that handles check boxes
(defmethod input :checkboxes
  [_ {:keys [options name values]}]
  (into [:div {}]
    (mapv (fn [{:keys [value label]}]
              [ :div 
                [ :input { :type "checkbox" 
                            :selected (if (some #(= value %) values) "selected" "")
                            :name name}]
                [ :label label ] ])
          options)))

(h/html
  (input :checkboxes {:values ["HI"]
                      :name "state"
                      :options [{:label "Alaska" :value "AK"}
                                {:label "Hawaii" :value "HI"}]}))

;; * Create a method that takes :date as its type, and produces
;;   select dropdowns for month, day, and year
(defmethod input :date
  [_ {:keys [options name value]}]
  (let [ a value ]
    "sdf"))

(h/html (input :select {:value   "HI"
                        :options (mapv #{} (range 1 12))})


  input :date {:value []}))


(def double #(* 2 %))
(double 4)
(def wrap (fn [ a ] { :wrapped true :value a }))
(def wrap #{ :wrapped true :value % })
(def wrap #({ :wrapped true :value % }))
(wrap "hello")

