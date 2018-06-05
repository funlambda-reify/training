(ns blackjack.util
    (:gen-class))

(defn parse-int [s]
  (try (Integer/parseInt s)
    (catch Exception e nil)))

(defn parse-one-of [options s]
  (if (some #{s} options) s nil))

(defn ask-until-valid-response
  [question parse-func]
  (loop [] 
    (println question)
    (let [parsed (parse-func (read-line))]
      (if (= parsed nil)
        (do (println "Invalid input!") (recur))
        parsed))))
