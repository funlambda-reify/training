(ns blackjack.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [blackjack.util :as util]))

(defn hand->string
  [hand]
  (str/join ", " (map (fn [ [ suit rank ] ] (str suit rank)) hand)))

(def starting-money 1000)

(defn new-deck
  []
  (apply list
    (shuffle
      (for [ suit [ "♠" "♥" "♦" "♣" ]
            rank [ "A" 2 3 4 5 6 7 8 9 10 "J" "K" "Q" ] ]
        [ suit rank ]))))

; TODO: Deal with empty deck scenario better
(defn deal-cards
  "Deals n cards from the input deck. Returns dealt cards and resulting deck."
  ([ n deck ] (deal-cards n deck []))
  ([ n deck dealt-cards ]
    (if (= n 0)
      [ dealt-cards deck ]
      (let [next-card (first deck)]
        (if next-card
          (recur (dec n) (rest deck) (conj dealt-cards next-card))
          (throw (Exception. "No cards left in deck!")))))))

(defn possible-card-scores
  "Returns the possible scores for a card. Number cards of rank n return [ n ],
   face cards return [ 10 ], and aces return [ 1 11 ]."
  [ [ suit rank ] ] 
  (if (= rank "A") [ 1 11 ]
    (if (contains? #{ "J" "Q" "K" } rank) [ 10 ] ; review this
      [ rank ])))

(defn possible-hand-scores
  "Returns the possible scores for a collection of cards (a hand)."
  ([hand] (possible-hand-scores hand [0]))
  ([hand acc]
    (if (empty? hand) 
      acc
      (let [next-card-possible-scores (possible-card-scores (first hand))]
        (recur (rest hand) (reduce concat (map (fn [ps1] (map (fn [ps2] (+ ps1 ps2)) next-card-possible-scores)) acc)))))))

(defn get-valid-scores
  "Filters a collection of blackjack scores down to valid (non-busting) scores (<= 21)"
  [scores]
  (filter #(<= % 21) scores))

; statuses: :waiting-for-bet, :ready-to-deal, :player-turn, :dealer-turn, :dealer-done
; actions: :make-bet($), :deal-hands, :player-hit, :player-stand, 
;          :dealer-play

(def initial-state 
  {
    :status :waiting-for-bet
    :round-result nil
    :player-money starting-money
    :current-bet nil
    :deck (new-deck)
    :player-hand nil
    :player-poss-scores nil
    :dealer-hand nil
    :dealer-poss-scores nil
  })

(defn require-status 
  [state status]
    (if (not= (:status state) status) 
        (throw (Exception. (str "Invalid action for current game state. Expected " status " but got " (:status state))))))

(defmulti handle-action (fn [ [ action-type ] state ] action-type))
(defmethod handle-action :start-new-round
  [ [ _ bet-amount ] state ]
  (require-status state :round-over)
  (assoc state
    :status :waiting-for-bet))
(defmethod handle-action :make-bet
  [ [ _ bet-amount ] state ]
  (require-status state :waiting-for-bet)
  (assoc state 
    :status :ready-to-deal 
    :current-bet bet-amount
    :player-money (- (:player-money state) bet-amount)))
(defmethod handle-action :deal-hands
  [ [ _ ] state ]
  (require-status state :ready-to-deal)
  (let [[player-hand deck] (deal-cards 2 (:deck state))
        [dealer-hand deck] (deal-cards 2 deck) ]
    (assoc state
      :status :player-turn
      :deck deck
      :player-hand player-hand
      :player-poss-scores (get-valid-scores (possible-hand-scores player-hand))
      :dealer-hand dealer-hand
      :dealer-poss-scores (get-valid-scores (possible-hand-scores dealer-hand)))))
(defmethod handle-action :player-hit
  [ [ _ ] state ]
  (require-status state :player-turn)
  (let [[ player-new-cards deck] (deal-cards 1 (:deck state))
          player-hand (concat (:player-hand state) player-new-cards)
          poss-hand-scores (get-valid-scores (possible-hand-scores player-hand))
          player-busted (empty? poss-hand-scores) ]
    (assoc state
      :status (if player-busted :winner-determined :player-turn)
      :round-result (if player-busted [ :player-lost :player-busted ] nil)
      :deck deck
      :player-hand player-hand
      :player-poss-scores poss-hand-scores)))
(defmethod handle-action :player-stand
  [ [ _ ] state ]
  (require-status state :player-turn)
  (assoc state :status :dealer-turn))
(defmethod handle-action :dealer-play
  [ [ _ ] state ]
  (require-status state :dealer-turn)
  (loop [state state] ; Take cards until bust or (highest poss.) dealer score is >= 17
    (if (or (empty? (:dealer-poss-scores state)) 
            (>= (last (:dealer-poss-scores state)) 17))
        (let [ dealer-busted (empty? (get-valid-scores (possible-hand-scores (:dealer-hand state)))) ]
          (assoc state 
            :status (if dealer-busted :winner-determined :dealer-done)
            :round-result (if dealer-busted [ :player-won :dealer-busted ] nil)))
        (let [ [dealer-new-cards deck] (deal-cards 1 (:deck state))
                dealer-hand (concat (:dealer-hand state) dealer-new-cards)
                poss-hand-scores (get-valid-scores (possible-hand-scores dealer-hand)) ]
          (recur
            (assoc state
              :deck deck
              :dealer-hand dealer-hand
              :dealer-poss-scores poss-hand-scores))))))
(defmethod handle-action :compare-scores
  [ [ _ ] state ]
  (require-status state :dealer-done)
  (let [player-score (last (:player-poss-scores state))
        dealer-score (last (:dealer-poss-scores state))]
    (assoc state
      :status :winner-determined
      :round-result (cond (> player-score dealer-score) [ :player-won :higher-score ]
                          (< player-score dealer-score) [ :player-lost :higher-score ]
                          :else [ :tied ]))))
(defmethod handle-action :settle-money
  [ [ _ ] state]
  (require-status state :winner-determined)
  (assoc state
    :status :round-over
    :current-bet 0
    :player-money (case (first (:round-result state))
                      :player-won (+ (:player-money state) (* 2 (:current-bet state)))
                      :tied (+ (:player-money state) (:current-bet state))
                      :player-lost (:player-money state))))
(defmethod handle-action :default
  [ action _ ] 
  (throw (Exception. (str "Unsupported action: " action))))


(defn print-game
  [ state hide-dealer-card ]
  (let [ display-dealer-cards (if hide-dealer-card [ (first (:dealer-hand state)) ] (:dealer-hand state))]
    (println "-----")
    (println "Your hand: " (str (hand->string (:player-hand state)) " (" (str/join " or " (:player-poss-scores state)) ")"))
    (println (str "Dealer hand: " (hand->string display-dealer-cards) (if hide-dealer-card " ??" "")))
    (println (str "Bet: " (:current-bet state)))
    (println "-----")))

(defmulti handle-io-for-state (fn [ { :keys [ status ]}] status))
(defmethod handle-io-for-state :waiting-for-bet
  [ state ]
  (println (str "You have " (:player-money state)))
  (let [response (util/ask-until-valid-response "How much do you want to bet? " util/parse-int)]
    [:make-bet response]))
(defmethod handle-io-for-state :ready-to-deal
  [ state ]
  (println (str "Dealing now..."))
  (Thread/sleep 2000)
  [:deal-hands])
(defmethod handle-io-for-state :player-turn
  [ state ]
  (print-game state true)
  (let [response (util/ask-until-valid-response "(h)it or (s)tand?" (partial util/parse-one-of [ "h" "s" ]))]
      (cond 
        (= response "h") [ :player-hit ]
        (= response "s") [ :player-stand ]
        :else nil)))
(defmethod handle-io-for-state :dealer-turn
  [ state ]
  (println "Dealer playing...")
  (Thread/sleep 2000)
  [:dealer-play])
(defmethod handle-io-for-state :dealer-done
  [ state ]
  (println "Dealer done playing. Comparing scores...")
  [ :compare-scores ])
(defmethod handle-io-for-state :winner-determined
  [ state ]
  (print-game state false)  
  (let [[ result reason ] (:round-result state)
        result-msg (case result :player-won "You won" :tied "Game tied" :player-lost "You lost")
        reason-msg (case reason nil "" :player-busted "because you busted" :higher-score "because of higher score" :dealer-busted "because dealer busted")]
    (println (str result-msg " " reason-msg)))
  (println "Settling money...")
  (Thread/sleep 2000)
  [ :settle-money ])
(defmethod handle-io-for-state :round-over
  [ state ]
  (println "Round over.")
  (let [response (util/ask-until-valid-response "Play another round? (y/n)" (partial util/parse-one-of [ "y" "n" ]))]
    (if (= response "y") [ :start-new-round ] nil)))
(defmethod handle-io-for-state :default
  [ state ]
  (println "Handle IO: unsupported state." (:status state))
  nil)

(defn game-loop
  [initial-state]
  (loop [ state initial-state ]
    (let [next-action (handle-io-for-state state)]
      (if (= next-action nil) "" (recur (handle-action next-action state))))))

(defn -main
  []
  (game-loop initial-state))
