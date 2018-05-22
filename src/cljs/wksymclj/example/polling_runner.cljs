(ns wksymclj.example.polling-runner
  (:require [goog.dom :as gdom]
            [wksymclj.nodejs-interface.time :refer [now-ms]]
            [wksymclj.state-control.polling :as wk-polling]
            [wksymclj.state-control.protomat :as protomat]
            [reagent.core :as r]))

;; SLPM-based lifecycle agent.
;; simulates something that changes state over time, and finally
;; settling onto an end state
(def $boaty-lifecycle
  [:order-boat
   :prepare-boat
   :deliver-boat
   :sail-boat
   :park-boat
   :boat-is-parked])

(def completion-pattern
  (protomat/slpm--build
   [:boat-is-parked] 
   identity
   protomat/slpm--default-reducer
   (fn complete [end-state]
     (do
       ;; do something now we're all done?
       nil)
     (assoc end-state :value :finish))))

(def $_fn-default-advancer
  (protomat/slpm--make-default-advancer $boaty-lifecycle))

(def ordered-pattern-seq
  [completion-pattern
   $_fn-default-advancer])

(defn make-live-agent [timescale]
  (let [get-next-interval (fn []
                            (+ timescale
                               (rand-int timescale)))
        
        visited-states (atom [])
        run-step!
        (fn run-step! []
          (let [world (protomat/slpm--match-with-precedence
                       ordered-pattern-seq {} @visited-states)]
            (when-let [next-state (get-in world [:value :next])]
              (js/setTimeout
               (fn [& _]
                 (do
                   ;; do something with next-state?
                   nil)
                 (swap! visited-states conj next-state)
                 (run-step!))
               (get-next-interval)))))]
    (swap! visited-states
           conj (first $boaty-lifecycle))
    (run-step!)

    (fn query-state []
      (last @visited-states))))


(comment
  (let [query-interval 1000
        queryable-agent (make-live-agent
                         ;; simulate field latency based
                         ;; on our accepted polling time
                         (* 2.5 query-interval))
        fake-async-get-state (fn [callback]
                               (js/setTimeout
                                (fn []
                                  (callback
                                   (queryable-agent)))
                                (rand-int (+ query-interval
                                             (rand-int query-interval)))))

        print! (fn [label & ss]
                 (println
                  (apply str label
                         (-> (js/Date.)
                             (.toISOString))
                         " " ss)))

        vanilla-log (partial print! "[VANILLA] ")
        remain-n-query (atom 20)
        run-vanilla-poller!
        (fn run-vanilla-poller! []
          (fake-async-get-state
           (fn [current-state]
             (vanilla-log "state is: " current-state)
             (swap! remain-n-query dec)
             (when (and (< 0 @remain-n-query)
                        (not= current-state :boat-is-parked))
               (js/setTimeout
                run-vanilla-poller!
                query-interval)))))

        chocola-log (partial print! "[CHOCOLA] ")
        chocola-poller
        (wk-polling/make-poller
         query-interval
         (fn [push-state! current-state]
           (fake-async-get-state
            (fn [state-value]
              (push-state! state-value))))
         :state-atom (r/atom nil)
         :pre-start (fn [state]
                      (chocola-log "before start!")
                      (println state))
         :on-update (fn [state]
                      (let [history-size (count (:history state))
                            last-event (last (:history state))
                            last-value (:data last-event)]
                        (chocola-log "state is: "
                                     last-value
                                     "; history has "
                                     history-size
                                     " items now")
                        (when (or (<= 100 history-size)
                                  (= last-value :boat-is-parked))
                          (chocola-log "stop condition reached")
                          (assoc state :should-stop? true))))
         :post-stop (fn [state]
                      (chocola-log "after finish! END STATE:\n...")
                      (println state)))]

    (run-vanilla-poller!)
    
    (when-let [target-el (gdom/getElement "panel-A")]
      (r/render
       [(fn []
          [:div
           {:style {:width "200px"
                    :height "200px"
                    :border (str "6px solid "
                                 (if (:is-started? @chocola-poller)
                                   "green"
                                   "gray"))
                    :background (if (:is-stopped? @chocola-poller)
                                  "red"
                                  "skyblue")}}
           [:div
            (str "started at "
                 (:t0 @chocola-poller))]
           [:div
            (str (:request-count @chocola-poller) " requests")]
           [:div
            (str (:response-count @chocola-poller) " responses")]
           [:div
            (str (count (:history @chocola-poller))
                 " items in history")]
           [:div
            (str "latest state: "
                 (-> (:history @chocola-poller)
                     (last)
                     (:data)))]])]
       target-el))
    
    ((:start! @chocola-poller))))
