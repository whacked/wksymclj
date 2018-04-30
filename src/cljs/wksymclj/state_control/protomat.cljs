(ns wksymclj.state-control.protomat
  (:require [cljs.core.async :refer [chan <! >! close!
                                     mult tap untap untap-all
                                     pub sub unsub unsub-all]]
            [wksymclj.nodejs-interface.time :refer [now-ms]]
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.data-manipulation.collection :as wk-coll])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]))

;; mini state management logic inspired by ztellman/automat

;; <common>
(def state (atom {}))

(def world (atom nil))

(defn set-time-trigger!
  ([nil-op]
   (when-let [itv (get-in @state [:time-trigger :itv])]
     (js/clearTimeout itv))
   (swap! state assoc :time-trigger {}))
  ([op time f]
   (when-let [itv (get-in @state [:time-trigger :itv])]
     (js/clearTimeout itv))
   (let [[rel-time abs-time] (case op
                               :at [(- time (now-ms)) time]
                               :in [time (+ time (now-ms))])
         next-timer (->> rel-time
                         (js/setTimeout
                          (fn []
                            (set-time-trigger! nil)
                            (f))))]
     (swap! state assoc :time-trigger {:at abs-time
                                       :itv next-timer}))))

(defn set-state-time-now! [& ks]
  (swap! state merge
         (let [t (now-ms)]
           (zipmap ks (repeat (count ks) t)))))

(defn get-time-until-trigger [& [now]]
  (let [now (or now (now-ms))
        get-sec (fn [k]
                  (if-let [t (k @state)]
                    (.toFixed (/ (- now (k @state)) 1000) 1)))]
    (if-let [trigger-time (get-in @state [:time-trigger :at])]
      (- trigger-time now))))
;; </common>

;; <event management>
(def task-chan (chan 1))
;; (close! task-chan)

(defn trigger! [op & {:keys [] :or {} :as msg}]
  (go (>! task-chan (assoc msg :op op))))

(defn start-event-loop! [dispatch-msg!]
  (go-loop []
    (when-let [msg (<! task-chan)]
      (dispatch-msg! msg)
      (recur))))

(defn dlog [& args]
  (apply js/console.log args))

(defn make-stepper [custom-spec]
  (let [ch (chan 1)
        spec (merge {:pre (fn [self w] (dlog "%cPRE" "color:orange;"))
                     :run (fn [self w] (dlog "%cRUN" "color:green;"))
                     :post (fn [self w]
                             (dlog "%cPOST" "color:blue;"))
                     
                     :-destroy! (fn [self w]
                                  (dlog "%cDESTROY" "color:red;")
                                  (dlog "WORLD IS NOW")
                                  (dlog (clj->js w))
                                  (close! ch))
                     :-channel ch
                     }
                    custom-spec)]
    (assoc spec
           :runall
           (let [xns {:pre :run
                      :run :post
                      :post :-destroy!}
                 trigger-xns! (fn [state-name new-world]
                                (go (>! ch [state-name new-world])))]
             (fn [world]
               (go-loop []
                 (when-let [[end-state world'] (<! ch)]
                   (let [next-state (xns end-state)
                         next-processor (spec next-state)]
                     (trigger-xns! next-state
                                   (or (next-processor spec world')
                                       world')))
                   (recur)))
               (trigger-xns! :pre ((:pre spec) spec world)))))))

(defrecord StateMatcher [value reducer])

;; this state is special
(def a--status (atom {:accepted? false
                      :checkpoint nil
                      :state-index 0
                      :start-index 0
                      :stream-index 0 ;; this advances with stream
                      :value nil}))

(defn a--$ [reducer-key]
  {:-reducer reducer-key})

(defn a--compile [state-sequence
                  opt]
  ;; this is the fsm
  (assoc opt
         :match-seq
         (loop [ss (flatten state-sequence)
                out []]
           (if (empty? ss)
             out
             (let [item (first ss)]
               (recur (rest ss)
                      (if-let [rd-key (:-reducer item)]
                        (conj (vec (butlast out))
                              (StateMatcher. (:value (last out))
                                             rd-key))
                        (conj out (StateMatcher. item nil)))))))))

(defn a--find [machine init-state stream]
  (let [world init-state
        *a--history* (atom [])]
    (dlog "In find\n")
    (let [get-signal (:signal machine)
          match-state-seq (:match-seq machine)
          match-value-seq (map :value match-state-seq)
          signal-list (map get-signal
                           (drop (:stream-index @a--status) stream))

          match-size (count match-state-seq)
          ]
      (loop [start-idx (count @*a--history*)
             match-length 0
             user-state init-state
             queue []
             input-stream (drop (count @*a--history*) stream)]
        (if-not (seq input-stream)
          ;; stop condition for failure
          @a--status
          (let [stream-input (first input-stream)
                stream-signal (get-signal stream-input)
                ]
            (dlog (str stream-signal))
            (dlog (str "searching stream "
                       (count input-stream)
                       " "
                       stream-signal
                       " -- "
                       (first match-value-seq)
                       " QUEUE: "
                       (count queue)
                       " <si> "
                       ))
            ;; (dlog (clj->js input))
            (let [idx-match-seq-end
                  (loop [idx-match-seq 0]
                    (if (and (< idx-match-seq match-size)
                             (< idx-match-seq (count input-stream))
                             (= (nth match-value-seq idx-match-seq)
                                (get-signal (nth input-stream idx-match-seq))))
                      (do
                        (dlog (str ">>> RUNREDUCE"
                                   ))
                        (recur (inc idx-match-seq)))
                      idx-match-seq))]
              (dlog
               (str "MATCH SIZE: " idx-match-seq-end))
              
              (if (= idx-match-seq-end match-size)
                ;; have a match, run reducers
                (let [
                      ;; NOTE!
                      ;; need to store a reducer map to monitor
                      ;; state changes per input. this implies that
                      ;; every state with the same signal name has
                      ;; the same reducer!!! which is an overly
                      ;; strong assumption. but for our purposes of
                      ;; mimicking automat it makes design simpler.
                      ;; you just have to remember this yourself.
                      reducer-map (assoc (:reducers machine)
                                         :-noop (fn [state input] state))
                      ]
                  ;; (dlog "%c** ** ** run reducers" "color:white;background:red;")
                  ;; (dlog (clj->js reducer-map))
                  ;; (dlog "RESULTANT STATE")
                  (assoc @a--status
                         :state-index (dec idx-match-seq-end)
                         :stream-index (+ start-idx (dec idx-match-seq-end))
                         :accepted? true
                         :value (loop [idx 0
                                       state init-state]
                                  (if-not (< idx idx-match-seq-end)
                                    state
                                    (let [reducer (reducer-map
                                                   (or (:reducer (nth match-state-seq idx))
                                                       :-noop))
                                          input (nth input-stream idx)]
                                      (dlog (str
                                             "COMPARE: "
                                             (clj->js (nth match-value-seq idx))
                                             " -- "
                                             (clj->js (get-signal input))))
                                      (recur (inc idx)
                                             (reducer state input)))))))
                (recur (+ start-idx 1 idx-match-seq-end)
                       match-length
                       user-state
                       (take-last match-size (conj (vec queue) stream-signal))
                       (rest input-stream))))
            
            )))
      
      )))
;; </event management>