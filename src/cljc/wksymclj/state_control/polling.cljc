(ns wksymclj.state-control.polling
  #?(:cljs
     (:require-macros
      [cljs.core.async.macros :refer [go go-loop]]))
  (:require #?(:clj
               [clojure.core.async
                :refer [chan go go-loop
                        <! >! timeout close! dropping-buffer]])
            #?(:cljs
               [cljs.core.async
                :refer [chan
                        <! >! timeout close! dropping-buffer]])))

;; util
#?(:clj
   (do
     (defn now-ms []
       (System/currentTimeMillis))

     (defn make-uuid []
       (str (java.util.UUID/randomUUID)))))

#?(:cljs
   (do
     (defn now-ms []
       (-> (js/Date.)
           (.getTime)))

     (defn make-uuid []
       (cljs.core/random-uuid))))

(def $poller-pool (atom {}))

(defrecord PollerEvent
    [time data])

(defrecord PollerState
    [id           ;; uuid str
     channel      ;; async channel
     history      ;; seq
     interval     ;; int
     count        ;; int
     start!       ;; fn
     should-stop? ;; bool
     is-started?  ;; bool
     is-stopped?  ;; bool
     t0           ;; int (timestamp)
     ])

(defn make-poller
  "
  takes a regular polling interval and a callable, and returns
  a polling object atom with a start method bound to `:start!`

  `polling-interval`: `int` representing the regular interval. may
                      be overwritten externally by modifying the
                      `PollerState` atom's `:interval` field
  `polling-callable`: the callable that is called regularly to poll
                      for state updates. It _must_ work this way:
                      (fn [push-state!:fn current-state:map]
                       ;; wrap your async state getter call in here
                       (your-async-state-getter  ;; e.g. AJAX
                        (fn your-state-getter-callback [new-data]
                         ;; current state *before* any update
                         (println current-state)
                         ;; record data and maybe transition
                         (push-state! new-data)))
  
  optional keyword args:
  3 state functions: they take 1 argument, the entire current state,
                     prior to any state update
  `pre-start`: fires before the first poll. Can, but should not,
               modify PollerState.
  `on-update`: fires on incoming state data. May modify PollerState.
  `post-start`: fires after poller stopped. Can, but should not,
                modify PollerState.
   
  `preserve-history?`: default `true`; if `false`, does not store
                       prior state data into `:history`
  "
  [polling-interval
   polling-callable
   & {:keys [pre-start
             on-update
             post-stop
             preserve-history?]
      :or {pre-start identity
           on-update identity
           post-stop identity
           preserve-history? true}}]
  {:pre [(fn? polling-callable)
         (fn? pre-start)
         (fn? on-update)
         (fn? post-stop)]}

  (comment
    ;; example
    (def my-poller
      (make-poller 1000
                   (fn [push-state current-state]
                     (println "(fakely) retrieve data..."
                              (count (:history current-state))
                              "items now")
                     (push-state {:something (rand-int 100)}))
                   :pre-start (fn [state]
                                (println "before start!")
                                (println state))
                   :on-update (fn [state]
                                (let [history-size (count (:history state))]
                                 (println "(REALLY) updating state..."
                                          history-size
                                          "items now")
                                 (when (<= 10 history-size)
                                   (println "stop condition reached")
                                   (assoc state :should-stop? true))))
                   :post-stop (fn [state]
                                (println "after finish!")
                                (println state))))
    ((:start! @my-poller)))

  (let [uuid (make-uuid)
        ch (chan (dropping-buffer 1))
        _pstate (atom (PollerState.
                       uuid             ;; uuid str
                       ch               ;; async channel
                       []               ;; seq
                       polling-interval ;; int
                       0                ;; int
                       nil              ;; fn
                       false            ;; bool
                       false            ;; bool
                       false            ;; bool
                       nil              ;; t0
                       ))
        starter (fn []
                  (when-not (:is-started? @_pstate)
                    (swap! _pstate assoc
                           :is-started? true
                           :t0 (now-ms))
                    (pre-start @_pstate)
                    (go
                      (loop []
                        (let [num-pollers (count @$poller-pool)
                              wait-interval (if (= 0 (:count @_pstate))
                                              0
                                              (:interval @_pstate))
                              jitter (if (= 0 num-pollers)
                                       0
                                       (rand-int (/ wait-interval
                                                    num-pollers)))]
                          ;; this is the effective time delay
                          (<! (timeout (+ jitter wait-interval))))
                        (polling-callable
                         (fn push-state [data]
                           (swap! _pstate
                                  (fn [cur-state]
                                    (let [tN (now-ms)
                                          next-history (if preserve-history?
                                                         (conj (:history cur-state)
                                                               (PollerEvent. tN data))
                                                         nil)
                                          next-state (-> cur-state
                                                         (update :count inc)
                                                         (assoc :history next-history))]
                                      ;; continue to the next.
                                      ;; Note that we expect the on-update function to return a full state
                                      ;; representation.  the `or` here is really a bad stopgap, which will
                                      ;; happen if `on-update` happens to be a side-effect-only function,
                                      ;; e.g. just some dom update, and the user does not remember to
                                      ;; pass-through the state value
                                      (or (on-update next-state)
                                          next-state))))
                           (go (>! ch data)))
                         @_pstate)
                        (if-not (:should-stop? @_pstate)
                          (recur)
                          (do
                            (close! ch)
                            (swap! _pstate assoc :is-stopped? true)
                            (swap! $poller-pool dissoc (:id @_pstate))
                            (post-stop @_pstate)))))))]
    (swap! $poller-pool assoc uuid _pstate)
    (swap! _pstate assoc :start! starter)
    _pstate))
