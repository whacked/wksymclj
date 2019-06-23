(ns wksymclj.state-control.protomat
  (:require [cljs.core.async :refer [chan <! >! close!
                                     mult tap untap untap-all
                                     pub sub unsub unsub-all]]
            [wksymclj.nodejs-interface.time :refer [now-ms]]
            [wksymclj.data-manipulation.collection :as wk-coll]
            [schema.core :as scm
             :include-macros true])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]))

;; mini state management logic inspired by ztellman/automat

;; <common>
(defn set-time-trigger!
  ([state-atom nil-op]
   (when-let [itv (get-in @state-atom [:time-trigger :itv])]
     (js/clearTimeout itv))
   (swap! state-atom assoc :time-trigger {}))
  ([state-atom op time f]
   (when-let [itv (get-in @state-atom [:time-trigger :itv])]
     (js/clearTimeout itv))
   (let [[rel-time abs-time] (case op
                               :at [(- time (now-ms)) time]
                               :in [time (+ time (now-ms))])
         next-timer (->> rel-time
                         (js/setTimeout
                          (fn []
                            (set-time-trigger! state-atom nil)
                            (f))))]
     (swap! state-atom assoc :time-trigger {:at abs-time
                                            :itv next-timer}))))

(defn set-state-time-now! [state-atom & ks]
  (swap! state-atom merge
         (let [t (now-ms)]
           (zipmap ks (repeat (count ks) t)))))

(defn get-time-until-trigger [state-atom & [now]]
  (let [now (or now (now-ms))
        get-sec (fn [k]
                  (if-let [t (k @state-atom)]
                    (.toFixed (/ (- now (k @state-atom)) 1000) 1)))]
    (if-let [trigger-time (get-in @state-atom [:time-trigger :at])]
      (- trigger-time now))))
;; </common>


;; <a-state init>
;; this state is special
(def $a--base-status {:accepted? false
                      :checkpoint nil
                      :state-index 0
                      :start-index 0
                      :stream-index 0 ;; this advances with stream
                      :value nil})

(def FsmValue
  {(scm/optional-key :next) scm/Any
   (scm/optional-key :history) [scm/Any]
   scm/Any scm/Any})
;; </a-state init>


;; disallow spaces and commas in state names
(def $SIGNAL-PATTERN-STRING-SEPARATOR ", ")

(defn signal-pattern-to-string-matcher
  [signal-pattern]
  (->> signal-pattern
       (map (fn [pattern]
              (case pattern
                :-bound-begin "^"
                :-bound-end "$"
                (str "(" (name pattern) ")"))))
       (interpose $SIGNAL-PATTERN-STRING-SEPARATOR)
       (apply str)
       (re-pattern)))

(defn signal-pattern-to-string
  [signal-pattern]
  (str
   $SIGNAL-PATTERN-STRING-SEPARATOR
   (->> signal-pattern
        (map (fn [pattern]
               (str (name pattern))))
        (interpose $SIGNAL-PATTERN-STRING-SEPARATOR)
        (apply str))
   $SIGNAL-PATTERN-STRING-SEPARATOR))

(defn match-matcher-against-input-sequence
  [matcher input-pattern]
  (some->> input-pattern
           (re-find matcher)
           (drop 1)
           (map keyword)))

(defn match-pattern-against-input-sequence
  ([pattern-seq input-seq]
   (match-pattern-against-input-sequence pattern-seq input-seq identity))
  ([pattern-seq input-seq signaler]
   (let [matcher (signal-pattern-to-string-matcher pattern-seq)
         input-pattern (map signaler input-seq)]
     (some->> input-pattern
              (signal-pattern-to-string)
              (match-matcher-against-input-sequence
               matcher)))))

(comment
  ;; TEST: regex transformer
  (let [pattern-sequence-free ["my-tag" "foo-tag" :bar-bar]
        pattern-sequence-bound-begin (concat [:-bound-begin] pattern-sequence-free)
        pattern-sequence-bound-end (concat pattern-sequence-free [:-bound-end])
        pattern-sequence-bound-both (concat [:-bound-begin] pattern-sequence-free [:-bound-end])
        
        input-sequence-with-decoy [{:tag :left-FAKE}
                                   {:tag "my-tag"}
                                   {:tag "foo-tag"}
                                   {:tag :bar-bar}
                                   {:tag :right-FAKE}]
        signaler (fn [input] (-> input (:tag) (keyword)))
        expected-full-match [:my-tag :foo-tag :bar-bar]
        ]
    (for [[desc expected pattern-seq input-seq]
          [["no bound" expected-full-match
            pattern-sequence-free input-sequence-with-decoy]
           ["bound left should not match extra left" nil
            pattern-sequence-bound-begin input-sequence-with-decoy]
           ["bound left should match without extra left" expected-full-match
            pattern-sequence-bound-begin (drop 1 input-sequence-with-decoy)]
           ["bound right should not match extra left" nil
            pattern-sequence-bound-end input-sequence-with-decoy]
           ["bound right should match without extra right" expected-full-match
            pattern-sequence-bound-end (drop-last input-sequence-with-decoy)]
           ["bound both should not match extra ends" nil
            pattern-sequence-bound-both input-sequence-with-decoy]
           ["bound both should match without extra ends" expected-full-match
            pattern-sequence-bound-both (->> input-sequence-with-decoy
                                             (drop 1)
                                             (drop-last))]]]
      (let [received (match-pattern-against-input-sequence
                      pattern-seq input-seq signaler)
            ok? (= received expected)]
        (println
         (str
          "* [" (if ok? "PASS" "FAIL") "] " desc
          (when-not ok?
            (str "\n"
                 "-- expected: " (pr-str expected) "\n"
                 "-- received: " (pr-str received)))))))))

(defrecord StringLikePatternMatcher
    [matcher signaler reducer completer])

(defn slpm--make-default-advancer [state-seq]
  (fn [state input]
    (assoc-in state
              [:value :next]
              (some->> state-seq
                       (drop-while (fn [spec-state]
                                     (not= spec-state input)))
                       (drop 1)
                       (first)))))

(defn slpm--default-reducer [state input]
  (-> state
      (update-in [:history]
                 (fn [hist]
                   (-> hist
                       (vec)
                       (conj input))))))

(defn slpm--build [pattern signaler reducer completer]
  (StringLikePatternMatcher.
   (signal-pattern-to-string-matcher pattern)
   signaler
   reducer
   completer))

(defn slpm--match [slp-matcher state stream]
  (let [{:keys [matcher signaler reducer completer]}
        slp-matcher]
    (if-let [matched-state (some->> stream
                                    (map signaler)
                                    (signal-pattern-to-string)
                                    (match-matcher-against-input-sequence
                                     matcher))]
      (-> (assoc $a--base-status
                 :accepted? true
                 :pattern matched-state
                 :value (reduce reducer state matched-state))
          (completer))
      $a--base-status)))

(comment
  ;; test slp-matcher
  (let [slpm (slpm--build ["my-tag" "foo-tag" :bar-bar]
                          (fn [input] (-> input (:tag) (keyword)))
                          slpm--default-reducer
                          (fn [end-state]
                            (js/console.log "%cALL DONE"
                                            "color:yellow;background:blue;font-weight:bold;")
                            (println end-state)))
        input-sequence-with-decoy [{:tag :left-FAKE}
                                   {:tag "my-tag"}
                                   {:tag "foo-tag"}
                                   {:tag :bar-bar}
                                   {:tag :right-FAKE}]]
    (slpm--match slpm
                 {:hello "world"}
                 input-sequence-with-decoy)))

(defn slpm--match-with-precedence
  [ordered-pattern-seq state 
   stream ;; task seq
   ]
  (loop [remain-pattern ordered-pattern-seq
         index 0
         match-result nil]
    ;; NOTE: no rejected state expected or being captured
    (if (or (:accepted? match-result)
            (empty? remain-pattern))
      match-result
      ;; we will accept either a straight function for matching,
      ;; or an StringLikePatternMatcher
      (let [pattern-matcher (first remain-pattern)]
        (recur (rest remain-pattern)
               (inc index)
               (assoc
                (if (fn? pattern-matcher)
                  ;; straight function call;
                  ;; [current-state stream-input] -> next-state
                  (pattern-matcher
                   (-> state
                       (update-in [:value :history]
                                  (fn [cur]
                                    (concat cur stream)))
                       (assoc :stream-index (dec (count stream))
                              :accepted? true))
                   (last stream))
                  (slpm--match
                   pattern-matcher
                   (assoc state :history stream)
                   stream))
                :pattern pattern-matcher))))))

;; <event management>
(def global-task-channel (chan 1))
;; (close! task-chan)

(defn trigger!
  ([op]
   (when nil (go (>! global-task-channel {:op op}))))
  ([task-channel op & {:keys [] :or {} :as msg}]
   (go (>! task-channel (assoc msg :op op)))))

(defn start-event-loop!
  ([dispatch-msg!]
   (when nil
     (start-event-loop! global-task-channel dispatch-msg!)))
  ([task-channel dispatch-msg!]
   (go-loop []
     (when-let [msg (<! task-channel)]
       (dispatch-msg! msg)
       (recur)))))

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

(defrecord StateMatcher [value
                         reducer
                         bound-begin?
                         bound-end?
                         ])

(def a--status (atom $a--base-status))

(defn a--$ [reducer-key]
  {:-reducer reducer-key})

(defn a--> [callable]
  {:-transitioner callable})

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
                      (let [maybe-rd-key (:-reducer item)
                            last-index (dec (count out))
                            update-last (partial update out last-index)]
                        (cond maybe-rd-key
                              (update-last
                               (fn [sm]
                                 (assoc sm :-reducer maybe-rd-key)))
                              
                              (= :-bound-begin item)
                              (update-last
                               (fn [sm]
                                 (assoc sm :bound-begin? true)))
                              
                              (= :-bound-end item)
                              (update-last
                               (fn [sm]
                                 (assoc sm :bound-end? true)))
                              
                              :else
                              (conj out (StateMatcher. item nil nil nil))))))))))

(defn a--find [machine init-state stream]
  (let [world init-state
        *a--history* (atom [])]
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
            (let [idx-match-seq-end
                  (loop [pair-remain (map vector
                                          match-state-seq
                                          input-stream)
                         idx-match-seq 0]
                    (if (empty? pair-remain)
                      idx-match-seq
                      (let [[matcher input] (first pair-remain)
                            match-value (:value matcher)]
                        (if (= match-value (get-signal input))
                          (recur (rest pair-remain)
                                 (inc idx-match-seq))
                          idx-match-seq))))
                  ]

              (if (and (= idx-match-seq-end match-size)
                       ;; if right-bounded, the final symbol should be
                       ;; the same as the final signal from the matchers
                       ;; FIXME: this should be at a higher level, along
                       ;; with bound-begin checking
                       (if (-> (nth match-state-seq (dec idx-match-seq-end))
                               (:bound-end?))
                         (do
                           ;; end / right bound check
                           (= (get-signal (last input-stream))
                              (last match-value-seq)))
                         true))
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
                  (assoc @a--status
                         :state-index (dec idx-match-seq-end)
                         :stream-index (+ start-idx (dec idx-match-seq-end))
                         :accepted? true
                         :value (loop [idx 0
                                       state init-state]
                                  (if-not (< idx idx-match-seq-end)
                                    state
                                    (let [maybe-callable-reducer (:-reducer (nth match-state-seq idx))
                                          reducer (if (fn? maybe-callable-reducer)
                                                    maybe-callable-reducer
                                                    (reducer-map
                                                     (or maybe-callable-reducer
                                                         :-noop)))
                                          input (nth input-stream idx)]
                                      (recur (inc idx)
                                             (reducer state input)))))))
                (recur (+ start-idx 1 idx-match-seq-end)
                       match-length
                       user-state
                       (take-last match-size (conj (vec queue) stream-signal))
                       (rest input-stream))))
            
            )))
      
      (when-let [match-idx (wk-coll/is-subvector? match-value-seq signal-list)]
        (swap! a--status assoc
               :stream-index (+ (@a--status :stream-index)
                                match-idx)
               :accepted? true
               :value (if-let [reducer (:reducer (last match-state-seq))]
                        (let [;; FIXME or REMOVEME -- input-state is undefined
                              ;; hack here to allow compilation
                              input-state nil
                              ]
                         (reducer world
                                  ;; if use is-subvector?
                                  ;; this becomes troublesome
                                  input-state)))))
      
      )))
;; </event management>
