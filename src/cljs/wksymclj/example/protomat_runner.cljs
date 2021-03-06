(ns wksymclj.example.protomat-runner
  (:require [cljs.core.async :refer [chan <! >! close!
                                     mult tap untap untap-all
                                     pub sub unsub unsub-all]]
            [wksymclj.nodejs-interface.time :refer [now-ms]]
            [wksymclj.state-control.protomat
             :refer [dlog]
             :as protomat]
            [wksymclj.data-manipulation.graph :as grf]))

(comment
  ;; toy run
  (let [my-step (protomat/make-stepper
                 {:pre (fn [self w]
                         (dlog "%c PRE step" "color:orange;")
                         (dlog (clj->js w))
                         (assoc w :did-pre true))
                  :proc (fn [self w]
                          (dlog "%c PROC step" "color:green;")
                          (js/setTimeout
                           (fn []
                             (dlog "hello from main run")
                             (dlog (clj->js w)))
                           1000)
                          (js/console.log "ran main")
                          (assoc w :did-run true))
                  :post (fn [self w]
                          (dlog "%c POST step" "color:blue;")
                          (assoc w :did-post true))})
        runall (fn run-step [stepper world]
                 (dlog "%crun stepper, sequence with world:" "color:green;")
                 (dlog (clj->js world))
                 ((:runall stepper) world))
        ]
    (runall my-step {})))

(comment
  ;; slpm sample run through

  (def $task-seq [:begin
                  :introduction
                  :do-some-work
                  :take-a-break
                  :say-goodbye])

  ;; set up a pattern to match against that determines
  ;; whether we are all-done
  (def completion-pattern
    (protomat/slpm--build
     (concat
      [:begin :introduction]
      (apply concat
             (repeat 3 [:do-some-work
                        :take-a-break]))
      [:say-goodbye])
     identity
     protomat/slpm--default-reducer
     (fn complete [end-state]
       (dlog "%cALL DONE" "color:white;background:blue;")
       (assoc end-state :value :finish))))

  (def work-set-pattern
    (protomat/slpm--build
     (conj
      (->> [:do-some-work
            :take-a-break]
           (repeat 3)
           (apply concat)
           (vec))
      :-bound-end)
     identity
     protomat/slpm--default-reducer
     (fn go-say-goodbye [end-state]
       (-> end-state
           (assoc-in
            [:value :next] :say-goodbye)))))

  (def back-to-work-pattern
    (protomat/slpm--build
     [:take-a-break :-bound-end]
     identity
     protomat/slpm--default-reducer
     (fn go-do-some-work [end-state]
       (-> end-state
           (assoc-in
            [:value :next] :do-some-work)))))
  
  (def $_fn-default-1-back-advancer
    (protomat/slpm--make-default-advancer $task-seq))

  (def ordered-pattern-seq
    [completion-pattern
     work-set-pattern
     back-to-work-pattern
     $_fn-default-1-back-advancer])

  (let [state-list (->> [:begin
                         :introduction
                         :do-some-work
                         :take-a-break
                         :do-some-work
                         :take-a-break
                         :do-some-work
                         :take-a-break
                         :say-goodbye
                         ]
                        (take 9))]
    (println "START STATE LIST" state-list)
    (protomat/slpm--match-with-precedence
     ordered-pattern-seq {} state-list))

  (do
    ;; OLD
    (def task-channel (chan 1))
    (def task-progress (atom []))
    ;; set up a deterministic pre->post step mapping
    (def task-transition-spec
      (map #(vector %1 %2 {})
           (drop-last $task-seq)
           (rest $task-seq)))
    (defn advance-state! [completed-task-name]
      (let [cur-progress (swap! task-progress conj completed-task-name)
            fsm-status (protomat/slpm--match-with-precedence
                        ordered-pattern-seq
                        {}
                        @task-progress)]
        (if (:accepted? fsm-status)
          (do (println "-- fsm --" fsm-status)
              (if-let [next-state (get-in fsm-status [:value :next])]
                (do
                  (dlog (str "%cTRANSITION TO NEW STATE " next-state)
                        "color:salmon;font-weight:bold;")
                  (protomat/trigger! task-channel next-state))))
          (doseq [task-key (grf/get-next-state-list
                            task-transition-spec
                            completed-task-name
                            {})]
            ;; XXX 2019-06-23 12:53:29 not clear why this should be the else case
            (dlog (str "%cTRANSITION TO NEW STATE " task-key)
                  "color:gold;font-weight:bold;")
            (protomat/trigger! task-channel task-key)))))
    
    (defn advance-state-in-ms! [from-state ms]
      (js/setTimeout
       (fn []
         (advance-state! from-state)) ms))
    (def mstate (atom {:current-task nil}))

    (defn dummy-task-runner [task-state-atom cur-task]
      (protomat/dlog (str "running task... %c" cur-task)
                     "font-weight:bold;color:white;background:black;")
      (swap! task-state-atom assoc :current-task cur-task)
      (advance-state-in-ms! cur-task 1000))
    
    (let [handler-mapping {:begin dummy-task-runner
                           :introduction dummy-task-runner
                           :do-some-work dummy-task-runner
                           :take-a-break dummy-task-runner
                           :say-goodbye dummy-task-runner}]
      (def EVCH (protomat/start-event-loop!
                 task-channel
                 (fn [{task-key :op
                       :as msg}]
                   (dlog (str "%cTRIGGER " (pr-str msg))
                         "background:blue;color:white;font-weight:bold;")
                   (protomat/set-time-trigger! mstate nil)
                   ;; NOTE setting phase here!!
                   (if-let [handler (handler-mapping task-key)]
                     (handler mstate task-key)
                     (dlog (str "%cWARNING: no handler for " task-key)
                           "color:white;background:red;font-weight:bold;"))))))
    
    ;; initialize the event sequence
    (protomat/trigger! task-channel :begin)))
