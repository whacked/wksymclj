(ns wksymclj.example.protomat-runner
  (:require [wksymclj.nodejs-interface.time :refer [now-ms]]
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
                  :run (fn [self w]
                         (dlog "%c RUN step" "color:green;")
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
  
  (defn $_fn-default-1-back-advancer
    [state input]
    (assoc-in state
              [:value :next]
              (some->> $task-seq
                       (drop-while (fn [spec-state]
                                     (not= spec-state input)))
                       (drop 1)
                       (first))))

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
     ordered-pattern-seq {} state-list)))

(comment
  ;; OLD
  (def task-progress (atom []))
  ;; set up a deterministic pre->post step mapping
  (def task-transition-spec
    (map #(vector %1 %2 {})
         (drop-last $task-seq)
         (rest $task-seq)))
  (defn advance-state! [completed-task-name]
    (let [cur-progress (swap! task-progress conj completed-task-name)
          fsm-status (slpm--match-with-precedence
                      ordered-pattern-seq
                      $reducers
                      {}
                      @task-progress)]
      (if (:accepted? fsm-status)
        (do (println "-- fsm --" fsm-status)
            (js/alert (str "OK! next state: " (:value fsm-status))))
        (doseq [task-key (grf/get-next-state-list
                          task-transition-spec
                          completed-task-name
                          {})]
          (dlog (str "%cTRANSITION TO NEW STATE " task-key) "color:gold;font-weight:bold;")
          (protomat/trigger! task-key)))))

  (defn advance-state-in-ms! [from-state ms]
    (js/setTimeout
     (fn []
       (advance-state! from-state)) ms))
  (def mstate (atom {:current-task nil}))
  (defn get-current-task []
    (get-in @mstate [:current-task]))

  (defn dummy-task-runner []
    (let [cur-task (get-current-task)]
      (protomat/dlog (str "running task: %c" cur-task)
                     "font-weight:bold;color:white;background:black;")
      (advance-state-in-ms! step 1000)))
  
  (let [handler-mapping {:begin dummy-task-runner
                         :introduction dummy-task-runner
                         :do-some-work dummy-task-runner
                         :take-a-break dummy-task-runner
                         :say-goodbye dummy-task-runner}]
    (def EVCH (protomat/start-event-loop!
               (fn [msg]
                 (dlog "%c TRIGGER %s"
                       (pr-str msg)
                       "background:blue;color:white;font-weight:bold;")
                 (protomat/set-time-trigger! nil)
                 (let [task-key (:op msg)]
                   ;; NOTE setting phase here!!
                   (if-let [handler (handler-mapping task-key)]
                     (handler task-key)
                     (dlog (str "%cWARNING: no handler for " task-key)
                           "color:white;background:red;font-weight:bold;"))
                   (swap! mstate assoc :current-task task-key))))))

  (defn main []
    (protomat/trigger! :begin)))


