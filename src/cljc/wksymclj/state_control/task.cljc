(ns wksymclj.state-control.task)

(defprotocol TaskProtocol
  "base task protocol"
  (pre [self world] "-> world'; actions preceding run")
  (proc [self world] "-> world'; body of task")
  (post [self world] "-> world'; actions after run")
  (did-succeed?
    [self world] "-> bool; check if success criteria met")
  (on-success
    [self world] "-> world'; fires immediately when success criteria met")
  (did-fail?
    [self world] "-> bool; check if failure criteria met")
  (on-failure
    [self world] "-> world'; fires immediately when failure criteria met"))

(defn execute-one-shot [task world]
  "generic executor"
  (->> world
       (pre task)
       (proc task)
       (post task)))

(defn execute-for-completion [task start-world]
  "conditional executor, evaluates acceptance / rejection critiera
   checkers in a loop. May loop indefinitely if no stopping condition
   reached"
  (loop [world (pre task start-world)]
    (cond (did-succeed? task world)
          (->> world
               (on-success task)
               (post task))

          (did-fail? task world)
          (->> world
               (on-failure task)
               (post task))

          :else
          (recur (proc task world)))))

(comment
  (do
    ;; it looks like the strategy in
    ;; https://clojuredocs.org/clojure.core/extend#example-5be9f7e9e4b00ac801ed9ef3
    ;; does not work with clojurescript; one method to achieve inheritance
    ;; is by separating namespaces: https://stackoverflow.com/a/32725477
    ;; but this example is too simple to warrant namespace separation,
    ;; so we will just make new defs for each new example type
    (deftype SoapboxRaceTask [name]
      TaskProtocol
      (pre [self world]
        (println "[" name "]" "running pre/" world)
        (-> world
            (update :distance (partial + 1))
            (assoc :stage "ignition")))
      (proc [self world]
        (println "[" name "]" "running proc/" world)
        (-> world
            (update :distance (partial + 20))
            (assoc :stage "dashing")))
      (post [self world]
        (println "[" name "]" "running post/" world)
        (-> world
            (update :distance (partial + 5))
            (assoc :stage "fall in the water"))))
    
    (println "=== one shot execution ===")
    (->> (execute-one-shot
          (SoapboxRaceTask. "fearless fumbler")
          {:distance 0})
         (println "RESULT:"))
    
    (deftype SoapboxRaceTaskConditional [name]
      TaskProtocol
      (pre [self world]
        (-> world
            (update :distance inc)))
      (proc [self world]
        (update world :distance dec))
      (did-succeed? [self world] false)
      (did-fail? [self world]
        (println "checking for failure...")
        (< (:distance world) -3))
      (on-failure [self world]
        (println "OH NO!!!")
        world)
      (post [self world]
        (assoc world :stage (str name ": where did we go???"))))
    
    (println "=== conditional execution with failure ===")
    (->> (execute-for-completion
          (SoapboxRaceTaskConditional. "flaming failure")
          {:distance 0})
         (println "RESULT:"))
    
    (deftype SoapboxRaceTaskConditionalAgain [name]
      TaskProtocol
      (pre [self world]
        (update world :distance inc))
      (proc [self world]
        (update world :distance (partial + 5)))
      (did-succeed? [self world]
        (< 100 (:distance world)))
      (on-success [self world]
        (println "slow and steady wins!" name)
        world)
      (did-fail? [self world]
        (< (:distance world) 0))
      (on-failure [self world]
        (println "WTF?")
        world)
      (post [self world]
        (assoc world :stage (str name ": collect your prize!"))))
    
    (println "=== conditional execution with success ===")
    (->> (execute-for-completion
          (SoapboxRaceTaskConditionalAgain. "ferocious fighter")
          {:distance 0})
         (println "RESULT:"))))

(comment
  (do
    (deftype AsyncTestTask [name]
      TaskProtocol
      (pre [self world]
        (println "[" name "]" "running pre/" world)
        (let [next-world
              (-> world
                  (update :distance (partial + 1))
                  (assoc :stage "ignition"))]
          ;; goloop method
          ;; (go
          ;;   (<! (timeout 2000))
          ;;   (proc self next-world))
          (js/setTimeout
           (fn [] (proc self next-world))
           2000)))
      (proc [self world]
        (println "[" name "]" "running proc/" world)
        (let [next-world (-> world
                             (update :distance (partial + 20))
                             (assoc :stage "dashing"))]
          ;; goloop method
          ;; (go
          ;;   (<! (timeout 1500))
          ;;   (post self next-world))
          (js/setTimeout
           (fn [] (post self next-world))
           1500)))
      (post [self world]
        (println "[" name "]" "running post/" world)
        (let [next-world (-> world
                             (update :distance (partial + 5))
                             (assoc :stage "fall in the water"))]
          (js/setTimeout
           (fn [] (js/console.warn "END"))
           1000))))
    (let []
      (pre (AsyncTestTask. "ASYNC-TESTER")
           {:state "init"}))))
