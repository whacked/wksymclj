(ns wksymclj.state-control.task)

(defprotocol TaskProtocol
  "base task protocol"
  (pre [self world] "actions preceding run")
  (proc [self world] "body of task")
  (post [self world] "actions after run"))

(defn execute [task world]
  "generic executor"
  (->> world
       (pre task)
       (proc task)
       (post task)))

(comment
  (do
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

    (->> (execute (SoapboxRaceTask. "fearless fumbler")
                  {:distance 1})
         (println "RESULT:"))))
