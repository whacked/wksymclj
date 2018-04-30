(ns wksymclj.example.protomat-runner
  (:require [wksymclj.state-control.protomat
             :refer [dlog]
             :as protomat]))

(comment
  ;; example run
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
