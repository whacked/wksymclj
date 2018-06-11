(ns wksymclj.common)

(defn now-seconds []
  (-> (System/currentTimeMillis)
      (/ 1000)
      (double)))

