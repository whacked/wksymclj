(ns wksymclj.nodejs-interface.time)

(defn now-ms []
  (.now js/Date))
