(ns wksymclj.data-manipulation.simple-json
  #?(:clj
     (:require [clojure.data.json :as json])))

(def read-str
  #?(:clj json/read-str)
  #?(:cljs (fn [s]
             (-> (.parse js/JSON s)
                 (js->clj)))))

(def write-str
  #?(:clj json/write-str)
  #?(:cljs (fn [clj-data]
             (.stringify js/JSON (clj->js clj-data)))))
