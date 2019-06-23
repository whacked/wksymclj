(ns wksymclj.codec.response-data
  (:require [clojure.string]
            [wksymclj.data-manipulation.simple-json :as json]))

(defrecord Toer1 [time onset expected received])

(defn rec->toer-v1 [clj-rec]
  (->> (map clj-rec
            [:t :o :e :r])
       (apply Toer1.)))

(defn toer-v1->rec [toer-rec]
  (->> (map toer-rec
            [:time :onset :expected :received])
       (zipmap [:t :o :e :r])))

(defrecord Sert1 [stimulus expected response time])

(defn rec->sert-v1 [clj-rec]
  (->> (map clj-rec
            [:s :e :r :t])
       (apply Sert1.)))

(defn sert-v1->rec [sert-rec]
  (->> (map sert-rec
            [:stimulus :expected :response :time])
       (zipmap [:s :e :r :t])))
