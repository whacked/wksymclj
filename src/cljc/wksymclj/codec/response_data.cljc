(ns wksymclj.codec.response-data
  (:require [cuerdas.core :as cstr]
            [clojure.string]
            [wksymclj.data-manipulation.simple-json :as json]
            #?(:clj [clj-time.core :as time])
            #?(:clj [clj-time.format :as tfmt])
            #?(:cljs [cljs-time.core :as time])
            #?(:cljs [cljs-time.format :as tfmt])))

(defrecord Toer1 [time onset expected received])

(defn rec->toer-v1 [clj-rec]
  (->> (map clj-rec
            [:t :o :e :r])
       (apply Toer1.)))

(defn toer-v1->rec [toer-rec]
  (->> (map toer-rec
            [:time :onset :expected :received])
       (zipmap [:t :o :e :r])))
