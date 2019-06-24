(ns wksymclj.codec.response-data
  (:require [clojure.string]
            [wksymclj.data-manipulation.simple-json :as json]
            [schema.core :as scm
             #?@(:cljs [:include-macros true])]))

(def Toer1Schema
  {:time scm/Num
   :onset scm/Num
   :expected scm/Any
   :received scm/Any})
(defrecord Toer1 [time onset expected received])

(defn rec->toer-v1 [clj-rec]
  (->> (map clj-rec
            [:t :o :e :r])
       (apply ->Toer1)))

(defn toer-v1->rec [toer-rec]
  (->> (map (fn [k] (k toer-rec))
            [:time :onset :expected :received])
       (zipmap [:t :o :e :r])))

(def Sert1Schema
  {:stimulus scm/Num
   :expected scm/Any
   :received scm/Any
   :time scm/Num})
(defrecord Sert1 [stimulus expected received time])

(defn rec->sert-v1 [clj-rec]
  (->> (map clj-rec
            [:s :e :r :t])
       (apply ->Sert1)))

(defn sert-v1->rec [sert-rec]
  (->> (map (fn [k] (k sert-rec))
            [:stimulus :expected :received :time])
       (zipmap [:s :e :r :t])))

(def Sertrt1Schema
  {:stimulus scm/Num
   :expected scm/Any
   :received scm/Any
   :time scm/Num
   (scm/optional-key :rt) scm/Num})
(defrecord Sertrt1 [stimulus expected received time rt])

(defn rec->sertrt-v1 [clj-rec]
  (->> (map clj-rec
            [:s :e :r :t :rt])
       (apply ->Sertrt1)))

(defn sertrt-v1->rec [sertrt-rec]
  (->> (map (fn [k] (k sertrt-rec))
            [:stimulus :expected :received :time :rt])
       (zipmap [:s :e :r :t :rt])))
