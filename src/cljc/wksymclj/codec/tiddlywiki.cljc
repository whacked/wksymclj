(ns wksymclj.codec.tiddlywiki
  (:require [cuerdas.core :as cstr]
            [clojure.string]
            [wksymclj.data-manipulation.simple-json :as json]
            #?(:clj [clj-time.core :as time])
            #?(:clj [clj-time.format :as tfmt])
            #?(:cljs [cljs-time.core :as time])
            #?(:cljs [cljs-time.format :as tfmt])))

(def $TIDDLYWIKI-TIMESTAMP-FORMAT "yyyyMMddHHmmssSSS")

(defn tid-timestamp-to-date [ts]
  (tfmt/parse
   (tfmt/formatter-local
    $TIDDLYWIKI-TIMESTAMP-FORMAT)
   ts))

(defn date-to-tid-timestamp [clj-datetime]
  (tfmt/unparse-local
   (tfmt/formatter $TIDDLYWIKI-TIMESTAMP-FORMAT)
   clj-datetime))

(defn parse-tid-header [tid-content]
  (loop [remain (-> tid-content
                    (clojure.string/split #"\r?\n\r?\n")
                    (first)
                    (clojure.string/split-lines))
         out {}]
    (if (empty? remain)
      out
      (recur (rest remain)
             (let [[match k-str v-str]
                   (-> remain
                       (first)
                       (clojure.string/split
                        #"([^:]+): (.+)\s*$"))
                   k (keyword k-str)]
               (if-not k
                 out
                 (assoc out
                        k
                        (case k
                          (:created :modified) (tid-timestamp-to-date v-str)
                          (:tmap.style :tmap.edges) (json/read-str v-str) 
                          
                          v-str))))))))
