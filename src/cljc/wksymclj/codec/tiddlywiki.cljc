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

(def $tid-header-required-keys #{:created :modified :title :type})

(defn is-valid-tid-header? [hdr]
  (let [matches (->> $tid-header-required-keys
                     (map hdr))]
    (and (not (empty? matches))
         (every? identity matches))))

(defn split-tid [tid-content]
  (clojure.string/split
   tid-content #"\r?\n\r?\n" 2))

(defn map-to-json [m]
  #?(:clj
     (json/write-str m))
  #?(:cljs
     (->> (clj->js m)
          (json/write-str))))

(defn parse-tid-header [tid-content]
  (loop [remain (-> tid-content
                    (split-tid)
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
                        #"([^:]+):\s*(.*)\s*$"))
                   k (keyword k-str)]
               (if-not k
                 out
                 (assoc out
                        k
                        (case k
                          (:created :modified) (tid-timestamp-to-date v-str)
                          (:tmap.style :tmap.edges) (json/read-str v-str) 
                          
                          v-str))))))))

(defn parse-tid-content [tid-content]
  (let [spl (split-tid tid-content)
        maybe-hdr (parse-tid-header (first spl))]
    (cond (not= 2 (count spl))
          nil

          (empty? maybe-hdr)
          nil

          :else
          {:header maybe-hdr
           :content (last spl)})))

(defn render-tid-header [tid-header]
  (->> tid-header
       (map (fn [[k v]]
              (str (name k)
                   ": "
                   (cond (some-> v (aget "date"))
                         (date-to-tid-timestamp v)

                         (map? v)
                         (map-to-json v)
                         
                         :else v))))
       (interpose "\n")
       (apply str)))

(defn render-tid [parsed-tid]
  (str (-> (:header parsed-tid)
           (render-tid-header))
       "\n\n"
       (:content parsed-tid)))
