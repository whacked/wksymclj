(ns wksymclj.nodejs-interface.fileio
  (:require [cljs.nodejs :as nodejs]
            [clojure.string])
  (:require-macros
   [swiss.arrows :refer [-<> -<>>]]))

(def fs (nodejs/require "fs"))
(def path (nodejs/require "path"))

(def $USER-HOME js/process.env.HOME)

;; path
(defn path-join [& argv]
  (apply (aget path "join") argv))

(defn path-basename [fpath]
  (let [func (aget path "posix" "basename")]
    (func fpath)))

(defn path-exists? [fpath]
  (.existsSync fs fpath))

(defn node-expand-user [path]
  (clojure.string/replace path #"^~" $USER-HOME))

;; io
(defn simple-slurp [fpath]
  (.readFileSync fs fpath "utf-8"))

(defn simple-spit [fpath content]
  (.writeFileSync fs fpath content))

(defn slurp-if-exists [fpath]
  (when (path-exists? fpath)
    (simple-slurp fpath)))

(def FILE-CANDIDATE-SEARCH-HISTORY (atom {}))
(defn get-first-existing-candidate-from-dirs [file-name try-dirs]
  (swap! FILE-CANDIDATE-SEARCH-HISTORY
         assoc file-name try-dirs)
  (loop [remain (distinct try-dirs)
         result nil]
    (if (or result (empty? remain))
      result
      (recur (rest remain)
             (let [candidate-path (path-join (first remain)
                                             file-name)]
               (if (path-exists? candidate-path)
                 candidate-path
                 result))))))

(defn get-file-stat [filepath]
  (if (path-exists? filepath)
    (-<>> (.statSync fs filepath)
          (.assign js/Object #js {})
          (js->clj <> :keywordize-keys true)
          (map (fn [[k v]]
                 [k (if (= (type v)
                           js/Date)
                      (.getTime v)
                      v)]))
          (into {}))))

(defn get-relative-path [base-path full-path]
  (.relative path base-path full-path))

(defn get-extension [filepath]
  (.extname path filepath))
