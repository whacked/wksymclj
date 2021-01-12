(ns wksymclj.codec.jsonl
  (:require [clojure.string]
            #?(:clj [clojure.data.json :as json])))

(defn read-jsonl [jsonl-string]
  (->> jsonl-string
       (clojure.string/split-lines)
       (remove empty?)
       (map (fn [line]
              #?(:clj (json/read-str line))
              #?(:cljs
                 (js->clj (js/JSON.parse line)
                          :keywordize-keys true))))
       (vec)))
