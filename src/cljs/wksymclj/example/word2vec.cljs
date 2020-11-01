(ns wksymclj.example.word2vec
  (:require [cljs.nodejs :as nodejs]
            ["word2vec" :as w2v]
            [wksymclj.nodejs-interface.fileio :as fio]))

;; uses
;; https://github.com/LeeXun/word2vector
;; (def w2v (nodejs/require "word2vector"))

;; tried others
;; https://github.com/fourseasonslab/word2vec (sparse documentation)
;; https://github.com/Planeshifter/node-word2vec (slower)

;; npm install word2vector
;; this requires node >= 8
(when (> 8
         (-> (aget js/process "versions" "node")
             (js/parseInt)))
  (throw "node version too low"))

(defn load-model [model-filepath]
(js/console.warn "LOAD MODEL we load" model-filepath)
  (js/console.log w2v)
  (.load w2v model-filepath))

(defn get-similar-words
  ([model words-string]
   (get-similar-words model words-string 10))
  ([model words-string limit]
   (-> ^js/Array (.getSimilarWords w2v words-string (clj->js {:N limit}))
       (js->clj :keywordize-keys true))))

(comment
  (def model (load-model
              (fio/path-join
               "./"
               "word2vec.6B.50d.bin")))

  (get-similar-words model "bird animal"))
