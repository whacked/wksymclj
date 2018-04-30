(ns wksymclj.nodejs-interface.fileio
  (:require [cljs.nodejs :as nodejs]))

(def fs (nodejs/require "fs"))
(def path (nodejs/require "path"))

(defn path-join [& argv]
  (apply (aget path "join") argv))

