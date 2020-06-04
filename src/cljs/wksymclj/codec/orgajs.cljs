;; npm install
;;   unified orga orga-unified unified orga-rehype rehype-stringify
(ns wksymclj.codec.orgajs
  (:require
     ["unified" :as unified]
     ["orga-unified" :as orga-parse]
     ["orga-rehype" :as orga-mutate]
     ["rehype-stringify" :as rehype-html]))

(defn preprocess-org-src
  "orga seems to have trouble with single links like [[target]];
   this function preprocesses an org string so [[tgt]] becomes [[tgt][tgt]]"
  [org-src]
  (->> org-src
       (clojure.string/split-lines)
       (map (fn [line]
              (if-let [[matched-line link-target] (re-find #".*\[\[([^\]]+)\]\].*" line)]
                (str "[[" link-target "][" link-target "]]")
                line)))
       (interpose "\n")
       (apply str)))

(defn org-string-to-html [org-string]
  (let [processor (-> (unified)
                      (.use orga-parse)
                      (.use orga-mutate)
                      (.use rehype-html))]
    (-> processor
        (.processSync
         (preprocess-org-src org-string))
        (str))))
