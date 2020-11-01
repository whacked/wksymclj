;; npm install
;;   unified orga orga-unified unified orga-rehype rehype-stringify
(ns wksymclj.codec.orgajs
  (:require
     ["unified" :as unified]
     ["orga-unified" :as orga-parse]
     ["orga-rehype" :as orga-ast-to-rehype-ast]
     
     ;; rehype-slug substitute
     ["github-slugger" :as github-slugger]
     ["unist-util-visit" :as visit]
     ["hast-util-to-string" :as toString]
     ["hast-util-is-element" :as is]
     ["hast-util-has-property" :as has]

     ["rehype-stringify" :as rehype-html]))

(def $ORG-HEADING-ID-PREFIX
  "org-header-")

;; slug logic ref
;; https://github.com/rehypejs/rehype-slug/blob/main/index.js except
;; we want to qualify the slug, instead of creating an id
;; directly from the text
(def $headings #js ["h1" "h2" "h3" "h4" "h5" "h6"])

(def $slugs (github-slugger))
(defn add-slugs-to-headings [& [options]]
  (let [heading-prefix (get options :prefix "")
        heading-postfix (get options :postfix "")]
    (fn transform [tree]
      (.reset $slugs)
      (visit tree
             "element"
             (fn [node]
               (when (and (is node $headings)
                          (not (has node "id")))
                 (aset node "properties" "id"
                       (.slug $slugs
                              (str heading-prefix
                                   (toString node)
                                   heading-postfix)))))))))

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
                      (.use orga-ast-to-rehype-ast)
                      (.use add-slugs-to-headings
                            {:prefix $ORG-HEADING-ID-PREFIX})
                      (.use rehype-html))]
    (-> processor
        (.processSync
         (preprocess-org-src org-string))
        (str))))
