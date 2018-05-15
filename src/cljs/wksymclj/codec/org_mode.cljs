(ns wksymclj.codec.org-mode
  (:require [cljs.nodejs :as nodejs]
            ))

(def Org (nodejs/require "org"))
(def OrgParser (aget Org "Parser"))
(def OrgConverter (aget (nodejs/require "org/lib/org/converter/converter.js")
                        "Converter"))
(def OrgNode (aget (nodejs/require "org/lib/org/node.js")
                   "Node"))

(def $org-js-html-class-prefix "org-")
(def $org-js-html-id-prefix "org-")
(def $org2hiccup-default-options
  {:htmlClassPrefix $org-js-html-class-prefix})

(defn org2html [org-src]
  (let [parser (new OrgParser)
        org-document (.parse parser org-src)
        org-htmldoc (.convert org-document
                              (aget Org "ConverterHTML")
                              (clj->js
                               {:htmlClassPrefix $org-js-html-class-prefix
                                :htmlIdPrefix $org-js-html-id-prefix
                                :headerOffset 0
                                :exportFromLineNumber false
                                :suppressSubscriptHandling false
                                :suppressAutoLink false}))]
    ;; INSPECT:
    ;; (js/console.dir org-htmldoc)
    ;; > { title, contentHTML, tocHTML, toc }
    (.toString org-htmldoc) ;; Rendered HTML
    ))

(def compute-aux-data-for-node
  (fn [input-node]
    (comment
      ;; FIXME
      (let [org-inline-container-type (aget OrgNode "types" "inlineContainer")
            node (loop [out input-node]
                   (if (and (aget node "parent")
                            (= (aget node "parent" "type")
                               org-inline-container-type))
                     (aget node "parent")
                     out))]
        (loop [attributesText ""
               attributesNode (aget node "previousSibling")]
          (if (and attributesNode
                   (= attributesNode.type OrgNode.types.directive)
                   (= attributesNode.directiveName "attr_html"))
            (recur
             (str attributesText
                  (aget attributesNode "directiveRawValue")
                  " ")
             (aget attributesNode "previousSibling"))))))
    {}
    
    ))

(defn recur-get-text [node]
  (or (aget node "value")
      (aget node "name")
      (let [js-subnodes (aget node "children")
            n-subnodes (aget js-subnodes "length")]
        (loop [i 0
               out []]
          (if-not (< i n-subnodes)
            (apply str out)
            (let [subnode (aget js-subnodes i)]
              (recur (inc i)
                     (if-let [maybe-value (recur-get-text subnode)]
                       (conj out maybe-value)
                       out))))))))

(defn de-nestify [maybe-nested-seq]
  ;; the final value will be enclosed in a
  ;; single vector element, i.e.
  ;; [[:span "foo"] [:div "bar"]]
  ;; so you will want to concat it somewhere!
  (comment
    (de-nestify
     [:div
      [:span "HI"]
      [:div "foo bar"]
      [[[:b "some bold text"]]]
      "FOOBAR"
      "BAX"
      [:span "HI HI"]]))
  (cond (not (sequential? maybe-nested-seq))
        [maybe-nested-seq]

        (empty? maybe-nested-seq)
        nil

        (keyword? (first maybe-nested-seq))
        (let [leading (first maybe-nested-seq)
              maybe-property (second maybe-nested-seq)
              has-property? (map? maybe-property)]
          (->> maybe-nested-seq
               (drop (if has-property? 2 1))
               (de-nestify)
               (concat [leading
                        (if has-property?
                          maybe-property
                          {})])
               (vec)
               (vector)))
        
        :else
        (->> maybe-nested-seq
             (map de-nestify)
             (apply concat)
             (vec))))

(def converter-hiccup
  ;; html.js:54
  ;; ConverterHTML.prototype = {
  ;;   __proto__: Converter.prototype,

  ;;   convert: function () {
  ;;     var title = this.orgDocument.title ? this.convertNode(this.orgDocument.title) : this.untitled;
  ;;     var titleHTML = this.tag("h" + Math.max(Number(this.headerOffset), 1), title);
  ;;     var contentHTML = this.convertNodes(this.orgDocument.nodes, true /* record headers */);
  ;;     var toc = this.computeToc(this.documentOptions["toc"]);
  ;;     var tocHTML = this.tocToHTML(toc);

  ;;     return {
  ;;       title: title,
  ;;       titleHTML: titleHTML,
  ;;       contentHTML: contentHTML,
  ;;       tocHTML: tocHTML,
  ;;       toc: toc,
  ;;       toString: function () {
  ;;         return titleHTML + tocHTML + "\n" + contentHTML;
  ;;       }
  ;;     };
  ;;   },
  ;;   ...
  ;; }
  (fn [org-document export-options]
    (this-as self
      (js/console.log "converting")
      (js/console.log self)
      (js/console.warn (aget self "prototype"))
      (let [record-headers true]
        (js/console.log "doto..." self)
        (doto self
          (.initialize
           org-document
           (-> (merge $org2hiccup-default-options
                      (js->clj export-options :keywordize-keys true))
               (clj->js)))
          (aset "result"
                (-> (.convert self record-headers)
                    (de-nestify))))))))

(defn org2hiccup [org-src]
  (-> (new OrgParser)
      (.parse org-src)
      (.convert converter-hiccup)))

;; FIXME
(defn get-org-class-name [org-obj element-type]
  "")

(defn get-export-option [org-obj option-name]
  nil)

(def
  ;; converter.js:350
  ;; http://daringfireball.net/2010/07/improved_regex_for_matching_urls
  converter-urlPattern (re-pattern "(?i)\\b(?:https?://|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\\\".,<>?«»“”‘’])"))

(declare converterMapping)

(do
  
  (defn getConverter [node]
    (this-as self
      (let [node-type (aget node "type")]
        (js/console.info (str "TYPE: %c" node-type)
                         "color:white;background:blue;font-weight:bold;")
        (js/console.log "matched converter" (converterMapping node-type))
        (or (converterMapping node-type)
            (converterMapping "default")))))

  ;; converter.js:89
  (defn convertNode [node recordHeader insideCodeElement]
    (this-as self
      ;; (when (not insideCodeElement)
      ;;   (if (= node.type OrgNode.types.directive)
      ;;     (when (or (= node.directiveName "example")
      ;;               (= node.directiveName "src"))
      ;;       (assign insideCodeElement true))
      ;;     (when (= node.type OrgNode.types.preformatted)
      ;;       (assign insideCodeElement true))))

      ;; (when (= (typeof node) "string")
      ;;   ;; this returns
      ;;   ;; PrototypeNode {type: "text" children: [] value: nil}
      ;;   (assign node (OrgNode.createText nil {:value node})))

      (let [ ;;child-text (self.getChildText node recordHeader insideCodeElement)
            aux-data (compute-aux-data-for-node node)]
        (js/console.info "convertNode: self is" self)
        (-> (getConverter node)
            (.call self node recordHeader insideCodeElement)))
      
      ;; (comment
      ;;  (switch node.type
      ;;          (OrgNode.types.header
      
      ;;           ;; Compute section number
      ;;           (var sectionNumberText nil)
      ;;           (when recordHeader
      ;;                 (var thisHeaderLevel node.level
      ;;                      previousHeaderLevel this.sectionNumbers.length)
      ;;                 (if (> thisHeaderLevel previousHeaderLevel)
      ;;                     ;; Fill missing section number
      ;;                     (each (j) (range (- thisHeaderLevel previousHeaderLevel))
      ;;                           ;; Extend
      ;;                           (set this.sectionNumbers.length
      ;;                                (- thisHeaderLevel 1 j)
      ;;                                0))
      ;;                     (< thisHeaderLevel previousHeaderLevel)
      ;;                     ;; Collapse
      ;;                     (assign this.sectionNumbers.length thisHeaderLevel))
      ;;                 (incr (get this.sectionNumbers
      ;;                            (- thisHeaderLevel 1)))
      ;;                 (assign sectionNumberText (this.sectionNumbers.join "."))
      ;;                 ;; Can be used in ToC
      ;;                 (assign node.sectionNumberText sectionNumberText))

      ;;           (assign text (this.convertHeader node childText auxData
      ;;                                            taskStatus sectionNumberText))
      ;;           (when recordHeader
      ;;                 (this.headers.push node)))

      ;;          (OrgNode.types.listElement
      ;;           (if node.isDefinitionList
      ;;               (assign text (this.convertDefinitionItem
      ;;                             node childText auxData
      ;;                             (this.convertNodesInternal node.term recordHeader insideCodeElement)
      ;;                             childText))
      ;;               (assign text (this.convertListItem node childText auxData))))
      
      ;;          (OrgNode.types.paragraph
      ;;           (assign text (this.convertParagraph node childText auxData)))

      ;;          (OrgNode.types.preformatted
      ;;           (assign text (this.convertPreformatted node childText auxData)))


      
      ;;          (OrgNode.types.tableCell
      ;;           (assign text
      ;;                   (if node.isHeader
      ;;                       (this.convertTableHeader node childText auxData)
      ;;                       (this.convertTableCell node childText auxData))))

      ;;          ;; ============================================================
      ;;          ;; Inline
      ;;          ;; ============================================================ 
      ;;          (OrgNode.types.inlineContainer
      ;;           (assign text (this.convertInlineContainer node childText auxData)))

      ;;          (OrgNode.types.link
      ;;           (assign text (this.convertLink node childText auxData)))

      ;;          (OrgNode.types.text
      ;;           (assign text (this.convertText node.value insideCodeElement)))

      ;;          (default (throw (Error (+ "unknown node type: " node.type))))))
      
      ))

  (defn convertNodesInternal [nodes recordHeader insideCodeElement]
    (js/console.log "%cCONVERTING INTERNAL"
                    "color:white;background:lime;")
    (this-as self
      (let [n-nodes (aget nodes "length")
            ]
        (js/console.log
         "convertnode"
         n-nodes
         convertNode
         )
        (when (< 0 n-nodes)
          (loop [i 0
                 out []]
            (if-not (< i n-nodes)
              out
              (recur (inc i)
                     (conj
                      out
                      (-> (aget nodes i)
                          (convertNode recordHeader insideCodeElement))))))))))

  (defn getChildText [node recordHeader insideCodeElement]
    (this-as self
      (let []
        (js/console.warn "get :getChildText"
                         )
        (when (aget node "children")
          (convertNodesInternal
           (aget node "children")
           recordHeader
           insideCodeElement))                  
        )
      ))

  ;; html.js:314
  (def replaceMap
    {;; [replacing pattern predicate]
     "&"  ["&#38;" nil]
     "<"  ["&#60;" nil]
     ">"  ["&#62;" nil]
     "\"" ["&#34;" nil]
     "'"  ["&#39;" nil]})
  
  (defn escapeSpecialChars [text insideCodeElement]
    (comment
     (this-as self
       (let [replace-keys (keys replaceMap)]
         (when (not self.replaceRegexp)
           (assign self.replaceRegexp
                   (regex (|> (keys self.replaceMap)
                              (.join "|"))
                          "g")))
         (var replace-map self.replaceMap)
         (|> text
             (.replace self.replaceRegexp
                       (fn [matched]
                         (when (not (get replace-map matched))
                           (throw (Error "escapeSpecialChars: Invalid match")))
                         (var predicate (get replace-map matched 1))
                         (if (and (= (typeof predicate)
                                     "function")
                                  (not (predicate.call self text insideCodeElement)))
                           matched
                           (get replace-map matched 0))))))))
    text)

  (defn convertText [text insideCodeElement]
    (this-as self
      (let [
            exportOptions (if-let [eo (aget self "exportOptions")]
                            (js->clj eo :keywordize-keys true)
                            (do (println "WARNING: NO EXPORT OPTIONS")
                                {}))

            default-text (fn [text & _] text)
            
            
            makeSubscripts (if-let [f (aget self "makeSubscripts")]
                             f (do (println "WARNING: no makesubscripts")
                                   default-text))
            linkURL (if-let [f (aget self "linkURL")]
                      f (do (println "WARNING: no linkURL")
                            default-text))
            makeDate (if-let [f (aget self "makeDate")]
                       f (do (println "WARNING: no makeDate")
                             default-text))]
        (cond-> (escapeSpecialChars text insideCodeElement)
          (and (not (exportOptions :suppressSubScriptHandling))
               (not insideCodeElement))
          (makeSubscripts insideCodeElement)

          (not (exportOptions :suppressAutoLink))
          (linkURL)

          ;; AD-HOC AND NEVER USED OPTION!!! `suppressAutoDate`
          ;; created right here right now.
          ;; don't know how this interacts with the date parser
          (not (exportOptions :suppressAutoDate))
          (makeDate)))))

  (def converterMapping
    {"default" (fn [node]
                 [:span
                  (recur-get-text node)])

     "dashed" (fn [node]
                (this-as self
                  [:del
                   {:class (get-org-class-name "dashed")}
                   (.call getChildText self node)]))

     "horizontalRule" (fn [node] [:hr])

     "bold" (fn [node]
              (this-as this
                [:b
                 {:class
                  ;; (aget this "orgClassName" "bold")
                  (get-org-class-name this "bold")}
                 (.call getChildText this node)]))

     "italic" (fn [node]
                (this-as self
                  [:i
                   {:class (get-org-class-name "italic")}
                   (.call getChildText self node)]))

     "underline" (fn [node]
                   (this-as self
                     [:span
                      {:class (get-org-class-name "underline")}
                      (.call getChildText self node)]))
     
     ;;  :directive (fn [node]
     ;;               (var child-text (this.getChildText node)
     ;;                    aux-data (compute-aux-data-for-node node))

     ;;               (switch node.directiveName
     ;;                       ("quote"
     ;;                        [:blockquote
     ;;                         (merge aux-data
     ;;                                {:style {:font-family "Verdana"}}
     ;;                                )
     ;;                         child-text])
     ;;                       ("example"
     ;;                        [:pre aux-data
     ;;                         ;; SPECIAL CASE
     ;;                         (apply-to-textual-element-in-miccup
     ;;                          child-text
     ;;                          string-with-transclude-to-miccup)
     ;;                         ])
     ;;                       ("src"
     ;;                        (var codeLanguage (or (get node.directiveArguments 0)
     ;;                                              "unknown"))
     ;;                        [:pre
     ;;                         [:code
     ;;                          (merge aux-data
     ;;                                 {:class (+ "language-" codeLanguage)})
     ;;                          child-text]])
     ;;                       ([:html "html:"]
     ;;                        (throw (new Error "fixme"))
     ;;                        (assign text (this.convertHTML node childText auxData)))
     ;;                       (default
     ;;                        (console.warn "FALLBACK IN DIRECTIVENAME SWITCH ON"
     ;;                                      node.directiveName)
     ;;                        (console.log node)
     ;;                        (assign text
     ;;                                (if (and this.exportOptions.customDirectiveHandler
     ;;                                         (get this.exportOptions.customDirectiveHandler node.directiveName))
     
     ;;                                  ;; note this may be a js gotcha: ref converter.js:225
     ;;                                  (|> (get this.exportOptions.customDirectiveHandler
     ;;                                           [node.directiveName])
     ;;                                      (.call this node childText auxData))
     ;;                                  childText)))))
     
     "example" (fn [node]
                 ;; aux-data?
                 (this-as self
                  [:pre (.call getChildText self node)]))

     ;;  :link (fn [node]
     ;;          (var self this)
     ;;          (var srcParameterStripped
     ;;               (node.src.replace (regex "\\?.*$") "")
     ;;               imageExtensionPattern
     ;;               (re-pattern (str "(?i)("
     ;;                                (-> ["bmp" "png" "jpeg" "jpg" "gif" "tiff"
     ;;                                     "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "svg"]
     ;;                                    (.join "|"))
     ;;                                ")$"))
     ;;               maybe-match
     ;;               (.exec imageExtensionPattern srcParameterStripped))
     ;;          (if (not maybe-match)
     ;;            [:a
     ;;             {:class (self.orgClassName "link")
     ;;              :href node.src}
     ;;             (this.getChildText node)]
     ;;            (let []
     ;;              (var aux-data (compute-aux-data-for-node node))
     ;;              (var imgText (self.getNodeTextContent node))
     ;;              [:img
     ;;               (merge
     ;;                aux-data
     ;;                {:src node.src
     ;;                 :alt imgText
     ;;                 :title imgText})])))

     ;; converter.js:248
     "text" (fn [node insideCodeElement]
              (this-as self
               [:span
                (.call convertText self (aget node "value") insideCodeElement)]))

     "inlineContainer" (fn [node]
                         (this-as self
                           (let [recordHeader false
                                 insideCodeElement false]
                             (.call getChildText
                                    self node recordHeader insideCodeElement))))

     "definitionList" (fn [node]
                        (this-as self
                          (let [recordHeader false
                                insideCodeElement false
                                child-text (.call getChildText self node)
                                aux-data (compute-aux-data-for-node node)]
                            [:dl
                             (merge aux-data
                                    {:class (.orgClassName self "definition-list")})
                             child-text])))

     "orderedList" (fn [node]
                     (this-as self
                      (let [recordHeader false
                            insideCodeElement false
                            child-text (.call getChildText self node)
                            aux-data (compute-aux-data-for-node node)]
                        [:ol
                         (merge aux-data
                                {:class (.orgClassName self "ordered-list")})
                         child-text]
                        ))
                     )
     
     "unorderedList" (fn [node]
                       (this-as this
                         (let [recordHeader false
                               insideCodeElement false]
                           (-> (concat
                                [:ul
                                 (merge (compute-aux-data-for-node node)
                                        {:class (.orgClassName this "unordered-list")})]
                                (.call getChildText this node recordHeader insideCodeElement))
                               (vec)))))

     "listElement" (fn [node]
                     (this-as self
                       (let [child-text (.call getChildText self node)
                             aux-data (compute-aux-data-for-node node)]
                         (if (aget node "isDefinitionList")
                           ;; definitionItem
                           [:dl
                            [:dt
                             {:class (get-org-class-name "description-term")}
                             (.call convertNodesInternal self node.term)]
                            [:dd
                             {:class (get-org-class-name "description-description")}
                             child-text]]

                           ;; listItem
                           (if
                               ;; (aget self "exportOptions" "suppressCheckboxHandling")
                               (get-export-option self "suppressCheckboxHandling")
                             [:li aux-data child-text]
                             (let [listItemAttributes {}
                                   listItemText child-text
                                   maybe-match nil
                                   ;; FIXME
                                   ;; maybe-match (-> (regex "^\\s*\\[(X| |-)\\]([\\s\\S]*)")
                                   ;;                 (.exec listItemText))
                                   ]
                               
                               [:li
                                (merge listItemAttributes
                                       aux-data)
                                (if (not maybe-match)
                                  listItemText
                                  ;; embed checkbox
                                  (let [checkboxIndicator (get maybe-match 1)
                                        checkboxAttributes {:type "checkbox"}
                                        listItemText (get maybe-match 2)]
                                    ;; (case checkboxIndicator
                                    
                                    ;;   ("X"
                                    ;;    (set checkboxAttributes "checked" true)
                                    ;;    (set listItemAttributes "data-checkbox-status" "done"))
                                    ;;   ("-"
                                    ;;    (set listItemAttributes "data-checkbox-status" "intermediate"))
                                    ;;   (default
                                    ;;    (set listItemAttributes "data-checkbox-status" "undone")))
                                    [:input checkboxAttributes
                                     listItemText]))]))))))

     ;;  :macro (fn [node]
     ;;           (var self this)
     ;;           ;; (console.warn "HANDLING MACRO"
     ;;           ;;               node.name
     ;;           ;;               node.args)
     ;;           ;; (console.log node)
     ;;           (handle-org-macro node.name node.args))

     ;;  :timestamp (fn [node]
     ;;               ;; (console.warn "DO TIMESTAMP ========")
     ;;               ;; (console.log node)
     ;;               (var self this)
     ;;               [:code
     ;;                {:class (self.orgClassName "timestamp")}
     ;;                (|> (new Date node.date)
     ;;                    (.toISOString))])

     "code" (fn [node]
              (this-as self
                (let [recordHeader false
                      insideCodeElement false]
                  [:code
                   {:class (get-org-class-name "code")}
                   (.call getChildText self node recordHeader insideCodeElement)])))

     "header" (fn [node & optional-args]
                (this-as self
                  (let [recordHeader (first optional-args)
                        insideCodeElement (second optional-args)

                        child-text (recur-get-text node)
                        ;; Parse task status
                        task-status (cond (re-find #"^TODO\s+" child-text)
                                          "todo"

                                          (re-find #"^DONE\s+" child-text)
                                          "done"

                                          :else
                                          nil)
                        header-keyword (keyword
                                        (str "h"
                                             (+ (aget self "headerOffset")
                                                (aget node "level"))))]
                    
                    (concat
                     [header-keyword
                      (merge {}
                             (if (aget node "sectionNumberText")
                               {:id (str "header-"
                                         (clojure.string/replace
                                          (aget node "sectionNumberText")
                                          #"\." "-"))}))]
                     (if task-status
                       [:span
                        {:class (str "task-status " task-status)}
                        (clojure.string/upper-case task-status)])

                     ;; ref
                     ;; childText = this.inlineTag("span", childText.substring(0, 4), {
                     ;;   "class": "task-status " + taskStatus
                     ;; }) + childText.substring(5);
                     (let [out-text (if task-status
                                      (subs child-text 4)
                                      child-text)]
                       (if (aget node "sectionNumberText")
                         [:span
                          {:class "section-number"}
                          out-text]
                         [out-text]))))))
     
     "paragraph" (fn [node]
                   (this-as self
                    (let [recordHeader nil
                          insideCodeElement nil
                          aux-data (compute-aux-data-for-node node)]
                      
                      (concat
                       [:p aux-data]
                       (if-let [children (aget node "children")]
                         (.call convertNodesInternal self
                                children
                                recordHeader
                                insideCodeElement)))
                      ;; TODO verify that we don't need the
                      ;; empty "p" hack as necessary in miccup
                      )))

     "table" (fn [node child-text aux-data]
               (this-as self
                 (let [child-text (.call getChildText self node)
                       aux-data (compute-aux-data-for-node node)]
                   [:table
                    (merge (or aux-data {})
                           {:border 1})
                    [:tbody child-text]])))
     
     "tableRow" (fn [node child-text aux-data]
                  (this-as self
                   [:tr (.call getChildText self node)]))

     "tableHeader" (fn [node child-text aux-data]
                     (this-as self
                       [:th (.call getChildText self node)]))

     "tableCell" (fn [node child-text aux-data]
                   (this-as self
                     [:td (.call getChildText self node)]))
     }
    )

  
  
  (let []
    (doto converter-hiccup
      (aset "prototype"
            (clj->js
             {"__proto__" (aget OrgConverter "prototype")
              "orgClassName" (fn [class-name]
                               (js/console.warn "get class name")
                               (this-as self
                                 (str
                                  ;; (aget self "exportOptions" "htmlClassPrefix")
                                  (get-export-option self "htmlClassPrefix")
                                  class-name)))

              "orgId" (fn [id]
                        (js/console.warn "get :orgId")
                        (this-as self
                          (str (aget self "exportOptions" "htmlIdPrefix") id)))                         

              "getConverter" getConverter
              
              "escapeSpecialChars" escapeSpecialChars

              ;; :getNodeTextContent (fn [node]
              ;;                       (var self this)
              ;;                       (if (= node.type OrgNode.types.text)
              ;;                         (self.escapeSpecialChars node.value)
              ;;                         (if (not node.children)
              ;;                           nil
              ;;                           (|> (node.children.map
              ;;                                self.getNodeTextContent
              ;;                                self)
              ;;                               (.join "")))))
              
              "getChildText" getChildText
              

              ;; ;; also don't know how this interacts with link: below
              ;; :linkURL
              ;; (fn [text]
              ;;   (var self this)
              ;;   (var maybe-match (|> converter-urlPattern
              ;;                        (.exec text)))
              ;;   (if (not maybe-match)
              ;;     text
              ;;     (let []
              ;;       (var match (get maybe-match 0)
              ;;            full-url (if (< (match.indexOf "://") 0)
              ;;                       (+ "http://" match)
              ;;                       match))
              ;;       [:a {:class (self.orgClassName "link linkURL")
              ;;            :href full-url} match])))

              "convertText" convertText

              ;; ;; incomplete and doesn't work correctly -- headers seem to go through different logic
              ;; ;; so this only does paragraph dates
              ;; :makeDate
              ;; (fn [text]
              ;;   (var self this)
              ;;   (var date-pattern (regex "[\\[<](\\d{4}-\\d{2}-\\d{2})(.[\\d:]+)?[\\]>]"))
              ;;   (var maybe-match (date-pattern.exec text))
              ;;   (if maybe-match
              ;;     [:strong {:class (self.orgClassName "date")}
              ;;      (get maybe-match 1)]
              ;;     text))

              
              
              ;; converter.js:265
              "convertNodesInternal" convertNodesInternal 

              "convert" (fn [recordHeader]
                          (this-as self
                            (js/console.warn "CALLING CONVERT" self)
                            (let [js-nodes (aget self "orgDocument" "nodes")
                                  n-nodes (aget js-nodes "length")]
                              (loop [i 0
                                     out []]
                                (if-not (< i n-nodes)
                                  out
                                  (let [node (aget js-nodes i)]
                                    (recur (inc i)
                                           (conj
                                            out
                                            (-> (.call getConverter self node)
                                                (.call self node recordHeader))))))))))
              
              "convertPreformatted"
              (fn [node child-text aux-data]
                (js/console.log "%cPREFORMATTED" "border:1px solid lime;color:white;background:navy;")
                [:pre aux-data child-text])
              })
            )
      ))
  )




