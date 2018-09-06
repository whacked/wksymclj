(ns wksymclj.codec.org-mode
  (:require [cljs.nodejs :as nodejs]
            [clojure.string]
            [com.rpl.specter :as spct])
  (:require-macros
   [com.rpl.specter :refer [select transform]]))


(def $print-debug-to-console? true)
(if $print-debug-to-console?
  (do
    (defn dlog [& argv]
      (apply js/console.log argv))
    (defn dinfo [& argv]
      (apply js/console.info argv))
    (defn dwarn [& argv]
      (apply js/console.warn argv)))
  (do
    (defn dlog [& _] nil)
    (defn dinfo [& _] nil)
    (defn dwarn [& _] nil)))

(def Org (nodejs/require "org"))
(def OrgParser (aget Org "Parser"))
(def OrgConverter (aget (nodejs/require "org/lib/org/converter/converter.js")
                        "Converter"))
(def OrgNode (aget (nodejs/require "org/lib/org/node.js")
                   "Node"))

(def $org-js-html-class-prefix "org-")
(def $org-js-html-id-prefix "org-")
(def $org2hiccup-default-options
  {:htmlClassPrefix $org-js-html-class-prefix
   :htmlIdPrefix $org-js-html-id-prefix})

(defn org2html
  ([org-src]
   (org2html {}))
  ([org-src custom-config]
   (let [default-config {:htmlClassPrefix $org-js-html-class-prefix
                         :htmlIdPrefix $org-js-html-id-prefix
                         :headerOffset 0
                         :exportFromLineNumber false
                         :suppressSubscriptHandling false
                         :suppressAutoLink false}
         parser (new OrgParser)
         org-document (.parse parser org-src)
         org-htmldoc (.convert org-document
                               (aget Org "ConverterHTML")
                              (clj->js (merge default-config
                                               custom-config)))]
     ;; INSPECT:
     ;; (js/console.dir org-htmldoc)
     ;; > { title, contentHTML, tocHTML, toc }
     (.toString org-htmldoc) ;; Rendered HTML
     )))

(def compute-aux-data-for-node
  (fn [input-node]
    ;; currently does nothing with the node data
    ;; and returns an empty map
    (let [org-inline-container-type (aget OrgNode "types" "inlineContainer")
          node (loop [focus-node input-node]
                 (if (and (aget focus-node "parent")
                          (= (aget focus-node "parent" "type")
                             org-inline-container-type))
                   (recur (aget focus-node "parent"))
                   focus-node))]
      (loop [attributesText ""
             attributesNode (aget node "previousSibling")
             out {}]
        ;; attributesNode contains e.g.
        ;; - firstChild
        ;; - fromLineNumber: int
        ;; - lastChild
        ;; - parent
        ;; - previousSibling
        ;; - type: "listElement" | ...
        ;; ;; if it contains a directive, then it also contains
        ;; - directiveName: "example" | ...
        ;; - directiveRawValue: ":session mysession ..."
        ;; - directiveArguments: [":session" "mysession" ...]
        ;; - directiveOptions
        ;; - type: "directive"
        (if-not (and attributesNode
                     (= attributesNode.type OrgNode.types.directive)
                     (= attributesNode.directiveName "attr_html"))
          out
          (recur
           (str attributesText
                (aget attributesNode "directiveRawValue")
                " ")
           (aget attributesNode "previousSibling")
           out))))))

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
  (comment  ;; example
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
      (dlog "converting..." self)
      (let [record-headers true]
        (doto self
          (.initialize
           org-document
           (-> (merge (->> $org2hiccup-default-options
                           (map (fn [[k v]]
                                  [[k v]
                                   [(name k)
                                    v]]))
                           (apply concat)
                           (into {}))
                      (js->clj export-options :keywordize-keys true))
               (clj->js)))
          (aset "result"
                (-> (.convert self record-headers)
                    (de-nestify))))))))

(defn org2hiccup [org-src]
  (comment
    (cljs.pprint/pprint
     (org2hiccup org-src)))
  (concat
   [:div {}]
   (-> (new OrgParser)
       (.parse org-src)
       (.convert converter-hiccup))))

(defn get-export-option [org-object option-name]
  (let [option-key (if (keyword? option-name)
                     (name option-name)
                     option-name)]
    (some-> (aget org-object "exportOptions")
            (aget option-name))))

(def
  ;; converter.js:350
  ;; http://daringfireball.net/2010/07/improved_regex_for_matching_urls
  converter-urlPattern (re-pattern "(?i)\\b(?:https?://|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\\\".,<>?«»“”‘’])"))

(declare converterMapping)
(declare escapeSpecialChars)

(defn get-node-type [node]
  (aget node "type"))

(defn getNodeTextContent [node]
  (this-as self
    (if (= (get-node-type node)
           (aget OrgNode "types" "text"))
      (.call escapeSpecialChars self (aget node "value"))
      (if-not (aget node "children")
        nil
        (->> (aget node "children")
             (array-seq)
             (map (fn [node]
                    (.call getNodeTextContent self node)))
             (apply str))))))

(defn getConverter [node]
  (let [node-type (aget node "type")]
    (dlog "matched converter" (converterMapping node-type))
    (or (converterMapping node-type)
        (converterMapping "default"))))

(defn convertNode [node recordHeader insideCodeElement]
  ;; see converter.js:89
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
          aux-data (compute-aux-data-for-node node)
          converter (getConverter node)]
      (dinfo
       (str
        "[%c"
        (aget node "type")
        "%c] convertNode: self is "
        self)
       "color:white;background:blue;font-weight:bold;"
       "color:black;background:none;")
      (.call converter self node recordHeader insideCodeElement))
    
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
  #_(dlog "%cCONVERTING INTERNAL"
          "color:white;background:lime;")
  (this-as self
    (let [n-nodes (aget nodes "length")
          ]
      #_(dlog "convertnode" n-nodes convertNode)
      (when (< 0 n-nodes)
        (loop [i 0
               out []]
          (if-not (< i n-nodes)
            out
            (recur (inc i)
                   (conj
                    out
                    (.call convertNode self
                           (aget nodes i)
                           recordHeader insideCodeElement)))))))))

(defn getChildText [node recordHeader insideCodeElement]
  (this-as self
    (let []
      (dwarn "get :getChildText")
      (when (aget node "children")
        (.call
         convertNodesInternal
         self
         (aget node "children")
         recordHeader
         insideCodeElement)))))

(def replaceMap
  ;; html.js:314
  { ;; [replacing pattern predicate]
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
                          (do (dwarn "WARNING: NO EXPORT OPTIONS")
                              {}))

          default-text (fn [text & _] text)
          
          
          makeSubscripts (if-let [f (aget self "makeSubscripts")]
                           f (do (dwarn "WARNING: no makesubscripts")
                                 default-text))
          linkURL (if-let [f (aget self "linkURL")]
                    f (do (dwarn "WARNING: no linkURL")
                          default-text))
          makeDate (if-let [f (aget self "makeDate")]
                     f (do (dwarn "WARNING: no makeDate")
                           default-text))]
      (cond-> (escapeSpecialChars text insideCodeElement)
        (and (not (exportOptions :suppressSubScriptHandling))
             (not insideCodeElement))
        (do
          ;; FIXME BYPASS
          text)

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
                 {:class (.orgClassName self "dashed")}
                 (.call getChildText self node)]))

   "horizontalRule" (fn [node] [:hr])

   "bold" (fn [node]
            (this-as this
              [:b
               {:class
                (.orgClassName this "bold")}
               (.call getChildText this node)]))

   "italic" (fn [node]
              (this-as self
                [:i
                 {:class (.orgClassName self "italic")}
                 (.call getChildText self node)]))

   "underline" (fn [node]
                 (this-as self
                   [:span
                    {:class (.orgClassName self "underline")}
                    (.call getChildText self node)]))
   
   "directive" (fn [node]
                 (this-as self
                   (let [child-text (.call getChildText self node)
                         aux-data (compute-aux-data-for-node node)]

                     (case (aget node "directiveName")
                       "quote"
                       [:blockquote
                        (merge aux-data
                               {:style {:font-family "Verdana"}})
                        child-text]
                       
                       "example"
                       [:pre aux-data
                        ;; SPECIAL CASE
                        (do
                          ;; FIXME
                          #_(apply-to-textual-element-in-miccup
                             child-text
                             string-with-transclude-to-miccup)
                          child-text)]
                       
                       "src"
                       [:pre
                        [:code
                         (merge aux-data
                                {:class (str "language-"
                                             (or (aget node "directiveArguments" 0)
                                                 "unknown"))})
                         child-text]]
                       
                       [:html "html:"]
                       (throw (js/Error. "fixme"))
                       #_(assign text (this.convertHTML node childText auxData))
                       
                       (do
                         (dwarn "FAILBACK IN DIRECTIVENAME SWITCH ON"
                                (aget node "directiveName"))
                         (dlog node)
                         (if (and (get-export-option self "customDirectiveHandler")
                                  (-> (get-export-option self "customDirectiveHandler")
                                      (get (aget node "directiveName"))))
                           ;; note this may be a js gotcha: ref converter.js:225
                           ;; FIXME
                           ;; (-> (get-export-option self "customDirectiveHandler")
                           ;;     (get [(aget node "directiveName")])
                           ;;     (.call this node child-text aux-data))
                           child-text))))))
   
   "example" (fn [node]
               ;; aux-data?
               (this-as self
                 [:pre (.call getChildText self node)]))

   "link" (fn [node]
            (this-as self
              (let [node-src (aget node "src")
                    srcParameterStripped (-> node-src
                                             (clojure.string/replace #"\\?.*$" ""))
                    imageExtensionPattern
                    (re-pattern (str "(?i)("
                                     (->> ["bmp" "png" "jpeg" "jpg" "gif" "tiff"
                                           "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "svg"]
                                          (interpose "|")
                                          (apply str))
                                     ")$"))
                    maybe-match
                    (.exec imageExtensionPattern srcParameterStripped)]
                
                (if-not maybe-match
                  [:a
                   {:class (.orgClassName self "link")
                    :href node-src}
                   (.call getChildText self node)]
                  (let [aux-data (compute-aux-data-for-node node)
                        imgText (.call getNodeTextContent self node)]
                    
                    [:img
                     (merge
                      aux-data
                      {:src node-src
                       :alt imgText
                       :title imgText})])))))

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
                           {:class (.orgClassName self "description-term")}
                           (.call convertNodesInternal self node.term)]
                          [:dd
                           {:class (.orgClassName self "description-description")}
                           child-text]]

                         ;; listItem
                         (if (get-export-option self "suppressCheckboxHandling")
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
                 {:class (.orgClassName self "code")}
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
                             (dwarn "get class name: " class-name)
                             (this-as self
                               (str (get-export-option self "htmlClassPrefix")
                                    class-name)))

            "orgId" (fn [id]
                      (dwarn "get :orgId")
                      (this-as self
                        (str (aget self "exportOptions" "htmlIdPrefix") id)))                         

            "getConverter" getConverter
            
            "escapeSpecialChars" escapeSpecialChars

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
                          (dwarn "CALLING CONVERT" self)
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
              (dlog "%cPREFORMATTED" "border:1px solid lime;color:white;background:navy;")
              [:pre aux-data child-text])
            })
          )
    ))



;;;;;;;;;;;;;;;;;;;
;; ORGA ADDITION ;;
;;;;;;;;;;;;;;;;;;;
;; npm install --save
;;   unified orga orga-unified orga-rehype
;; NOTE: orga@0.5.2 seems to break. @0.5.1 seems ok
(def unified-js (nodejs/require "unified"))
(def orga-parser (-> (nodejs/require "orga")
                     (aget "Parser")))
(def orga-uni-parser (nodejs/require "orga-unified"))
(def orga-uni-translator (nodejs/require "orga-rehype"))
(def hast-to-html (nodejs/require "hast-util-to-html"))

(defn org->clj-ast
  "read an org string, convert it to unified AST,
   and convert that directly into edn"
  ([org-content]
   (org->clj-ast org-content identity))
  ([org-content preprocessor]
   (let [;; to-clj strips `parent` because it's a pointer
         ;; to the parent node; this causes infinite loop
         to-clj (fn to-clj [ast]
                  (loop [remain (->> (.keys js/Object ast)
                                     (array-seq)
                                     (remove #{"parent"}))
                         out {}]
                    (if (empty? remain)
                      out
                      (let [k (first remain)
                            data (if (= k "children")
                                   (->> (aget ast k)
                                        (map to-clj)
                                        (vec))
                                   (aget ast k))]
                        (recur (rest remain)
                               (assoc out
                                      (keyword k)
                                      (js->clj data)))))))]
     (-> (new orga-parser)
         (.parse (preprocessor org-content))
         (to-clj)))))

(defn clj-ast->hast [clj-ast]
  (-> (unified-js)
      (.use orga-uni-translator)
      (.runSync (clj->js clj-ast))))

(defn org-ast-get-first-headline [org-ast]
  (let [maybe-type (:type org-ast)]
    (if-not maybe-type
      nil
      (if (= maybe-type "headline")
        (some->> (:children org-ast)
                 (filter (fn [node]
                           (= "text" (:type node))))
                 (first)
                 (:value))
        (some->> (:children org-ast)
                 (map org-ast-get-first-headline)
                 (first))))))

(defn hast->hiccup [hast]
  (let [tag (case (aget hast "type")
              "root" :section
              (or (some-> (aget hast "tagName")
                          (keyword))
                  :div))
        prop (or (some-> (aget hast "properties")
                         (js->clj :keywordize-keys true))
                 {})
        base-hiccup (if-let [value (aget hast "value")]
                      [tag prop value]
                      [tag prop])
        children (or (some-> (aget hast "children")
                             (array-seq))
                     [])]
    (->> children
         (mapv hast->hiccup)
         (concat base-hiccup)
         (vec))))

(defn hast-to-html-stringifier [config]
  (this-as this
    (aset this "Compiler"
          (fn compiler [tree]
            (hast-to-html tree)))
    ;; perplexing unified/javascript behavior.
    ;; returning `this` will cause breakage.
    ;; it seems like _anything besides `this`_
    ;; is fine.
    "fakeout!"))

(defn hast-to-hiccupifier [config]
  (this-as this
    (let [compiler
          (fn compiler [hast]
            (hast->hiccup hast))]
      (aset this "Compiler" compiler))
    "fakeout!"))

(defn orga-org->hiccup
  ([org-content]
   (orga-org->hiccup identity))
  ([org-content preprocessor]
   (-> (unified-js)
       (.use orga-uni-parser)
       (.use orga-uni-translator)
       (.use hast-to-hiccupifier)
       (.processSync (preprocessor org-content))
       (aget "contents"))))

(defn parse-drawer
  ;; this may get obviated if orga adds a native drawer parser
  [drawer-text]
  (->> drawer-text
       (clojure.string/split-lines)
       (map clojure.string/trim)
       (map (fn [line]
              (->> line
                   (re-find #"^:([^:]+):\s+(.+)$")
                   (rest)
                   (vec))))
       (into {})))

(defn orga-tree-id-getter [orga-tree]
  (when (= (:type orga-tree)
           "headline")
    ;; see https://orgmode.org/manual/Property-syntax.html
    ;; proper syntax dictates the draw must be right after the headline.
    ;; the `orga-tree` should be a headline element; i.e., like
    ;; {:type "headline",
    ;;  :children
    ;;  [{:type "text", :children [], :value "the headline text"}
    ;;   {:type "drawer",
    ;;    :children [],
    ;;    :name "PROPERTIES",
    ;;    :value "     :ID:       AABBCCDD-1111-2222-3333-44445555EEEE"}],
    ;;  :level 4, 
    ;;  :keyword nil, 
    ;;  :priority nil, 
    ;;  :tags []}
    (let [first-element
          (get-in orga-tree [:children 1])]
      (if (= (:type first-element)
             "drawer")
        (-> (:value first-element)
            (parse-drawer)
            (get "ID"))))))

;; see https://github.com/nathanmarz/specter/issues/201#issuecomment-292269620
(def INDEXED
  "A path that visits v and collects k in [[k v], ...].

  This is useful if you want to collect a path to something, see [[path-walker]]."
  [spct/ALL (spct/collect-one spct/FIRST) spct/LAST])

(def INDEXED-SEQ
  "A selector that visits all elements of a seq, and collects their indices.

  This is useful if you want to collect a path to something, see [[path-walker]]."
  [(spct/view #(map-indexed vector %)) INDEXED])

(def path-walker
  (spct/recursive-path
   [term-pred] p
   (spct/cond-path
    (spct/pred term-pred) spct/STAY
    map? [INDEXED p]
    vector? [INDEXED-SEQ p])))

(defn find-all-headlines
  "returns a collection of maps;
   the maps have structure
   :path = path to the headline ast within the ast, reachable using `get-in`
   :text = the headline text, i.e. text of the first child text element"
  [orga-ast]
  (->> orga-ast
       (select
        (path-walker
         #(and (= "headline" (:type %)))))
       (map (fn [section-path-and-target]
              (let [section-path (vec (drop-last section-path-and-target))
                    ;; the first child of the headline element should be a `type: text`
                    ;; with the headline text. If the headline is empty, its value will
                    ;; just be ""
                    headline-text (get-in (last section-path-and-target)
                                          [:children 0 :value])]
                {:path section-path
                 :text headline-text})))))
