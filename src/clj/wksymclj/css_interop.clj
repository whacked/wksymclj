;; [net.sourceforge.cssparser/cssparser "0.9.27"]
(ns wksymclj.css-interop
  (:import
   [org.w3c.css.sac InputSource]
   [com.steadystate.css.parser
    CSSOMParser SACParserCSS3]))

(defn parse-style-string [style-decl-string]
  ;; ref http://cssparser.sourceforge.net/gettingStarted.html
  (let [parser (CSSOMParser. (SACParserCSS3.))
        decl (.parseStyleDeclaration
              parser
              (InputSource. (java.io.StringReader. style-decl-string)))]
    (loop [out {}
           i-remain (range (.getLength decl))]
      (if (empty? i-remain)
        out
        (let [i (first i-remain)
              prop-name (.item decl i)]
          (recur (assoc out
                        (keyword prop-name)
                        (.getPropertyValue decl prop-name))
                 (rest i-remain)))))))

(defn css->clj [css-string]
  (let [reader (java.io.StringReader. css-string)
        source (InputSource. reader)
        parser (CSSOMParser. (SACParserCSS3.))
        ;; ^CSSStyleSheet
        sheet (.parseStyleSheet parser source nil nil)]
    (let [rules (.getCssRules sheet)
          num-rules (.getLength rules)]
      (loop [out []
             i-remain (range num-rules)]
        (if (empty? i-remain)
          out
          (let [i (first i-remain)
                rule (.item rules i)
                selector (str (first (.getSelectors (.getSelectors rule))))
                style-decl-string (str (.getStyle rule))]
            (recur (conj out
                         [(keyword selector)
                          (parse-style-string style-decl-string)])
                   (rest i-remain))))))))
