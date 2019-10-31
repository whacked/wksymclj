(ns wksymclj.ui.data-renderer
  (:require [reagent.core :as reagent :refer [atom]]))

(defn clj->hiccup
  "
  example:
  (reagent/render
   [(fn []
      (clj->hiccup @data))]
   container)
  "
  [orig-data & custom-args]
  (let [{:keys [base-style
                object-table-style
                object-row-style
                object-key-style
                object-value-style
                array-list-style
                array-item-style
                content-renderer]
         :or {base-style {:font-size "10pt"
                          :font-family "Mono"}
              object-table-style {:display "table"
                                  :border "1px solid green"
                                  :padding "0.25em"
                                  :margin "0.25em"}
              object-row-style {:display "table-row"
                                :margin "4px"
                                :padding "80px"}
              object-key-style {:display "table-cell"
                                :padding "0.1em"
                                :background "#CFC"}
              object-value-style {:display "table-cell"
                                  :background "#FFC"}
              array-list-style {:border-left "4px double #999"}
              array-item-style {:padding "0.25em"
                                :margin "0.25em"
                                :border "1px dashed #CCC"}
              content-renderer str}} (->> custom-args
                                          (partition 2)
                                          (map vec)
                                          (into {}))
        
        ->attr (fn ->attr
                 ([ext-style]
                  (->attr ext-style {}))
                 ([ext-style ext-attr]
                  (merge {:style (merge base-style
                                        ext-style)}
                         ext-attr)))
        recurse (fn recurse [data]
                  [:div
                   (cond
                     (map? data)
                     [:div
                      (->attr object-table-style)
                      (->> data
                           (map
                            (fn [[k sub-data]]
                              ^{:key (str "div-" k "-" sub-data)}
                              [:div
                               (->attr object-row-style)
                               [:div
                                (->attr object-key-style)
                                k]
                               [:div
                                (->attr object-value-style)
                                (apply clj->hiccup (cons sub-data custom-args))]])))]

                     (sequential? data)
                     [:ol
                      (->attr array-list-style
                              {:start 0})
                      (->> data
                           (map-indexed
                            (fn [i sub-data]
                              ^{:key (str "li-" i "." sub-data)}
                              [:li
                               (->attr array-item-style)
                               (recurse sub-data)])))]
                     
                     :else
                     [:span
                      {:style (cond (boolean? data)
                                    (merge {:font-family "Monospace"}
                                           (if (= true data)
                                             {:background "red"
                                              :color "white"}
                                             {:background "red"
                                              :color "white"}))

                                    (number? data)
                                    {:font-family "Monospace"
                                     :color "red"}
                                    
                                    :else
                                    {})}
                      (content-renderer data)
                      ])])]
    (recurse orig-data)))
