(ns wksymclj.data-manipulation.transformation)

;; #+pure
(def collapse-tree-to-1-layer
  #_(comment
      (= (collapse-tree-to-1-layer
          {:top {:ver 2
                 :sku "ABC"
                 :style {:width 2
                         :color "blue"
                         :data {:left "side"
                                :right "wrong"}}}})
       
         {[:top :ver] 2,
          [:top :sku] "ABC",
          [:top :style :width] 2,
          [:top :style :color] "blue",
          [:top :style :data :left] "side",
          [:top :style :data :right] "wrong"}))
  (fn self
    ([m]
     (self m []))
    ([m klist]
     (cond (map? m)
           (->> m
                (map (fn [[k v]]
                       (self v (if (sequential? k)
                                 (vec (concat klist k))
                                 (conj klist k)))))
                (into {}))
           
           :else
           (if (empty? klist)
             m
             [klist m])))))

;; #+pure
(defn fill-out-all-missing-keys [seq-of-maps]
  ;; given seq of maps, assumed flat, go through all contained maps
  ;; (records), extract keys and ensure every record contains the same
  ;; keys, fill nil if not.  this is mainly a preprocessing step for a
  ;; tabular renderer.
  (let [filler-map (->> seq-of-maps
                        (map keys)
                        (apply concat)
                        (set)
                        (map (fn [k] [k nil]))
                        (into {}))]
    (->> seq-of-maps
         (map (partial merge filler-map)))))

;; #+pure
(defn find-paths-to-sequential-children
  [data]
  #_(comment
      (= (find-paths-to-sequential-children
          {:a 1
           :c [1 2]
           :d {:e {:f [1 2 3]}}
           :b 2})
         '([:c] [:d :e :f])))
  (let [is-terminal?
        (fn [x] (every? (complement sequential?) x))
        
        extract-terminal
        (fn self [maybe-terminal]
          (if (is-terminal? maybe-terminal)
            maybe-terminal
            (self (first maybe-terminal))))
        
        preprocess
        (fn self
          ([val] (self val []))
          ([val khist]
           (cond (map? val)
                 (->> val
                      (map (fn [[k v]]
                             (self v (conj khist k))))
                      (remove nil?))
                 
                 (sequential? val)
                 khist
                 
                 :else
                 nil)))]
    (->> (preprocess data)
         (tree-seq
          (fn [x]
            (and (sequential? x)
                 (not (is-terminal? x))))
          rest)
         (map extract-terminal)
         (remove empty?))))

;; #+pure
(defn dissoc-in [m path]
  (cond (empty? path)
        m

        (= 1 (count path))
        (dissoc m (first path))

        :else
        (update-in m (drop-last path) dissoc (last path))))

;; #+pure
(defn remove-all-empty-children [m]
  (->> m
       (clojure.walk/postwalk
        (fn [x]
          (if-not (map? x)
            x
            (->> x
                 (map (fn [[k v]]
                        (when (or (not (seqable? v))
                                  (not (empty? v)))
                          [k v])))
                 (into {})))))))

;; #+pure
(def transform-to-tabular
  #_(comment
      (let [input {:stuff
                   {:more [{:set "a"
                            :hist [{:key 1}
                                   {:key 2}]}
                           {:set "b"
                            :hist [{:key 3}
                                   {:key 4}]}]}}
            output [{[:stuff :more :set] "a", [:stuff :more :hist :key] 1}
                    {[:stuff :more :set] "a", [:stuff :more :hist :key] 2}
                    {[:stuff :more :set] "b", [:stuff :more :hist :key] 3}
                    {[:stuff :more :set] "b", [:stuff :more :hist :key] 4}]]
        (= (-> input
               (transform-to-tabular)
               (fill-out-all-missing-keys)
               (vec))
           output)))
  
  (fn self
    ([data]
     (self data []))
    ([data khist]
     (self data khist {}))
    ([data khist mmerge]
     (cond (map? data)
           (if (and
                false
                (every?
                 (comp not sequential?)
                 (vals data)))
             (do
               (->> data
                    (map (fn [[k v]]
                           [(conj khist k) v]))
                    (into mmerge)))
             (let [
                   _shared (->> data
                                (remove (fn [[_ v]]
                                          (sequential? v)))
                                (map (fn [[k v]]
                                       [(conj khist k) v]))
                                (into {})
                                (collapse-tree-to-1-layer))

                   sequential-paths (find-paths-to-sequential-children data)
                   shared (->> (reduce
                                (fn [m path]
                                  (dissoc-in m path))
                                data
                                sequential-paths)
                               (remove-all-empty-children)
                               (map (fn [[k v]]
                                      [(conj khist k) v]))
                               (into {})
                               (collapse-tree-to-1-layer))

                   ]
               (if (empty? sequential-paths)
                 (->> data
                      (map (fn [[k v]]
                             [(conj khist k) v]))
                      (into mmerge))
                 
                 (->> sequential-paths
                      (map (fn [path]
                             [path (get-in data path)]))

                      (map
                       (fn [[sequential-key
                             sequential-vals]]
                         (->> sequential-vals
                              (map-indexed
                               (fn [i val]
                                 (self val
                                       (vec (concat khist sequential-key))
                                       shared))))))
                      (flatten)))))

           (sequential? data)
           (->> data
                (map (fn [subd]
                       (self subd khist mmerge)))
                (apply concat))
           
           :else
           data))))

(def transform-to-indexed-tabular
  #_(comment
      (let [input {:stuff
                   {:more [{:set "a"
                            :hist [{:key 1}
                                   {:key 2}]}
                           {:set "b"
                            :hist [{:key 3}
                                   {:key 4}]}]}}
            output [{[:stuff :more 0 :set] "a", [:stuff :more 0 :hist 0 :key] 1}
                    {[:stuff :more 0 :set] "a", [:stuff :more 0 :hist 1 :key] 2}
                    {[:stuff :more 1 :set] "b", [:stuff :more 1 :hist 0 :key] 3}
                    {[:stuff :more 1 :set] "b", [:stuff :more 1 :hist 1 :key] 4}]

            xfm (-> input
                    (transform-to-indexed-tabular)
                    (vec))]
        (= xfm output)))
  
  (fn self
    ([data]
     (self data []))
    ([data khist]
     (self data khist {}))
    ([data khist mmerge]
     (cond (map? data)
           (let [sequential-paths (find-paths-to-sequential-children data)
                 shared (->> (reduce
                              (fn [m path]
                                (dissoc-in m path))
                              data
                              sequential-paths)
                             (remove-all-empty-children)
                             (filter (fn [[_ v]] v))
                             (map (fn [[k v]]
                                    [(conj khist k) v]))
                             (into {})
                             (collapse-tree-to-1-layer))]
             
             (if (empty? sequential-paths)
               (->> data
                    (map (fn [[k v]]
                           [(conj khist k) v]))
                    (into mmerge))
               
               (->> sequential-paths
                    (map (fn [path]
                           [path (get-in data path)]))

                    (map
                     (fn [[sequential-key
                           sequential-vals]]
                       (->> sequential-vals
                            (map-indexed
                             (fn [row-index val]
                               (let [combined-key (vec (concat khist sequential-key [row-index]))]
                                 (self val combined-key shared)))))))
                    (flatten))))

           (sequential? data)
           (->> data
                (map-indexed
                 (fn [i subd]
                   (js/console.log i (pr-str subd))
                   (self subd khist mmerge)))
                (apply concat))
           
           :else
           data))))
