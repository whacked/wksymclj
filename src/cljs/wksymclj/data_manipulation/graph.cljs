(ns wksymclj.data-manipulation.graph)

;; #+pure
(defn get-edge-midpt [pt-seq]
  (let [npt (count pt-seq)
        midn (quot npt 2)]
    (if (odd? npt)
      (nth pt-seq midn)
      (let [p1 (nth pt-seq midn)
            p2 (nth pt-seq (dec midn))]
        {:x (/ (+ (:x p1) (:x p2)) 2)
         :y (/ (+ (:y p1) (:y p2)) 2)}))))

;; #+pure
(defn state-declaration-to-flow-graph
  "
  `state-declaration-list` is a list of 6-entry seqs, like:
    :step \"01c-2-branch-right\"                 ;; id
    \"try new branch?\"                          ;; label
    [[true \"cool\" \"some-aggregation-step\"]   ;; transitions: [match-condition edge-label next-step-id]
     [false \"fail\" \"02-fallback-procedure\"]]
    {}                                           ;; other
    validator-function                           ;; returns the match-condition
  "
  [state-declaration-list]
  {:node-list
   (->> state-declaration-list
        (map (fn [[type name label state-list & _]]
               (for [state state-list]
                 {:label label
                  :name name
                  :width 144
                  :height 20
                  :type type})))
        (apply concat))

   :edge-list
   (->> state-declaration-list
        (#(map
           vector
           (drop-last %)
           (drop 1 %)))
        (#(for [[pre post] %]
            (let [[pr-type pr-name pr-label pr-state-list & _] pre
                  [po-type po-name po-label po-state-list & _] post]
              (for [pr-state pr-state-list
                    po-state po-state-list]
                (let [po-specified (if (= 3 (count pr-state))
                                     (last pr-state)
                                     po-name)]
                  [pr-name po-specified {:label
                                         (str
                                          (first pr-state)
                                          "(" (second pr-state) ")")}])))))
        (apply concat))})

;; #+pure
(defn is-subvector?
  "`needle` and `haystack` are seq-able types
  returns index where `needle` is first found in `haystack`
  if exists, else returns nil"
  [needle haystack]
  (let [i-hs-max (inc (- (count haystack) (count needle)))
        i-nd-max (count needle)
        ]
    (loop [i-hs 0]
      (if (< i-hs i-hs-max)
        (let [match-length
              (loop [i-nd 0]
                (let [i-hs-combined (+ i-hs i-nd)]
                  (if (= i-nd i-nd-max)
                    i-nd
                    (let [nd-val (nth needle i-nd)
                          hs-val (nth haystack i-hs-combined)]
                      (if (= nd-val hs-val)
                        (recur (inc i-nd)))))))]
          (if match-length
            i-hs
            (recur (+ i-hs (or match-length 1)))))))))

;; #+pure
(defn get-proper-subset-keys [base comp]
  (filter (set (keys base))
          (keys comp)))

;; #+pure
(defn is-submap? [smaller bigger]
  (let [required-keys (get-proper-subset-keys smaller bigger)]
    (and (= (count smaller)
            (count required-keys))
         (every? identity
                 (map #(= (smaller %) (bigger %))
                      required-keys)))))

;; #+pure
(defn get-next-state-list [transition-spec now-state now-world]
  (->> transition-spec
       (map (fn [[name next world]]
              (if (and (= now-state name)
                       (is-submap? world now-world))
                next)))
       (filter identity)))
