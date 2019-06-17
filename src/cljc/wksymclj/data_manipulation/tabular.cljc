(ns wksymclj.data-manipulation.tabular)

;; NOTE:
;; * rowset-2d-coll:
;; [rowset-2d
;;  rowset-2d
;;  ...
;; ]
;; * rowset-2d:
;; [rowset
;;  rowset
;;  ...
;; ]
;; * rowset:
;; [{:somekey someval}
;;  {:somekey someval}
;;  ...
;; ]
;; * special keys
;; :name -- required
;; :group -- special
;; :anything-else -- picked up dynamically

(defn convert-rowset-2d-to-coords [rowset-2d-coll]
  (map-indexed
   ;; convert design sequences to compact lookup for faster combinatoris
   (fn [row-index sequence]
     (->> sequence
          (map-indexed
           (fn [col-index cell]
             [row-index col-index]))))
   rowset-2d-coll))

(defn make-coords-to-rowset-2d-converter [rowset-2d]
  (fn [rowset-2d-coords-coll]
    (->> rowset-2d-coords-coll
         (map
          ;; coords ->> rowset-2d
          (fn [rowset-2d-coords]
            (->> rowset-2d-coords
                 (map
                  (fn [coords-row]
                    (->> coords-row
                         (map
                          (fn [coord]
                            (when coord
                              (get-in rowset-2d coord)))))))))))))

(defn get-column
  "rows is a 2d iterable
  [[1 2 3]
   [4 5 6]] etc"
  [col-index rows]
  (map #(nth % col-index) rows))

(defn remove-empty-columns-from-rowset-2d-coll [rowset-2d-coll]
  (->> rowset-2d-coll
       (mapv
        ;; remove empty columns
        (fn [rowset-2d]
          (let [non-nil-indexes
                (->> (first rowset-2d)
                     (count)
                     (range)
                     (remove
                      (fn [column-index]
                        (->> (get-column column-index rowset-2d)
                             (every? nil?)))))]
            (->> rowset-2d
                 (mapv (fn [row]
                         (map (partial nth row) non-nil-indexes)))))))))

(defn get-rowset-2d-columns [rowset-2d]
  (->> (first rowset-2d)
       (count)
       (range)
       (map (fn [column-index]
              (get-column column-index rowset-2d)))))

(defn map-by-view
  [{:keys [rows cols]} func rowset-2d]
  {:pre [(or (= rows :all) (= 2 (count rows)))
         (or (= cols :all) (= 2 (count cols)))]}
  
  (comment
    (->> [[{:a "a1"} {:b "b1"} {:c "c1"} {:d "d1"} {:e "e1"}]
          [{:a "a2"} {:b "b2"} {:c "c2"} {:d "d2"} {:e "e2"}]
          [{:a "a3"} {:b "b3"} {:c "c3"} {:d "d3"} {:e "e3"}]
          [{:a "a4"} {:b "b4"} {:c "c4"} {:d "d4"} {:e "e4"}]
          [{:a "a5"} {:b "b5"} {:c "c5"} {:d "d5"} {:e "e5"}]]
         (map-by-view
          {:rows [nil 2]
           :cols [2 -2]}
          (fn [coord cell]
            (assoc cell :YA "hey")))
         (mapv (fn [row]
                 (println row)))))

  (let [total-rows (count rowset-2d)
        total-cols (count (first rowset-2d))
        
        convert-index
        (fn [dimension-size index nil-value]
          (cond (nil? index) nil-value
                (< index 0) (+ dimension-size
                               index)
                :else index))
        convert-row-index (partial convert-index total-rows)
        convert-col-index (partial convert-index total-cols)

        [r0 r1] (if (= rows :all) [0 total-rows])
        [c0 c1] (if (= cols :all) [0 total-cols])
        
        row-beg (convert-row-index r0 0)
        row-end (convert-row-index r1 total-rows)
        col-beg (convert-col-index c0 0)
        col-end (convert-col-index c1 total-cols)]
    (->> rowset-2d
         (map-indexed
          (fn [row-index row]
            (if (or (< row-index row-beg)
                    (< row-end row-index))
              row
              (->> row
                   (map-indexed
                    (fn [col-index cell]
                      (if (or (< col-index col-beg)
                              (< col-end col-index))
                        cell
                        (func
                         [row-index col-index] cell)))))))))))

(defn auto-tag-groups [processed-rowset-2d]
  (let [last-row-index (dec (count processed-rowset-2d))
        last-col-index (->> processed-rowset-2d
                            (last)
                            (count)
                            (dec))
        
        column-group-mapping
        (->> processed-rowset-2d
             (map drop-last) ;; drop the last row
             (get-rowset-2d-columns)
             (map drop-last) ;; drop the last column
             (map-indexed
              (fn [column-index column-cells]
                [column-index
                 (->> column-cells
                      (map :group)
                      (remove nil?)
                      (set)
                      (first))]))
             (into {})
             ((fn [m]
                (let [total-groups (count m)]
                  (loop [column-index 0
                         auto-group-id 1
                         group-id-assigned? (set (vals m))
                         out m]
                    (if (>= column-index total-groups)
                      out
                      (let [existing-group-id
                            (m column-index)
                            
                            assignable-group-id
                            (loop [check-id auto-group-id]
                              (if (not (group-id-assigned?
                                        check-id))
                                check-id
                                (recur (inc check-id))))]
                        (recur (inc column-index)
                               assignable-group-id
                               (conj group-id-assigned?
                                     (if (nil? existing-group-id)
                                       assignable-group-id))
                               (if-not (nil? existing-group-id)
                                 out
                                 (assoc out column-index assignable-group-id))))))))))]
    (->> processed-rowset-2d
         (map-by-view
          {:rows :all
           :cols :all}
          (fn [[row-index col-index] cell]
            (if (or (= row-index last-row-index)
                    (= col-index last-col-index))
              cell
              (let [existing-group (:group cell)]
                (assoc cell
                       :group (column-group-mapping
                               col-index)
                       :_auto-group (not existing-group)))))))))
