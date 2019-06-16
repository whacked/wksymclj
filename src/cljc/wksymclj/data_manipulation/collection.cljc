(ns wksymclj.data-manipulation.collection)

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

(defn get-proper-subset-keys [base comp]
  (filter (set (keys base))
          (keys comp)))

(defn is-submap? [smaller bigger]
  (let [required-keys (get-proper-subset-keys smaller bigger)]
    (and (= (count smaller)
            (count required-keys))
         (every? identity
                 (map #(= (smaller %) (bigger %))
                      required-keys)))))

;; from https://stackoverflow.com/a/15771713
(defn map-all [f & colls]
  (lazy-seq
   (when (some seq colls)
     (cons (apply f (map first colls))
           (apply map-all f (map rest colls))))))

(defn get-all-unique-keys [entry-colls]
  (->> entry-colls
       (map (fn [entry-coll]
              (->> entry-coll
                   (map keys))))
       (flatten)
       (distinct)))
