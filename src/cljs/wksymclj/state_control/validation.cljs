(ns wksymclj.state-control.validation)


(defn Any [val]
  val)

(defn Number [val]
  (number? val))

(defn NumberRangeMaker [min max]
  (fn [val]
    (and (<= min val)
         (<= val max))))

(defn DiscreteNumber [min max]
  (fn [val]
    (and (<= min val)
         (<= val max))))

(defn Date [val]
  val)

(defn NOP-validator [& _]
  nil)

(defn all-non-nil [m]
  (every?
   #(not (nil? %))
   (vals m)))
