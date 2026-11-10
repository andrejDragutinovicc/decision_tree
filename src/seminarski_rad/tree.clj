(ns seminarski-rad.tree
  (:require [seminarski-rad.data :as data]))

(defn class-counts
  "Counting resolt"
  [rows]
  (frequencies (map data/target rows)))
;
;(defn class-propabilities
;  "calculating persantage of each class "
;  [rows]
;  (let [sum (count rows)]
;    (update-vals (class-counts rows) (fn [x] (/ x sum)))
;    )
;  )

(defn counter-prob
  "docstring"
  [map-count]
  (let [total (reduce  +  (vals map-count))]
    (update-vals map-count (fn [x] (double (/ x total))))
    )
  )

(defn class-probabilities
  "docstring"
  [rows]
  (counter-prob (class-counts rows)))


(defn gini
  "gini index"
  [rows]

  (if (empty? rows)
    0.0
    (let [probs (vals (class-probabilities rows))]
      (- 1.0 (reduce + (map (fn [v] (* v v) ) probs))))))

(defn match
  "Can question split the row"
  [question row]

  (let [{:keys [column value]} question row-value (get row column)]

    (cond
      (nil? row-value) false
      (number? value ) (>= row-value value)
      :else (= row-value value))))

(defn split-rows
  "docstring"
  [ question rows]

  (let [groups (group-by (partial match question) rows)]
    [(vec (groups true))
     (vec (groups false))]))

(defn inf-gain
  "docstring"[question rows]
  (let [total (count rows)]
    (if (zero? total)
      0.0
      (let [[da ne] (split-rows question rows)
            prob-da (/ (count da) (double total))
            prob-ne (/ (count ne) (double total))]
        (- (gini rows)
           (+ (* prob-da (gini da))
              (* prob-ne (gini ne))))))))

