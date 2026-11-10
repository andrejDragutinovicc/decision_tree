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

;Test commita?