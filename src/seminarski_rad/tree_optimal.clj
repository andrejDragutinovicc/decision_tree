(ns seminarski-rad.tree-optimal
  (:require [seminarski-rad.data :as data] ))




(defn get-sorted-frequenses
  "Returning a map with sorted key value"
  [rows column]
  (into (sorted-map) (frequencies (map column rows))))

(defn counting-positivs-from-ds
  "docstring"
  [rows kolona value]

  (count (filter (fn [d]
                   (and
                     (some? (kolona d)) (== value (kolona d) )
                              (= :yes (data/target d))))
                 rows)))

(defn inf-gain-compact
  "Calculating inf-gain with gini index but with simpify formula just with total, toatal+, l, l+"
  [total total-p l l-p]
  (let [l-n (- l l-p)
        r-p (- total-p l-p)
        r (- total l)
        r-n (- r r-p)]
    (+ (/ (double (* l-p l-n))
          l)
       (/ (double (* r-p r-n))
          r))))

(defn best-split [rows]
  )

(defn leaf [rows]
  )

(defn split-rows
  "docstring"
  [question rows]
  )

(defn build-tree
  "Buillding the tree"
  [rows]
  (let [b-split (best-split rows)]
    (if (or (nil? b-split) (>= 0.1 (:gain b-split)))
      (leaf rows)

      (let [[da ne] (split-rows (:question b-split) rows )]
        {
         :question (:question b-split)
         :n (count rows)
         :yes (build-tree da)
         :no (build-tree ne)}))))
