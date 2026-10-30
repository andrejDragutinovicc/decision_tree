(ns seminarski-rad.tree
  (:require [seminarski-rad.data :as data]))

(defn class-counts
  "Counting resolt"
  [rows]
  (frequencies (map data/target rows)))
