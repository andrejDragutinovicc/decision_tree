(ns seminarski-rad.tree-optimal
  (:require [seminarski-rad.data :as data] ))




(defn get-sorted-frequenses
  "Returning a map with sorted key value"
  [rows column]
  (into (sorted-map) (frequencies (map column rows))))


(defn get-positiv
  "Returning a map with sorted key value"
  [rows column]
  (into (sorted-map) (frequencies (map column (filter (fn [x] (= :yes (data/target x)) ) rows  )))))


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



(defn cumulative-frequency
  "From to vectors of sorted frequences and positive frequences we form vector of acumulative values from bouth"
  [f p]
  (first
    (reduce (fn [[acc cum cum-p] [value cnt]]
              (let [cum   (+ cum cnt)
                    cum-p (+ cum-p (get p value 0))]
                [(conj acc [value cum cum-p]) cum cum-p]))
            [[] 0 0]
            f)))

(defn get-splits-from-column
  "Returning vector of questions like this question = {:column :age :value 30 :gain 208.90} for one column in a rows"
  [rows total total-p column]

  (let [cumulative (cumulative-frequency (get-sorted-frequenses rows column) (get-positiv rows column))
        ]


    (reduce (fn [acc x] (conj acc
                              {:column column :value (first x) :gain (inf-gain-compact total total-p (second x) (last x))})
              ) [] cumulative)))

(defn min-gain
  "returning min value of a sequence of questions ex: question = {:column :age :value 30 :gain 208.90}"
  [questions]
  (reduce (fn [m x] (if (< (:gain x) (:gain m)) x m))
          (first questions) (rest questions)))

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
