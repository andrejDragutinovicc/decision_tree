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
  "Returning map of the probabilities for the target class"
  [rows]
  (counter-prob (class-counts rows)))


(defn gini_formula
  "docstring"
  [map-probs]

  (- 1.0 (reduce + (map (fn [p] (* p p)) (vals map-probs))))
  )

(defn gini
  "gini index"
  [rows]

  (if (empty? rows)
    0.0
    (let [map-probs (class-probabilities  rows)]
      (gini_formula map-probs))))

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
      (let [[left right] (split-rows question rows)
            prob-left (/ (count left) (double total))
            prob-right (/ (count right) (double total))]
        (- (gini rows)
           (+ (* prob-left (gini left))
              (* prob-right (gini right))))))))


(defn get-distinct-values
  "from column get distinct values (sorted)"
  [rows key]
    (sort (distinct (filter some? (map key rows)) )))

(defn potencial-questions
  "Giving a all possible questions for given rows"
  [rows]

  (for [column data/columns value (get-distinct-values rows column)]
    {:column column :value value}))

(defn get-all-split-scores
  "From rows to [{:question {:column :fare, :value 6.8583}, :gain 0.00648201987721736}
  {:question {:column :fare, :value 7.0458}, :gain 0.0063332490032864674}...]"
  [rows]

  (map (fn [q] {:question q :gain (inf-gain q rows)}) (potencial-questions rows)))


(defn best-split
  "Finding best plit out of all possible questions "

  [rows]

  (let [scores (get-all-split-scores rows)]
    (if (not (empty? scores))
      (apply max-key :gain scores)
      nil)))

(defn leaf
  "Making leaf out of the remaining rows "
  [rows]
  (let [class-count (class-counts rows)]
    {:leaf true :n (reduce + (vals class-count)) :prediction (when (seq class-count)
                                                              (key (apply max-key val class-count)))
     :probability (val (apply max-key val (counter-prob class-count)))}))

(defn build-tree
  "Building the tree out of the given rows"
  [rows]
  (let [best (best-split rows)]
    (if (or (nil? best) (>= 0.01 (:gain best)))
      (leaf rows)
      (let [[da ne] (split-rows (:question best) rows)]
                   {
                    :question (:question best)
                    :n (count rows)
                    :yes (build-tree da)
                    :no (build-tree ne)}))))