(ns seminarski-rad.data
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


;;clojure documentory domaci cas 4
;;https://www.youtube.com/watch?v=Y24vK_QDLFg&pp=ygUTY2xvanVyZSBkb2N1bWVudGFyedIHCQkwDAGHKiGM7w%3D%3D

;4. POGLAVLJE KNJIGE
;4clojure nastavak
; doraditi seminarski


;simple vs eazy Nije domaci ali moze da pomogne

;Domaci za neki tamo cas:
;1. larman PART1 INTRODUCTION
;2. pogledati knjige iz eXtream Proggraminga (XP) - extreme programming installed pdf(15 strana chapter 1)
;3. USer stories applayed - Ako nas szanima vise prikupljanje porisnickih zahteva(Pocetak knjige - prvih 30 strana)



(defn load-raw
  "Loading all rows in dataset"
  []
  (with-open [r (io/reader (io/resource "titanic.csv"))]
    (let [rows   (csv/read-csv r)
          header (first rows)
          body   (rest rows)]
      (mapv (fn [row] (zipmap header row)) body))))

(defn parse_sex
  [s]
  (keyword s)
  )

(defn parse_survived
  "1 -> :yes / 0 -> :no"
  [s]
  (if (= s "1") :yes :no))


(def parse_embarked
  "docstring"
  {
   "S" :southampton
   "C" :cherbourg
   "Q" :queenstown
   }
  )

(defn parse_row
  "Parsing a single row"
  [row]
  {
   :age (parse-double (get row "Age"))
   :pclass  (parse-double (get row "Pclass"))
   :fare (parse-double (get row "Fare"))
   :sex (parse_sex (get row "Sex"))
   :survived (parse_survived (get row "Survived"))
   :embarked (parse_embarked (get row "Embarked"))
   }
  )

(defn load_data
  "Loading and parsing data"
  []
  (mapv parse_row (load-raw))

  )
(def columns [:pclass :sex :age :fare :embarked])
(def target :survived)


