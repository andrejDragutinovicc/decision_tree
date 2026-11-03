(ns seminarski-rad.data-test
  (:require [midje.sweet :refer :all]
            [seminarski-rad.data :as data]))

;( fact "parsed-surrvived mapping 1 into YES"
;       ( data/parse_survived "1") => :yes )

(tabular
  (fact "Testing all mappings for parse-survived"
        (data/parse_survived ?input) => ?output)
  ?input ?output
  "1" :yes
  "0" :no
  "" :no)
(facts "parse-sex strings to keyword"
       (data/parse_sex "male") => :male
       (data/parse_sex "female") => :female
       (data/parse_sex nil)=> nil)

(facts "parse-embarked  "
       (data/parse_embarked "S") => :southampton
       (data/parse_embarked "Q")=> :queenstown
       (data/parse_embarked "C")=> :cherbourg
       (data/parse_embarked "")=> nil
       )

(def first-row
  {"PassengerId" "1"
   "Survived"    "0"
   "Pclass"      "3"
   "Name"        "Braund, Mr. Owen Harris"
   "Sex"         "male"
   "Age"         "22"
   "SibSp"       "1"
   "Parch"       "0"
   "Ticket"      "A/5 21171"
   "Fare"        "7.25"
   "Cabin"       ""
   "Embarked"    "S"})

(fact "parse-row test with first row"
      (data/parse_row first-row)=>
      (just {
             :age      22.0
             :pclass   3.0
             :fare     (roughly 7.25 0.001)
             :sex      :male
             :survived :no
             :embarked :southampton
             }))

(fact "load-data test - testing just a first row"
      (data/load_data ) => (just [(contains {:sex :male, :survived :no})])
      (provided
        (data/load-raw) => [first-row]))