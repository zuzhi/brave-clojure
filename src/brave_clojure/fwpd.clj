(ns brave-clojure.fwpd)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int [str]
  (Integer/valueOf str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

;; (defn glitter-filter
;;   [minimum-glitter records]
;;   (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn glitter-filter
  [minimum-glitter records]
  (map :name (filter #(>= (:glitter-index %) minimum-glitter) records)))

;; (defn append
;;   [suspects suspect]
;;   (conj suspects suspect))

(def validations {:name nil?
                  :glitter-index nil?})

(defn validate
  [validations record]
  (every? true?
          (map
           (fn [[key validate-fn]]
             (not (validate-fn (key record))))
           validations)))

(defn append
  [suspects suspect]
  (if (validate validations suspect)
    (conj suspects suspect)
    suspects))

(defn to-csv-string
  [records]
  (clojure.string/join
   (map (fn [record]
          (str (:name record) ", " (:glitter-index record) "\n"))
        records)))
