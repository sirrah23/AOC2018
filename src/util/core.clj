(ns util.core)

(defn getLines 
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
      (doall (line-seq rdr))))

(def parseInt #(Integer/parseInt %))
