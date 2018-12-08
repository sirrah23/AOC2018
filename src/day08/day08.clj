(ns day08.day08
  (:require [util.core :as util]))

(defn getHeader [numStream]
  (numStream 2))

(defn getMetadata [numStream numMeta]
  (numStream numMeta))

(defn buildTree
  [numStream]
  (let [[numChildren numMeta] (getHeader numStream)
        children (vec (for [_ (range numChildren)] (buildTree numStream)))
        metadata (getMetadata numStream numMeta)]
    {:node [numChildren numMeta]
     :children children
     :metadata metadata}))

(defn numStreamGen [numStream]
  (let [numStream (atom numStream)]
    (fn [numElems]
      (if (= 0 numElems) []
          (let [elems (vec (take numElems @numStream))]
            (swap! numStream #(vec (drop numElems %)))
            elems)))))

(defn metadataTreeSum
  [tree]
  (let [metadata (:metadata tree)
        children (:children tree)
        currSum (reduce + 0 metadata)]
    (reduce +  currSum (if (empty? children) [0] (map metadataTreeSum (:children tree))))))


(def inputFile "./src/day08/day08_input.txt" )
(def inputStreamA (numStreamGen (vec (map util/parseInt (util/getLines inputFile)))))
(println (metadataTreeSum (buildTree inputStreamA)))
