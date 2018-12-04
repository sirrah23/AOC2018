(defn getLines
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (doall (line-seq rdr))))

(defn pts
  [x y w l]
  (for [i (range w)
        j (range l)]
    [(+ x i) (+ y j)]))

;(into #{} (reduce concat [] (map #(apply pts %) [[1 3 4 4] [3 1 4 4] [5 5 2 2]])))

(def parseInt #(Integer/parseInt %))

(defn parseLine
  [line]
  (let [[_ x y w l] (re-matches #"#\d+ @ (\d+),(\d+): (\d+)x(\d+)" line)]
    (map parseInt [x y w l])))

(defn countDupVals
  [fs]
  (count (filter #(> % 1) (vals fs))))

(defn computeMultiClaim
  ([claims]
   (computeMultiClaim claims {}))
  ([[claim & restClaims] counts]
   (if (nil? claim)
     (countDupVals counts)
     (let [ptsForClaim (apply pts claim)]
       (recur restClaims (merge-with + counts (frequencies ptsForClaim)))))))

(def inputFile "03_input.txt")

(println (computeMultiClaim (map parseLine (getLines inputFile))))