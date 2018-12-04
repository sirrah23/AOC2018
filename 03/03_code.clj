(defn getLines
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (doall (line-seq rdr))))

(defn pts
  [x y w l]
  (for [i (range w)
        j (range l)]
    [(+ x i) (+ y j)]))

(def parseInt #(Integer/parseInt %))

(defn parseLine
  [line]
  (let [[_ x y w l] (re-matches #"#\d+ @ (\d+),(\d+): (\d+)x(\d+)" line)]
    (map parseInt [x y w l])))

(defn countDupVals
  [fs]
  (count (filter #(> % 1) (vals fs))))

(defn computeClaimOverlaps
  ([claims]
   (computeClaimOverlaps claims {}))
  ([[claim & restClaims] counts]
   (if (nil? claim)
     counts
     (let [ptsForClaim (apply pts claim)]
       (recur restClaims (merge-with + counts (frequencies ptsForClaim)))))))

(defn allOnes
  [line freqs]
  (every? #(= 1 %) (map #(get freqs %) line)))

(def inputFile "03_input.txt")

(defn threeA []
  (countDupVals (computeClaimOverlaps (map parseLine (getLines inputFile)))))

(defn threeB []
  (let [lines (map parseLine (getLines inputFile))
        overlaps (computeClaimOverlaps lines)]
    (first (filter #(allOnes (apply pts %) overlaps) lines))))

(println ( threeA ))

(println ( threeB ))