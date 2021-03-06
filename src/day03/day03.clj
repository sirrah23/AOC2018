(ns day03.day03
  (:require [util.core :as util]))

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

(def inputFile "./src/day03/day03_input.txt")

(defn threeA []
  (countDupVals (computeClaimOverlaps (map parseLine (util/getLines inputFile)))))

(defn threeB []
  (let [lines (map parseLine (util/getLines inputFile))
        overlaps (computeClaimOverlaps lines)]
    (first (filter #(allOnes (apply pts %) overlaps) lines))))

(println ( threeA ))

(println ( threeB ))
