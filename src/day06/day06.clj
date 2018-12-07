(ns day06.day06
  (:require [util.core :as util]))

(defn parseInt [x] (Integer/parseInt x))

(defn indexOf
  [x ys]
  (loop [val x
         lst ys
         idx 0]
    (cond
      (empty? lst) nil
      (= val (first lst)) idx
      :else (recur val (rest lst) (+ 1 idx)))))

(defn abs [x]
  (if (neg? x) (* x -1) x))

(defn manhattanDist
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn determineLabel [refPt compPts]
  (let [dists (map #(manhattanDist refPt %) compPts)
        distFreqs (frequencies dists)
        minDist (reduce min dists)]
    (cond
      (= minDist 0) nil
      (> (get distFreqs minDist) 1) nil
      :else (get compPts (indexOf minDist dists)))))

(defn buildPtMap
  [pts]
  (reduce #(assoc %1 %2 []) {} pts))

(defn appendPtMap
  [ptMap [label val]]
  (merge-with into ptMap {label [val]}))

(defn computeSize
  [pts]
  (reduce max (flatten pts)))

(defn getGrid [size]
  (for [i (range size) j (range size)] [i j]))

(defn containsBorder [size pts]
  (loop [res []
         pts pts]
    (cond
      (empty? pts) false
      (some #(or (= 0 %) (= size %)) (first pts)) true
      :else (recur (conj res (first pts)) (rest pts)))))

(defn getLargestAreaSize
  [pts]
  (let [size (computeSize pts)
        grid (getGrid size)
        theMap (buildPtMap pts)
        labels (map #(determineLabel %1 pts) grid)]
    (inc
     (reduce max
             (map count
                  (filter #(not (containsBorder size %))
                          (map second
                               (dissoc (reduce appendPtMap theMap (map vector labels grid)) nil))))))))

(def inputFile "./src/day06/day06_input.txt")

(defn parsePoint
  [line]
  (let [[_ x y] (re-matches #"(\d+), (\d+)" line)]
    [(parseInt x) (parseInt y)]))

(println (getLargestAreaSize (vec (map parsePoint (util/getLines inputFile)))))
