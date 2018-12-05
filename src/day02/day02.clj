(ns day02.day02
  (:require [util.core :as util]))

(defn getCharFrequencies
  [str]
  (map second (seq (frequencies str))))

(defn exactlyTwice [freqs] (some #(= % 2) freqs))

(defn exactlyThrice [freqs] (some #(= % 3) freqs))

(defn truthy? [v] (if v 1 0))

(defn sum [s] (reduce + s))

; Optimize this abysmal trash
(defn twoThreeProduct
  [lines]
  (let [freqs (map getCharFrequencies lines)
        twoSum (sum (map truthy? (map exactlyTwice freqs)))
        threeSum (sum (map truthy? (map exactlyThrice freqs)))]
    (* twoSum threeSum)))

(defn strDiff [s1 s2] (sum (map #(truthy? (not (= %1 %2))) s1 s2)))

(defn oneCharDiff
  [lines]
  (last (for [i (range (count lines))
              j (range (count lines))
              :let [lineA (nth lines i) lineB (nth lines j)]
              :when (= 1 (strDiff lineA lineB))]
          [lineA lineB])))

(def inputFile "./src/day02/day02_input.txt")

(println (twoThreeProduct (util/getLines inputFile)))

(println (oneCharDiff (util/getLines inputFile)))
