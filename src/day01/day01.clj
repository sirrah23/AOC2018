(ns one.one_code
  (:require [util.core :as util]))

(defn toInt 
  [freqStr]
  (let [
    sign (get freqStr 0)
    num (read-string (subs freqStr 1))]
  (if (= \+ sign) num (* -1 num))))
 
(defn sum 
  [nums] 
  (reduce + nums))

(defn firstFreqTwice 
  [freqs]
  (let [freqStream (cycle freqs)]
    (loop [total 0 
          [currFreq & restFreqs] freqStream 
          freqCounts {}]  
      (let [totalCount (inc (get freqCounts total 0))]
        (if (= 2 totalCount) 
          total
          (recur (+ total currFreq) restFreqs (into freqCounts [{total totalCount}])))))))

(def inputFile "./src/day01/day01_input.txt")

; Answer to part A
(println (sum (map toInt (util/getLines inputFile))))

;Answer to part B
(println (firstFreqTwice (map toInt (util/getLines inputFile))))
