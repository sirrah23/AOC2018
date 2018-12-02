(defn getLines 
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
      (doall (line-seq rdr))))

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
  (loop [currFreq 0 [currNewFreq & restNewFreqs] freqs freqCounts {}]  
    (let [appliedCurrFreqCount (inc (get freqCounts currFreq 0))]
      (if (= 2 appliedCurrFreqCount) 
        currFreq
        (let [freqToLoop (if (nil? restNewFreqs) freqs restNewFreqs)]
          (recur (+ currFreq currNewFreq) freqToLoop (into freqCounts [{currFreq appliedCurrFreqCount}])))))))

(def inputFile "01_input.txt")

; Answer to part A
(println (sum (map toInt (getLines inputFile))))

;Answer to part B
(println (firstFreqTwice (map toInt (getLines inputFile))))