(ns attempt-1
  (:require [util]))

;; basic idea is to take each column, count the number of times each letter
;; can appear in that column (for the remaining possibilities) and then
;; score each words (where score is just the sum of the letters in each col added
;; together. 
;; 
;; example: so aback, and 10 a in col 0, 20 b's in col 1, ... 
;; would have a score of 10 + 20 + ...


(defn build-point-map [words]
  (for [c (range 5)]
    (reduce (fn [acc w]
              (update acc (get w c) inc))
            (into {} (for [a (range 26)]
                       [(char (+ (int \a) a)) 0]))
            words)))

(comment
  (build-point-map util/sample-words)
  ;
  )

(defn generate-score [point-map word]
  (reduce-kv (fn [acc i m]
               (+ acc (m (get word i))))
             0
             (vec point-map)))

(comment
  (generate-score (build-point-map util/sample-words)
                  "zbzzz")
  ; 
  )

(defn generate-guess
  "words-list is a full list of all allowed guesses. prior-guesses
   is a map of guesses like {abcde __GY_, aabbc GY__G}"
  [words-list prior-guesses]
  (let [processed-guesses (->> prior-guesses
                               (map #(apply util/process-guess %))
                               util/compile-guesses)
        legal-guesses (filter (partial util/is-word-possible? processed-guesses)
                              words-list)
        point-map (build-point-map legal-guesses)
        [guess _] (reduce (fn [[_ best :as acc] word]
                             (let [score (generate-score point-map word)]
                               (if (> score best)
                                 [word score]
                                 acc)))
                           ["" 0]
                           legal-guesses)]
    guess))

(comment
  (generate-guess util/sample-words {"exxxx" "Y____"})
  ;
  )
