(ns attempt-2
  (:require [attempt-1]
            [util]))

;; similar to attempt 1, but i want to penalize duplicate letters in the word
;; to hopefully hone in faster.

;; we're going to half the score of duplicate columns. specifically, we're going
;; to find the "largest" non duplicate and give it full credit, but half the
;; others

;; (defn generate-score [point-map word]
;;   (reduce-kv (fn [acc i m]
;;                (+ acc (m (get word i))))
;;              0
;;              (vec point-map)))

(defn find-duplicate-score [col-scores indicies]
  (let [scores (->> indicies
                    (map (fn [i]
                           (get col-scores i)))
                    (sort >))]
    (->> scores
         rest
         (map (fn [x] (int (/ x 2))))
         (reduce +)
         (+ (first scores)))))

(comment
  (find-duplicate-score [2 2 4 10 6]
                        #{0 1 2 3 4}))

(defn generate-score [point-map word]
  (let [score-by-col (reduce-kv (fn [acc i m]
                                  (conj acc (m (get word i))))
                                []
                                (vec point-map))
        duplicate-sets (->> word
                            frequencies
                            (filter (fn [[_ c]] (> c 1)))
                            (map first)
                            (map (fn [l]
                                   [l (->> (for [i (range (count word))]
                                             (when (= l (get word i))
                                               i))
                                           (filter identity))]))
                            (into {}))
        full-score-indices (->> duplicate-sets
                                (map second)
                                flatten
                                (reduce disj (into #{} (range (count word)))))
        duplicate-scores (for [[_ indicies] duplicate-sets]
                           (find-duplicate-score score-by-col indicies))]
    (+ (reduce + duplicate-scores)
       (reduce + (->> full-score-indices
                      (map (fn [i]
                             (get score-by-col i))))))))

(comment
  (generate-score (attempt-1/build-point-map util/sample-words)
                  "ababc")
  ;; 4 1 3 0 0
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
        point-map (attempt-1/build-point-map legal-guesses)
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
