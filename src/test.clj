(ns test
  (:require [clojure.string :as str]
            [util]
            [attempt-1]
            [attempt-2]))

(def answers (->> "resources/words.txt"
                  slurp
                  str/split-lines))

(def possible-guesses (->> "resources/words-long.txt"
                           slurp
                           str/split-lines))

(def possible-words (concat answers possible-guesses))

(defn guess-word [guess-fn word]
  (loop [guesses {}]
    (let [guess (guess-fn possible-words guesses)
          score (util/score-word word guess)]
      ;; (println (count guesses) guess score)
      (if (= score "GGGGG")
        (count guesses)
        (recur (assoc guesses guess score))))))

(comment
  (guess-word attempt-2/generate-guess "peach")
  ;
  )

(defn get-n-randomnly [possible-words n]
  (loop [words #{}]
    (let [new-words (conj words (rand-nth possible-words))]
      (if (= n (count new-words))
        new-words
        (recur new-words)))))

(defn get-avg-score [guess-fn words]
  (let [scores (reduce-kv (fn [acc i w]
                            (when (= 0 (mod i 10))
                              (println i))
                            (conj acc (guess-word guess-fn w)))
                          []
                          (vec words))]
    (double (/ (reduce + scores) (count scores)))))

(defn get-avg-scores [guess-fns attempts]
  (let [words (get-n-randomnly possible-words attempts)]
    (for [f guess-fns]
      (get-avg-score f words))))

(comment
  (get-n-randomnly possible-words 5)
  (get-avg-score attempt-2/generate-guess (get-n-randomnly possible-words 5))
  (get-avg-scores [attempt-1/generate-guess
                   attempt-2/generate-guess] 100)
  ;
  )
