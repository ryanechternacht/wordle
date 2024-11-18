(ns test
  (:require [clojure.string :as str]
            [util]
            [attempt-1]))

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
      (println (count guesses) guess score)
      (if (= score "GGGGG")
        (count guesses)
        (recur (assoc guesses guess score))))))

(comment
  (guess-word attempt-1/generate-guess "peach")
  ;
  )
