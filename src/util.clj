(ns util
  (:require [clojure.string :as str]))

(defn- get-greens [word guess]
  (reduce (fn [acc i]
            (cond-> acc
              (= (nth word i) (nth guess i))
              (conj i)))
          #{}
          (range (count word))))

(defn- inc-null-safe [x]
  (if (some? x)
    (inc x)
    1))

(defn- get-yellows [word guess greens]
  (let [remaining-letters (reduce (fn [acc i]
                                    (if (greens i)
                                      acc
                                      (update acc
                                              (nth word i)
                                              inc-null-safe)))
                                  {}
                                  (range (count guess)))]
    (:yellows (reduce (fn [{:keys [remaining-letters yellows] :as acc} i]
                        (let [l (nth guess i)]
                          (cond
                            (greens i) acc

                            (pos? (get remaining-letters l 0))
                            {:remaining-letters (update remaining-letters l dec)
                             :yellows (conj yellows i)}

                            :else acc)))
                      {:remaining-letters remaining-letters
                       :yellows #{}}
                      (range (count guess))))))

(defn score-word
  "returns a string of 'Y' 'G' ' ' to score a wordle guess"
  [word guess]
  (let [greens (get-greens word guess)
        yellows (get-yellows word guess greens)]
    (str/join ""
              (for [i (range (count word))]
                (cond
                  (greens i) "G"
                  (yellows i) "Y"
                  :else "_")))))

(comment
  (score-word "basic" "bassy")
  (score-word "basic" "axxxx")
  (score-word "basic" "aaxxx")
  (score-word "basic" "xxxaa")
  (score-word "aabbb" "xaxaa")
  (score-word "abcde" "ccccc")
  (score-word "peach" "sores")
  (score-word "peach" "teene")
  ;
  )


;; if we have a green for a column, that's the answer
;; if we have a yellow for a column, we know it's _not_ that column's
;; answer
;; we also need to know if yellow + green in the same column yields
;; multiple guesses

;; from a single guess, we can glean greens, yellows, extra letters
;; then we need to roll that up to know greens, columns that have noes
;; and letters we still need to find the answer to
(defn process-guess [guess result]
  (let [green-yellow-counts
        (reduce-kv (fn [acc i result]
                  (let [l (get guess i)]
                    (condp = result
                      \G (update acc l inc-null-safe)
                      \Y (update acc l inc-null-safe)
                      \_ acc)))
                {}
                (vec result))]
    (reduce (fn [acc i]
              (let [answer (get result i)
                    l (get guess i)]
                (condp = answer
                  \G (-> acc
                         (update-in [:letters-matched l] inc-null-safe)
                         (update :cols assoc i \G))
                  \Y (-> acc
                         (update-in [:letters-matched l] inc-null-safe)
                         (update :cols assoc i \Y))
                  \_ (assoc-in acc 
                               [:max-count l]
                               (get green-yellow-counts l 0)))))
            {:letters-matched {}
             :cols {}
             :guess guess
             :max-count {}}
            (range (count result)))))

(comment
  (process-guess "abcde" "__GY_")
  (process-guess "abdde" "__GY_")
  (process-guess "aaaaa" "__YG_")
  ;
  )

(defn compile-guesses
  [guesses]
  (let [{:keys [cols matched-letters max-count]}
        (reduce (fn [acc {:keys [letters-matched cols guess max-count]}]
                  (let [add-col-data
                        (reduce (fn [acc2 [c l]]
                                  (condp = l
                                    \G (assoc-in acc2 [:cols c :is] (get guess c))
                                    \Y (update-in acc2 [:cols c :cant] conj (get guess c))))
                                acc
                                cols)

                        add-matched-letters
                        (reduce (fn [acc2 [l c]]
                                  (update-in acc2 [:matched-letters l] #(max c (or % 0))))
                                add-col-data
                                letters-matched)]
                    (update add-matched-letters
                            :max-count
                            into
                            max-count)))
                {:cols {0 {:is nil :cant #{}}
                        1 {:is nil :cant #{}}
                        2 {:is nil :cant #{}}
                        3 {:is nil :cant #{}}
                        4 {:is nil :cant #{}}}
                 :matched-letters {}
                 :max-count {}}
                guesses)
        green-letters (reduce (fn [acc [_ {:keys [is]}]]
                                (cond-> acc
                                  is (update is inc-null-safe)))
                              {}
                              cols)
        extra-letters (->> green-letters
                           (reduce (fn [acc [l c]]
                                     (cond-> acc
                                       (acc l) (update l - c)))
                                   matched-letters)
                           (filter (fn [[_ c]]
                                     (pos? c)))
                           (into {}))]
    {:yellow-letters extra-letters
     :cols cols
     :max-count max-count}))

(comment
  ;; aaabb
  (compile-guesses [{:letters-matched {\a 2}, :cols {0 \G, 4 \Y} :guess "axxxa" :max-count {\x 0}}
                    {:letters-matched {\a 2}, :cols {0 \G, 1 \G} :guess "aaxxx" :max-count {\x 0}}
                    {:letters-matched {\b 2}, :cols {0 \Y, 1 \Y} :guess "bbbxx" :max-count {\x 0 \b 2}}
                    {:letters-matched {\a 3}, :cols {0 \G, 1 \G 4 \Y} :guess "aaxax" :max-count {}}])
  ;
  )

(defn is-word-possible?
  "guesses is a map of guess -> result'"
  [{:keys [yellow-letters cols max-count]} word]
  (let [didnt-exceed-letter-max
        (reduce (fn [_ [l c]]
                  (if (> c (get max-count l 5))
                    (reduced false)
                    true))
                true
                (frequencies word))

        extra-letters
        (reduce (fn [acc [i {:keys [is cant]}]]
                  (let [letter (get word i)]
                    (cond
                      is (if (= is letter)
                           acc
                           (reduced false))
                      (cant letter) (reduced false)
                      :else (update acc letter inc-null-safe))))
                {}
                cols)]
    (when (and didnt-exceed-letter-max extra-letters)
      (reduce (fn [_ [l c]]
                (if (>= (get extra-letters l 0) c)
                  true
                  (reduced false)))
              true
              yellow-letters))))

(comment
  (is-word-possible? {:yellow-letters {\a 1, \b 2},
                      :cols
                      {0 {:is \a, :cant #{\b}},
                       1 {:is \a, :cant #{\b}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{\a \x}}}
                      :max-count {\c 0}}
                     "aaabb")
  ;; no, 2nd letter doesn't match
  (is-word-possible? {:yellow-letters {\a 1, \b 2},
                      :cols
                      {0 {:is \a, :cant #{\b}},
                       1 {:is \a, :cant #{\b}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{\a \x}}}
                      :max-count {\c 0}}
                     "abbab")
  ;; no 5th letter can't be x 
  (is-word-possible? {:yellow-letters {\a 1},
                      :cols
                      {0 {:is \a, :cant #{\b}},
                       1 {:is \a, :cant #{\b}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{\a \x}}}
                      :max-count {\c 0}}
                     "aaaax")
  ;; no, missing extra a
  (is-word-possible? {:yellow-letters {\a 1},
                      :cols
                      {0 {:is \a, :cant #{\b}},
                       1 {:is \a, :cant #{\b}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{\a \x}}}
                      :max-count {\c 0}}
                     "aabbb")
  ;; has a miss in it
  (is-word-possible? {:yellow-letters {},
                      :cols
                      {0 {:is \a, :cant #{\b}},
                       1 {:is \a, :cant #{\b}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{\a \x}}}
                      :max-count {\c 0}}
                     "aabbc")

  ;; no filter at all (should be true)
  (is-word-possible? {:yellow-letters {},
                      :cols
                      {0 {:is nil, :cant #{}},
                       1 {:is nil, :cant #{}},
                       2 {:is nil, :cant #{}},
                       3 {:is nil, :cant #{}},
                       4 {:is nil, :cant #{}}}
                      ::max-count {\c 0}}
                     "aback")
  
  ;; e is a letter but also a miss, (cuz a word with multiple e's)
  (is-word-possible? {:yellow-letters {}, 
                      :cols {0 {:is nil, :cant #{}}, 
                             1 {:is \e, :cant #{}}, 
                             2 {:is nil, :cant #{}}, 
                             3 {:is nil, :cant #{\e}}, 
                             4 {:is nil, :cant #{}}}, 
                      :max-count {\e 1 \n 0 \o 0 \r 0 \s 0 \t 0}}
                     "peach")
  ;
  )

(def sample-words (->> "resources/words-sample.txt"
                       slurp
                       str/split-lines))

(comment
  (->> sample-words
       (filter #(is-word-possible? {:yellow-letters {\a 2},
                                    :cols
                                    {0 {:is nil, :cant #{\c}},
                                     1 {:is nil, :cant #{}},
                                     2 {:is nil, :cant #{}},
                                     3 {:is nil, :cant #{}},
                                     4 {:is nil, :cant #{}}}
                                    :misses #{\c \t}}
                                   %)))
  ;
  )
