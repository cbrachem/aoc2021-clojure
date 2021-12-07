(ns aoc2021.day04 (:require [clojure.string :as str]))

(defn parse-int [s] (Integer/parseInt s))

(defn all-ones [a] (every? #(= 1 %) a))

(defn get-rows [field] (partition 5 field))

(defn get-columns [field] (for [i (range 5)] (take-nth 5 (drop i field))))

(defn get-rows-and-columns [field] (concat (get-rows field) (get-columns field)))

(defn field-has-bingo [field]
  (->> field
       (map second)
       (get-rows-and-columns)
       (map all-ones)
       (some true?)))

(defn get-field-with-bingo [fields]
  (first (filter field-has-bingo fields)))

(defn game-has-bingo [game] (some field-has-bingo (:fields game)))

(defn mark-number [n field]
  (map (fn [[a b]] (if (= n a) (list a 1) (list a b))) field))

(defn unmarked-numbers [field]
  (map first (filter (fn [x] (= 0 (second x))) field)))

(defn init-bingo-field [s]
  (->> s
       (re-seq #"\d+")
       (map parse-int)
       (map #(list % 0))))

(defn parse-input [input]
  (let [s (str/split input #"\r?\n(\r?\n)+")]
    {:numbers (->> (first s)
                   (re-seq #"\d+")
                   (map parse-int))
     :fields (->> (rest s)
                  (map init-bingo-field))
     :last-number nil}))

(defn play-step [game]
  (let [n (first (:numbers game))]
    {:numbers (rest (:numbers game))
     :fields (map (partial mark-number n) (:fields game))
     :last-number n}))

(defn get-end-info [game]
  (let [field (get-field-with-bingo (:fields game))]
    {:field field
     :unmarked-numbers (unmarked-numbers field)
     :last-number (:last-number game)}))

(defn play-bingo [game]
  (if (game-has-bingo game)
    (get-end-info game)
    (play-bingo (play-step game))))

(defn play-anti-bingo [game]
  (if (game-has-bingo game)
    (if (= 1 (count (:fields game)))
      (get-end-info game)
      (play-anti-bingo (update game :fields #(filter (comp not field-has-bingo) %))))
    (play-anti-bingo (play-step game))))

(defn final-score [info]
  (* (:last-number info) (apply + (:unmarked-numbers info))))

(defn part1 [input]
  (->> (parse-input input)
       (play-bingo)
       (final-score)))

(defn part2 [input]
  (->> (parse-input input)
       (play-anti-bingo)
       (final-score)))

(comment
  (+ 1 1)

  (get-field-with-bingo (:fields endgame))

  (def test-input (slurp "input/day04-test.txt"))

  (def test-field (first (:fields (parse-input test-input))))

  (every? #(= 1 %) '(1 1 1 1 1))

  (play-bingo (parse-input test-input))

  (final-score (play-anti-bingo (parse-input test-input)))

  (count (:fields (update endgame :fields)))

  (update endgame :fields first)

  (part1 test-input)

  (part1 (slurp "input/day04-input.txt"))

  (part2 (slurp "input/day04-input.txt"))


  (some field-has-bingo (:fields endgame))


  (partition 5 (range 25)))

