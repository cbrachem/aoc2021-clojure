(ns aoc2021.day03 (:require [clojure.string :as str]))

(defn parse-binary [s] (Integer/parseInt s 2))

(defn seq-to-int [a] (parse-binary (apply str a)))

(defn sort-elements [a] (sort-by val (sort-by key a)))

(defn char-frequencies [input]
  (->> input
       (str/split-lines)
       (map seq)
       (apply map list)
       (map frequencies)
       (map sort-elements)))

(defn gamma-rate [freqs]
  (->> freqs
       (map second)
       (map first)
       (seq-to-int)))

(defn epsilon-rate [freqs]
  (->> freqs
       (map first)
       (map first)
       (seq-to-int)))

(defn power-consumption [freqs]
  (* (gamma-rate freqs) (epsilon-rate freqs)))

(defn part1 [input]
  (->> input
       (char-frequencies)
       (power-consumption)))

(defn sorted-chars-at-position [i number-strings]
  (->> number-strings
       (map #(nth % i))
       (frequencies)
       (sort-elements)
       (keys)))

(defn most-common-char-at [i number-strings]
  (last (sorted-chars-at-position i number-strings)))

(defn least-common-char-at [i number-strings]
  (first (sorted-chars-at-position i number-strings)))

(defn filter-most-common-char-at [i number-strings]
  (let [s (most-common-char-at i number-strings)]
    (filter #(= s (nth % i)) number-strings)))

(defn filter-least-common-char-at [i number-strings]
  (let [s (least-common-char-at i number-strings)]
    (filter #(= s (nth % i)) number-strings)))

(defn calculate-rating [filter-fn number-strings]
  (parse-binary (loop [lines number-strings
                       i 0]
                  (if (< 1 (count lines))
                    (recur (filter-fn i lines) (inc i))
                    (first lines)))))

(def oxygen-generator-rating
  (partial calculate-rating filter-most-common-char-at))

(def co2-scrubber-rating
  (partial calculate-rating filter-least-common-char-at))

(defn life-support-rating [number-strings]
  (* (oxygen-generator-rating number-strings) (co2-scrubber-rating number-strings)))

(defn part2 [input]
  (life-support-rating (str/split-lines input)))

(comment

  (+ 1 1)

  (nth "12345" 3)

  (sort-by val (sort-by key (frequencies '(\1 \0 \0 \1))))

  (def test-input (slurp "input/day03-test.txt"))
  (def test-lines (str/split-lines test-input))

  (oxygen-generator-rating test-lines)
  (co2-scrubber-rating test-lines)

  (> 1 2)

  (part1 test-input)

  (part1 (slurp "input/day03-input.txt"))

  (part2 (slurp "input/day03-input.txt")))