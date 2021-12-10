(ns aoc2021.day10 (:require [clojure.string :as str]))

(def brackets (apply hash-map (mapcat seq ["()" "[]" "{}" "<>"])))

(defn is-opening? [c] (contains? brackets c))

(def scores (hash-map \) 3 \] 57 \} 1197 \> 25137))
(def scores2 (hash-map \) 1 \] 2 \} 3 \> 4))

(defn parse-line [line] (seq line))

(defn parse-input [input] (map parse-line (str/split-lines input)))

(defn step [[status acc] c]
  (if (is-opening? c)
    [:ok (conj acc c)]
    (if (= c (brackets (first acc)))
      [:ok (rest acc)]
      (reduced [:mismatch c]))))

(def run-line (partial reduce step '()))

(defn part1 [input]
  (->> input
       (parse-input)
       (map run-line)
       (filter #(= :mismatch (first %)))
       (map #(scores (second %)))
       (apply +)))

(defn part2-score-step [score c]
  (+ (* 5 score) (scores2 c)))

(def part2-score (partial reduce part2-score-step 0))

(defn median [a] (nth (sort a) (int (/ (count a) 2))))

(defn part2 [input]
   (->> input
        (parse-input)
        (map run-line)
        (filter #(= :ok (first %)))
        (map second)
        (map #(map brackets %))
        (map part2-score)
        (median)))

(comment
  (def test-input (slurp "input/day10-test.txt"))
  
  (part1 (slurp "input/day10-input.txt"))
  (part2 (slurp "input/day10-input.txt"))
  
  (conj '(1 2 3) 9)
  
  (keys brackets)
  
 
  
  )