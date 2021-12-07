(ns aoc2021.day05
  (:require [clojure.string :as str]
            [aoc2021.util :as util]))

(defn inclusive-range [a b]
  (condp apply [a b]
    > (range b (inc a))
    < (range a (inc b))
    = (list a)))

(defn str->line [s] (map util/parse-int (re-seq #"\d+" s)))

(defn is-horizontal-or-vertical? [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2)))

;; signum
(defn sgn [x]
  (condp apply [x 0]
    < -1
    =  0
    >  1))

(defn get-direction [[x1 y1 x2 y2]] (list (sgn (- x2 x1)) (sgn (- y2 y1))))

(defn add-coords [a b] (map + a b))

(defn get-all-points [[x1 y1 x2 y2]]
  (let [dir (get-direction [x1 y1 x2 y2])]
    (loop [points (list (list x1 y1)) [x' y'] [x1 y1]]
      (if (and (= x' x2) (= y' y2))
        points
        (let [p' (add-coords [x' y'] dir)] (recur (conj points p') p'))))))

(defn part1 [input] (->> input
                         (str/split-lines)
                         (map str->line)
                         (filter is-horizontal-or-vertical?)
                         (map get-all-points)
                         (apply concat)
                         (frequencies)
                         (filter (comp #(< 1 %) val))
                         (count)))

(defn part2 [input] (->> input
                         (str/split-lines)
                         (map str->line)
                         (map get-all-points)
                         (apply concat)
                         (frequencies)
                         (filter (comp #(< 1 %) val))
                         (count)))

(comment
  (+ 1 1)

  (list 10)

  (map (partial * 3) '(1 -1))

  (map apply (list + *) (list (list 1 2) (list 30 40)))

  (concat (inclusive-range 0 5) (inclusive-range 3 3) (inclusive-range 9 11))

  (get-all-points [0 1 1 2])

  (add-coords)

  (inclusive-range 7 3)

  (for [x (inclusive-range 1 3)
        y (inclusive-range 0 0)]
    (list x y))

  (apply range (sort '(9 7)))

  (def test-input (slurp "input/day05-test.txt"))

  (def test-line (first (str/split-lines test-input)))

  (re-seq #"\d+" test-line)

  (get-direction [1 1 0 0])

  (util/parse-int "3")

  (map util/parse-int (re-seq #"\d+" test-line))

  (part2 test-input)

  (part2 (slurp "input/day05-input.txt")))

