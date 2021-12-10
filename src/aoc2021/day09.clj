(ns aoc2021.day09 (:require [aoc2021.util :as util] [clojure.string :as str] [clojure.set :as set]))

(defn parse-line [line]
  (->> line
       (seq)
       (map str)
       (map util/parse-int)))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)
       (to-array-2d)))

(def != (comp not =))

(defn xor [a b] (and (or a b) (not (and a b))))

(defn get-adjacent [a i j]
  (let [M (alength a)
        N (alength (aget a 0))
        imin (max 0 (dec i))
        imax (min M (+ i 2))
        jmin (max 0 (dec j))
        jmax (min N (+ j 2))]
    (for [i' (range imin imax)
          j' (range jmin jmax)
          :when (xor (= i i') (= j j'))]
      (list i' j'))))


(defn value-at [a [i j]]
  (aget a i j))

(defn get-elements-around [a i j]
  (map (partial value-at a) (get-adjacent a i j)))

(defn is-minimum-at [a i j]
  (let [xs (get-elements-around a i j)
        xmin (apply min xs)
        y (aget a i j)]
    (< y xmin)))

(defn get-minima [a]
  (let [M (alength a)
        N (alength (aget a 0))]
    (for [i (range 0 M)
          j (range 0 N)
          :when (is-minimum-at a i j)]
      (list i j))))

(defn all-points [a]
  (let [M (alength a)
        N (alength (aget a 0))]
    (for [i (range 0 M)
          j (range 0 N)]
      (list i j))))


(defn get-minima-values [a]
  (map (partial value-at a) (get-minima a)))

(defn get-risk-levels [a]
  (map (partial + 1) (get-minima-values a)))

(defn part1 [input]
  (->> input
       (parse-input)
       (get-risk-levels)
       (apply +)))

(defn basin
  ([a pos] (basin a pos #{}))
  ([a [i j] sofar]
   (let [adj (get-adjacent a i j)
         adj' (set/difference (set adj) sofar)
         newpos (filter #(!= 9 (value-at a %)) adj')
         visited (set/union sofar (set newpos))]
     (reduce #(basin a %2 %1) visited newpos))))

(defn get-all-basins [a]
  (distinct (map #(basin a %) (filter #(!= 9 (value-at a %)) (all-points a)))))

(defn part2 [input]
  (->> input
       (parse-input)
       (get-all-basins)
       (map count)
       (sort)
       (take-last 3)
       (apply *)))

(comment 
  (def test-input (slurp "input/day09-test.txt"))
  (def a (parse-input test-input))
  
  (part1 test-input)
  
  (part1 (slurp "input/day09-input.txt"))
  
  (part2 test-input)
  
  (part2 (slurp "input/day09-input.txt"))
  
  (basin a [0 0])
  
  )