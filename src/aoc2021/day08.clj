(ns aoc2021.day08 (:require [clojure.string :as str] [clojure.set :as set]))


(defn parse-line [line]
  (->> line
       (re-seq #"[a-g]+")
       (map set)
       ((fn [sets] (hash-map :signals (take 10 sets) :outputs (drop 10 sets))))))

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn filter-1-7-4-or-8 [outputs] (filter (comp #{2 3 4 7} count) outputs))

(defn get-signal-with-size [signals n]
  (first (filter #(= n (count %)) signals)))

(defn part1 [input]
  (->> input
       (parse-input)
       (map :outputs)
       (mapcat filter-1-7-4-or-8)
       (count)))

(def != (comp not =))

;; die 9 ist das signal mit 6 elementen, das alle elemente von signal 4 enthält
(defn find-signal-9 [signals signal-4]
  (first (filter #(set/subset? signal-4 %) (filter #(= 6 (count %)) signals))))

;; die 0 hat 6 eles, ist nicht die 9 und hat alle eles von der 1
(defn find-signal-0 [signals signal-9 signal-1]
  (first (filter (every-pred #(set/subset? signal-1 %) #(= 6 (count %)) #(!= signal-9 %)) signals)))

(defn find-signal-6 [signals signal-9 signal-0]
  (first (filter (every-pred #(!= signal-9 %) #(!= signal-0 %) #(= 6 (count %))) signals)))

(defn find-signal-3 [signals signal-1]
  (first (filter (every-pred #(= 5 (count %)) #(set/subset? signal-1 %)) signals)))

(defn join-numbers [a]
  (apply + (map * (reverse a) (iterate #(* 10 %) 1))))

(defn find-signal-mapping [thingy]
  (->> thingy
       (:signals)
       ((fn [signals]
          (let [signal-1 (get-signal-with-size signals 2)
                signal-7 (get-signal-with-size signals 3)
                signal-4 (get-signal-with-size signals 4)
                signal-8 (get-signal-with-size signals 7)
                seg-a (first (set/difference signal-7 signal-1))
                signal-9 (find-signal-9 signals signal-4)
                seg-e (first (set/difference signal-8 signal-9))
                signal-0 (find-signal-0 signals signal-9 signal-1)
                seg-d (first (set/difference signal-8 signal-0))
                signal-6 (find-signal-6 signals signal-9 signal-0)
                seg-c (first (set/difference signal-8 signal-6))
                signal-3 (find-signal-3 signals signal-1)
                seg-b (first (set/difference signal-9 signal-3))
                seg-f (first (set/difference signal-4 #{seg-b seg-c seg-d}))
                seg-g (first (set/difference signal-9 signal-4 #{seg-a}))
                signal-2 #{seg-a seg-c seg-d seg-e seg-g}
                signal-5 #{seg-a seg-b seg-d seg-f seg-g}]
            (hash-map signal-0 0 signal-1 1 signal-2 2 signal-3 3 signal-4 4 signal-5 5
                      signal-6 6 signal-7 7 signal-8 8 signal-9 9))))))

(defn get-output-value [thingy]
  (let [m (find-signal-mapping thingy)]
    (join-numbers (map m (:outputs thingy)))))

(defn part2 [input]
  (->> input
       (parse-input)
       (map get-output-value)
       (apply +)))

(comment
  (def test-input (slurp "input/day08-test.txt"))
  
  

  (part1 (slurp "input/day08-input.txt"))
  
  (part2 (slurp "input/day08-input.txt"))

  (filter-1-7-4-or-8 '(2))

  (def testthing (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))
  
  )