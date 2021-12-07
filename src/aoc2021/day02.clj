(ns aoc2021.day02)

(defn parse-int [s] (Integer/parseInt s))

(defn command-to-coords [[a b]]
  (case a
    "forward" (list b 0)
    "down"    (list 0 b)
    "up"      (list 0 (- b))
    '(0 0)))

(defn part1 [input]
  (->> input
       (re-seq #"\w+")
       (partition 2)
       (map (fn [[a b]] (list a (parse-int b))))
       (map command-to-coords)
       (apply map +)
       (apply *)))

(defn woosh [[x y aim] [dx daim]]
  (list (+ x dx) (+ y (* dx aim)) (+ aim daim)))

(defn part2 [input]
  (->> input
       (re-seq #"\w+")
       (partition 2)
       (map (fn [[a b]] (list a (parse-int b))))
       (map command-to-coords)
       (reduce woosh '(0 0 0))
       (take 2)
       (apply *)))

(comment

  (part1 test-input)
  (def test-input (slurp "input/day02-test.txt"))

  (part1 (slurp "input/day02-input.txt"))
  (part2 (slurp "input/day02-input.txt"))

  (command-to-coords '("up" 3))

  (partition 2 (re-seq #"\w+" test-input))
  
  )