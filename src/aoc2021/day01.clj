(ns aoc2021.day01)

(defn parse-int [s] (Integer/parseInt s))

(defn count-increments [t] (count (filter true? (map > (rest t) t))))

(defn read-input [f] (map parse-int (clojure.string/split-lines (slurp f))))

(defn part1 [] (count-increments (read-input "input/day01-input.txt")))

(defn sum3 [a] (map + a (drop 1 a) (drop 2 a)))

(defn part2 [] (count-increments (sum3 (read-input "input/day01-input.txt"))))

(comment

  (part1)
  
  (part2)

  (def t '(1 15 3 3 0 1 2 3 2))

  (map parse-int '("10" "20"))
  
  (->> (read-input "input/day01-test.txt")
       (sum3)
       (count-increments))
  
  )