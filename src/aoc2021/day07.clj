(ns aoc2021.day07)

(defn median [a]
  (nth (sort a) (int (/ (count a) 2))))

(defn abs [n] (Math/abs n))

(defn T [n] = (/ (* n (inc n)) 2))

(defn fuel [t mu] (apply + (map #(abs (- % mu)) t)))

;; Alternative Berchnung für Teil 2
(defn fuel2 [t mu] (apply + (map #(T (abs (- % mu))) t)))

(defn min-fuel [t] (fuel t (median t)))

(defn parse-input [input] (read-string (str "[" input "]")))

(defn part1 [input]
  (min-fuel (parse-input input)))

;; In Part 2 wird ge-brute-forced. Das sollte flott genug sein
(defn min-fuel2 [t]
  (first (sort-by second (map #(list % (fuel2 t %)) (range (apply min t) (apply max t))))))

(defn part2 [input] (min-fuel2 (parse-input input)))

(comment
  (/ 5 2)

  (def test-input (slurp "input/day07-test.txt"))

  (def t (parse-input test-input))
  
  
  
  (fuel2 t 5)

  (part1 (slurp "input/day07-input.txt"))
  (part2 (slurp "input/day07-input.txt"))

  (def t '(16,1,4,0,4,2,7,1,2,14))

  (median t)

  (fuel t 4)

  


  (int (/ 1 2))
  )