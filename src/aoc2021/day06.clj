(ns aoc2021.day06)

(defn parse-input [input] (read-string (str "[" input "]")))

;; Versuch 1: wir bilden die Fische als Map ab, die jeden Fisch einzeln als Zahl betrachtet
(defn age-and-spawn [age]
  (if (= age 0)
    [6 8]
    [(dec age)]))

(defn do-step [fishlist]
  (mapcat age-and-spawn fishlist))

(defn do-steps [n fishlist]
  (nth (iterate do-step fishlist) n))

(defn part1 [input]
  (->> input
       parse-input
       (do-steps 80)
       count))

;; Versuch 2: wir bauen uns eine Liste mit dem Alter der Fische als Index und der Anzahl als Wert,
;; denn die Verhalten sich alle gleich. So müssen wir nur noch max. 9 Fälle betrachten.
;; Sonst wird mir das zu langsam
(def zero-map (zipmap (range 9) (repeat 0)))

(defn to-indexed [fishlist]
  (->> fishlist
       (frequencies)
       (seq)
       (merge zero-map)
       (sort-by key)
       (map second)
       (vec)))

(defn indexed-step [[x & rest]]
  (assoc (vec rest) 8 x 6 (+ (nth rest 6) x)))

(defn indexed-do-steps [n fishlist]
  (nth (iterate indexed-step fishlist) n))

(defn fish-after-days [n input]
  (->> input
       parse-input
       to-indexed
       (indexed-do-steps n)
       (apply +)))

(def indexed-part1 (partial fish-after-days 80))

(def indexed-part2 (partial fish-after-days 256))


(comment
  (def test-input (slurp "input/day06-test.txt"))

  (indexed-part1 test-input)

  (indexed-part2 test-input)

  (->> test-input
       parse-input
       to-indexed
       (indexed-step)
       (apply +))

  (part1 (slurp "input/day06-input.txt"))

  (indexed-part1 (slurp "input/day06-input.txt"))

  (indexed-part2 (slurp "input/day06-input.txt"))



  (->> (parse-input test-input)
       to-indexed
       (def fi))

  (def fi (let [[x & rest] fi] (assoc (vec rest) 8 x 6 (+ (nth rest 6) x))))


  (assoc (rest fi) 8 (first fi)))