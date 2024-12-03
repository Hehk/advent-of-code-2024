(ns day-1)

(def test-input "3   4
4   3
2   5
1   3
3   9
3   3")
(def input (slurp "data/day_1.txt"))

(defn parse-line [line]
  (let [parts (clojure.string/split line #"\s+")
        left (Integer/parseInt (first parts))
        right (Integer/parseInt (second parts))]
    {:left left :right right}))

(parse-line "3   4")
(parse-line "4   3")
(parse-line "2   5")
(parse-line "1   3")
(parse-line "3   9")
(parse-line "3   3")

(defn parse-lines [input]
  (let [lines (clojure.string/split input #"\n")
        parsed-lines (map parse-line lines)]
    {:left (map :left parsed-lines)
     :right (map :right parsed-lines)}))

(parse-lines test-input)
(defn calc-distance [lines]
  (let [left (sort (:left lines))
        right (sort (:right lines))]
    (reduce + (map #(Math/abs (- %1 %2)) left right))))

(defn problem-1 [input]
  (->> input
       parse-lines
       calc-distance))

(problem-1 input)

(defn calc-similarity [n ns]
  (let [occurences (filter #(= n %) ns)]
    (* n (count occurences))))
(= 9 (calc-similarity 3 [1 2 3 3 3]))
(= 2 (calc-similarity 2 [1 2 3 3 3]))
(= 0 (calc-similarity 4 [1 2 3 3 3]))

(defn calc-similarity-total [lines]
  (let [left (:left lines)
        right (:right lines)]
    (reduce + (map #(calc-similarity %1 right) left))))

(defn problem-2 [input]
  (->> input
       parse-lines
       calc-similarity-total))
(problem-2 test-input)
(problem-2 input)
