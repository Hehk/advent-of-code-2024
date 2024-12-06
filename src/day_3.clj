(ns day-3)

(require '[clojure.string :as str])

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn take-number [input]
  (let [pattern #"^\d\d?\d?"
        num (re-find pattern input)]
    (if num
      {:value (Integer/parseInt num)
       :rest (str/replace input pattern "")}
      nil)))

(= 2 (:value (take-number "2")))
(= 222 (:value (take-number "222")))
(= 222 (:value (take-number "2222")))
(= 222 (:value (take-number "2224")))
(= 422 (:value (take-number "4222")))

(defn take-s [s input]
  (if (str/starts-with? input s)
    {:value s
     :rest (str/replace-first input s "")}
    nil))

(defn run-parser [input {:keys [parser acc] :or {acc [] parser []}}]
  (cond
    (and (empty? parser) (empty? acc)) nil
    (empty? parser) {:value acc
                     :rest input}
    :else
    (let [part (first parser)
          result (part input)]
      (if result
        (recur (:rest result) {:parser (rest parser) :acc (conj acc (:value result))})
        nil))))

(= [2] (:value (run-parser "2" {:parser [take-number]})))
(= ["(", 20, ")"] (:value (run-parser "(20)" {:parser [#(take-s "(" %)
                                                       take-number
                                                       #(take-s ")" %)]})))

(defn parse-mul [input]
  (let [parser [#(take-s "mul" %)
                #(take-s "(" %)
                take-number
                #(take-s "," %)
                take-number
                #(take-s ")" %)]
        result (run-parser input {:parser parser})]
    (if (nil? result)
      nil
      (let [a (nth (:value result) 2)
            b (nth (:value result) 4)]
        {:value (* a b)
         :rest (:rest result)}))))

(parse-mul "mul(2,3)")

(defn problem-1 [input acc]
  (if (empty? input)
    acc
    (let [result (parse-mul input)
          rest (if (nil? result) (subs input 1) (:rest result))
          new-acc (if (nil? result) acc (+ acc (:value result)))]
      (recur rest new-acc))))

(println "Problem 1 test:" (problem-1 test-input 0))
(println "Problem 1:" (problem-1 (slurp "data/day_3.txt") 0))

(defn problem-2 [input & {:keys [acc active] :or {acc 0 active true}}]
  (if (empty? input)
    acc
    (let [result (parse-mul input)
          result-dont (take-s "don't()" input)
          result-do (take-s "do()" input)]
      (cond
        (not (nil? result-dont)) (recur (:rest result-dont) {:acc acc :active false})
        (not (nil? result-do)) (recur (:rest result-do) {:acc acc :active true})
        (and (not (nil? result)) active)
        (recur (:rest result) {:acc (+ (:value result) acc) :active active})
        :else (recur (subs input 1) {:acc acc :active active})))))

(def test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(println "Problem 2 test:" (problem-2 test-input-2))
(println "Problem 2:" (problem-2 (slurp "data/day_3.txt")))

