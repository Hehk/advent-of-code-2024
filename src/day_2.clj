(ns day-2)

(def test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
(def input (slurp "data/day_2.txt"))

(defn parse-report [raw]
  (let [split (clojure.string/split raw #"\s+")]
    (map #(Integer/parseInt %) split)))

(parse-report "7 6 4 2 1")
(parse-report "1 2 7 8 9")
(parse-report "9 7 6 2 1")
(parse-report "1 3 2 4 5")
(parse-report "8 6 4 4 1")

(defn parse-reports [raw]
  (let [split (clojure.string/split raw #"\n")
        parsed (map parse-report split)]
    parsed))

(defn slope-direction [level-1 level-2]
  (if (< level-1 level-2)
    :lower
    (if (> level-1 level-2)
      :higher
      :equal)))

(defn is-valid-report? [report]
  (let [windows (partition 2 1 report)
        levels (map #(slope-direction (first %) (second %)) windows)
        change (map #(Math/abs (- (first %) (second %))) windows)]
    (and
     (every? #(= % (first levels)) levels)
     (every? #(and (>= % 1) (<= % 3)) change))))

(is-valid-report? (parse-report "7 6 4 2 1"))
(is-valid-report? (parse-report "1 2 7 8 9"))
(is-valid-report? (parse-report "9 7 6 2 1"))
(defn problem-1 [input]
  (->> input
       parse-reports
       (filter is-valid-report?)
       count))

(problem-1 test-input)
(problem-1 input)

(defn indexes [f coll]
  (keep-indexed #(when (f %2) %1) coll))
(defn drop-index [coll n]
  (concat (take n coll) (drop (inc n) coll)))

(defn create-fixed-report [report n]
  (drop-index report n))

; This is ugly, but it works...
(defn get-aggregate-direction [directions]
  (let [lower-counts (count (filter #(= % :lower) directions))
        higher-counts (count (filter #(= % :higher) directions))]
    (if (> lower-counts higher-counts)
      :lower
      :higher)))

(defn is-fixable-report? [report]
  (let [windows (partition 2 1 report)
        directions (map #(slope-direction (first %) (second %)) windows)
        changes (map #(Math/abs (- (first %) (second %))) windows)
        agg-direction (get-aggregate-direction directions)
        invalid-levels (indexes #(not= % agg-direction) directions)
        invalid-changes (indexes #(not (and (>= % 1) (<= % 3))) changes)
        patched-reports (concat
                         (list report)
                         (map #(create-fixed-report report (inc %)) invalid-levels)
                         (map #(create-fixed-report report %) invalid-levels)
                         (map #(create-fixed-report report (inc %)) invalid-changes)
                         (map #(create-fixed-report report %) invalid-changes))]
    (if (some is-valid-report? patched-reports)
      true
      (do (println "Invalid :" report invalid-levels invalid-changes)
          false))))

(parse-report "7 6 4 2 1")
(is-fixable-report? (parse-report "7 6 4 2 1"))
(defn problem-2 [input]
  (->> input
       parse-reports
       (filter is-fixable-report?)
       count))

(def reports (parse-reports test-input))
(is-fixable-report? (nth  reports 0))
(is-fixable-report? (nth  reports 1))
(is-fixable-report? (nth reports 2))
(is-fixable-report? (nth  reports 3))
(is-fixable-report? (nth  reports 4))
(is-fixable-report? '(2 1 2 3 4 5 6 7 8 9))

(problem-2 test-input)
(problem-2 input)
