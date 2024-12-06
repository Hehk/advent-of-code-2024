(ns day-4)

(require '[clojure.string :as str])

(def test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn parse-input [input]
  (vec (map vec (str/split-lines input))))

(defn get-char [input x y]
  (nth (nth input y) x))

(get-char (parse-input test-input) 0 0)
(get-char (parse-input test-input) 1 1)
(get-char (parse-input test-input) 2 2)
(get-char (parse-input test-input) 3 3)
(get-char (parse-input test-input) 4 4)

(defn get-neighbor-locs [input x y]
  (let [max-x (count (first input))
        max-y (count input)
        neighbors (for [dx [-1 0 1] dy [-1 0 1]]
                    (let [nx (+ x dx)
                          ny (+ y dy)]
                      (when (and (not (and (= dx 0) (= dy 0)))
                                 (>= nx 0) (< nx max-x)
                                 (>= ny 0) (< ny max-y))
                        [nx ny])))]
    (filter some? neighbors)))

(get-neighbor-locs (parse-input test-input) 0 0)
(get-neighbor-locs (parse-input test-input) 0 0)
(get-neighbor-locs (parse-input test-input) 9 9)

(defn get-nx [dir]
  (cond (= dir :up-left) -1
        (= dir :up) 0
        (= dir :up-right) 1
        (= dir :left) -1
        (= dir :right) 1
        (= dir :down-left) -1
        (= dir :down) 0
        (= dir :down-right) 1))

(defn get-ny [dir]
  (cond (= dir :up-left) -1
        (= dir :up) -1
        (= dir :up-right) -1
        (= dir :left) 0
        (= dir :right) 0
        (= dir :down-left) 1
        (= dir :down) 1
        (= dir :down-right) 1))

(defn apply-direction [input {:keys [x y dir]}]
  (let [max-x (count (first input))
        max-y (count input)
        nx (get-nx dir)
        ny (get-ny dir)
        x (+ x nx)
        y (+ y ny)]
    (cond
      (< x 0) nil
      (>= x max-x) nil
      (< y 0) nil
      (>= y max-y) nil
      :else {:x x :y y :dir dir})))

(defn get-next-point [input char {:keys [x y dir]}]
  (if (= (get-char input x y) char)
    (apply-direction input {:x x :y y :dir dir})
    nil))

(get-next-point (parse-input test-input) \X {:x 4 :y 0 :dir :down})

(defn get-problem-start-points-1 [input]
  (let [max-x (count (first input))
        max-y (count input)
        dirs #{:up-left :up :up-right :left :right :down-left :down :down-right}]
    (for [x (range max-x)
          y (range max-y)
          dir dirs]
      {:x x
       :y y
       :dir dir})))

(count (get-problem-start-points-1 (parse-input test-input)))

(defn get-next-points [input char points]
  (let [points (map #(get-next-point input char %) points)]
    (filter some? points)))

(defn problem-1 [input]
  (let [points (get-problem-start-points-1 input)
        points (get-next-points input \X points)
        points (get-next-points input \M points)
        points (get-next-points input \A points)]
    (count (filter #(= (get-char input (:x %) (:y %)) \S) points))))

(println "Problem 1 test input:" (problem-1 (parse-input test-input)))
(println "Problem 1:" (problem-1 (parse-input (slurp "data/day_4.txt"))))

(defn get-edge-char-counts [input {:keys [x y]}]
  (let [get-char (fn [dir]
                   (let [point (apply-direction input {:x x :y y :dir dir})]
                     (println "point:" point)
                     (if point
                       (get-char input (:x point) (:y point))
                       nil)))
        up-left (get-char :up-left)
        up-right (get-char :up-right)
        down-left (get-char :down-left)
        down-right (get-char :down-right)
        points [up-left up-right down-left down-right]]
    {:x-count (count (filter #(= \M %) points))
     :s-count (count (filter #(= \S %) points))
     :up-left up-left
     :up-right up-right
     :down-left down-left
     :down-right down-right
     :x x
     :y y}))

(defn problem-2 [input]
  (let [points (for [x (range (count (first input)))
                     y (range (count input))]
                 {:x x
                  :y y})
        points (filter #(= (get-char input (:x %) (:y %)) \A) points)
        char-counts (map #(get-edge-char-counts input %) points)
        solutions (filter #(and (= (:x-count %) 2)
                                (= (:s-count %) 2))
                          char-counts)]
    (println "solutions:" (take 10 solutions))
    (count solutions)))

(println "Problem 2 test input:" (problem-2 (parse-input test-input)))
(println "Problem 2" (problem-2 (parse-input (slurp "data/day_4_new.txt"))))

(def input (parse-input (slurp "data/day_4_new.txt")))
(println (drop 20 (nth input 2)))
(println (drop 20 (nth input 1)))
(println (drop 20 (nth input 3)))

(defn print-point [{:keys [x y]}]
  (let [input (parse-input (slurp "data/day_4_new.txt"))
        grid (for [dx [-1 0 1]
                   dy [-1 0 1]]
               (get-char input (+ x dx) (+ y dy)))
        windows (partition 3 grid)]
    (for [row windows]
      (println row))))

(defn get-sub-grid [input x y]
  (let [grid (for [dy [-1 0 1]
                   dx [-1 0 1]]
               (get-char input (+ x dx) (+ y dy)))
        windows (partition 3 grid)]
    windows))

(defn get-grid-pos [grid]
  (let [center (nth (nth grid 1) 1)
        top-left (nth (nth grid 0) 0)
        top-right (nth (nth grid 0) 2)
        bottom-left (nth (nth grid 2) 0)
        bottom-right (nth (nth grid 2) 2)]
    {:center center
     :top-left top-left
     :top-right top-right
     :bottom-left bottom-left
     :bottom-right bottom-right}))

(defn is-valid-grid? [grid]
  (cond
    (not (= (:center grid) \A)) false

    (and (= (:top-left grid) \M)
         (= (:top-right grid) \M)
         (= (:bottom-left grid) \S)
         (= (:bottom-right grid) \S)) true

    (and (= (:top-left grid) \M)
         (= (:top-right grid) \S)
         (= (:bottom-left grid) \M)
         (= (:bottom-right grid) \S)) true

    (and (= (:top-left grid) \S)
         (= (:top-right grid) \S)
         (= (:bottom-left grid) \M)
         (= (:bottom-right grid) \M)) true

    (and (= (:top-left grid) \S)
         (= (:top-right grid) \M)
         (= (:bottom-left grid) \S)
         (= (:bottom-right grid) \M)) true

    :else false))

(println (get-grid-pos (get-sub-grid input 1 2)))
(println (is-valid-grid? (get-grid-pos (get-sub-grid input 1 2))))

(defn get-grids [input]
  (let [max-x (count (first input))
        max-y (count input)
        grids (for [x (range 1 (- max-x  1))
                    y (range 1 (- max-y 1))]
                (let [grid (get-grid-pos (get-sub-grid input x y))]
                  (when (is-valid-grid? grid)
                    {:x x
                     :y y
                     :grid grid})))]
    (filter some? grids)))

(println (get-grids input))
(println (count (get-grids input)))
