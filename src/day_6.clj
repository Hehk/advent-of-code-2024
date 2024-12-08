(ns day-6)

(require '[clojure.string :as str])

(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def input (slurp "data/day_6.txt"))

(defn parse-node [node]
  (cond
    (= node \#) {:type :wall}
    (= node \.) {:type :floor :visited false}
    (= node \^) {:type :guard :direction :up}
    (= node \>) {:type :guard :direction :right}
    (= node \v) {:type :guard :direction :down}
    (= node \<) {:type :guard :direction :left}
    :else nil))

(defn show-node [node]
  (let [type (:type node)]
    (cond
      (= type :wall) \#
      (and (= type :floor) (:visited node)) \X
      (and (= type :floor) (not (:visited node))) \.
      (and (= type :guard) (= :up (:direction node))) \^
      (and (= type :guard) (= :right (:direction node))) \>
      (and (= type :guard) (= :down (:direction node))) \v
      (and (= type :guard) (= :left (:direction node))) \<
      :else nil)))

(defn parse-line [line]
  (vec (map parse-node line)))

(defn show-line [line]
  (apply str (map show-node line)))

(show-line (parse-line "..."))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        lines (map parse-line lines)]
    (vec lines)))

(defn print-grid [grid]
  (doseq [line grid]
    (println (show-line line))))

(defn find-guard [grid]
  (first (filter some? (for [x (range (count grid))
                             y (range (count (first grid)))]
                         (let [node (nth (nth grid y) x)]
                           (when (= (:type node) :guard)
                             {:x x :y y}))))))
(defn get-node [{:keys [x y]} grid]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (cond
      (nil? x) nil
      (and (< x max-x) (< y max-y)
           (>= x 0) (>= y 0))
      (nth (nth grid y) x)
      :else nil)))

(defn rotate-dir [dir]
  (cond (= dir :up) :right
        (= dir :right) :down
        (= dir :down) :left
        (= dir :left) :up))

(defn move-guard [{:keys [x y]} dir grid]
  (let [new-x (cond (= dir :left) (dec x)
                    (= dir :right) (inc x)
                    :else x)
        new-y (cond (= dir :up) (dec y)
                    (= dir :down) (inc y)
                    :else y)
        next-node (get-node {:x new-x :y new-y} grid)]
    (if (= (:type next-node) :wall)
      {:pos {:x x :y y}
       :guard (assoc (get-node {:x x :y y} grid) :direction (rotate-dir dir))}
      {:pos {:x new-x :y new-y}
       :guard (get-node {:x x :y y} grid)})))

(defn is-in-grid? [{:keys [x y]} grid]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (and (< x max-x) (< y max-y)
         (>= x 0) (>= y 0))))

(defn update-grid [{:keys [x y]} node grid]
  (if (is-in-grid? {:x x :y y} grid)
    (assoc grid y (assoc (nth grid y) x node))
    grid))

(defn run-step [{:keys [grid guard-pos]}]
  (let [guard (get-node guard-pos grid)
        direction (:direction guard)
        {:keys [pos guard]} (move-guard guard-pos direction grid)]
    (if (= guard-pos pos)
      {:grid (update-grid pos guard grid)
       :guard-pos guard-pos}
      {:grid (->> grid
                  (update-grid pos guard)
                  (update-grid guard-pos {:type :floor :visited true}))
       :guard-pos pos})))

(defn run [grid]
  (loop [step {:grid grid :guard-pos (find-guard grid)}]
    (let [result (run-step step)]
      (if (is-in-grid? (:guard-pos result) (:grid result))
        (recur result)
        (:grid result)))))

(defn count-visited-nodes [grid]
  (reduce + (map (fn [line]
                   (reduce + (map (fn [node]
                                    (if (:visited node)
                                      1
                                      0))
                                  line)))
                 grid)))

; (defn problem-1 [input]
;   (->> input
;        (parse-input)
;        (run)
;        (count-visited-nodes)))
;
; (problem-1 test-input)
; (println "Problem 1:" (problem-1 input))

; (run-steps test-input)

(defn add-guard-visit [{:keys [x y]} grid history]
  (let [y-history (get history x)
        old (get y-history y #{})
        new (conj old (:direction (get-node {:x x :y y} grid)))]
    (assoc history x (assoc y-history y new))))

(defn is-in-history? [{:keys [x y]} dir history]
  (let [old (get (get history x) y #{})]
    (contains? old dir)))

(defn run-with-loop-detection [grid]
  (if (nil? (find-guard grid))
    {:grid grid :loop false}
    (loop [step {:grid grid :guard-pos (find-guard grid)}
           guard-history {}]
      (let [guard-history (add-guard-visit (:guard-pos step) (:grid step) guard-history)
            {:keys [grid guard-pos]} (run-step step)
            dir (:direction (get-node guard-pos grid))]
        (cond
          (is-in-history? guard-pos dir guard-history) {:grid grid :loop true}
          (is-in-grid? guard-pos grid) (recur {:grid grid :guard-pos guard-pos} guard-history)
          :else {:grid grid :loop false})))))

(defn get-visited-nodes [grid]
  (filter some? (for [x (range (count grid))
                      y (range (count (first grid)))]
                  (let [node (nth (nth grid y) x)]
                    (when (:visited node)
                      {:x x :y y})))))

(defn problem-2 [input]
  (let [grid (parse-input input)
        result (run grid)
        visited-nodes (get-visited-nodes result)
        new-grids (map #(update-grid % {:type :wall} grid) visited-nodes)
        run-grids (map run-with-loop-detection new-grids)
        loop-grids (filter #(get % :loop) run-grids)]

    (println (count loop-grids))))

(println "Problem 2" (problem-2 input))
