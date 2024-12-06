(ns day-5)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def input (slurp "data/day_5.txt"))

(defn parse-order-rule [order-rule]
  (let [parts (str/split order-rule #"\|")
        [x y] (map #(Integer/parseInt %) parts)]
    {:x x :y y}))

(defn parse-update [update]
  (let [parts (str/split update #",")
        update (map #(Integer/parseInt %) parts)]
    update))

(defn create-order-map [order-rules]
  (loop [order-map {}
         rules order-rules]
    (if (empty? rules)
      order-map
      (let [rule (first rules)
            x (:x rule)
            y (:y rule)
            order-map (if (contains? order-map x)
                        order-map
                        (assoc order-map x {:before #{} :after #{}}))
            old-before (:before (get order-map x))
            order-map (assoc order-map x (assoc (get order-map x)
                                                :before (conj old-before y)))
            order-map (if (contains? order-map y)
                        order-map
                        (assoc order-map y {:before #{} :after #{}}))
            old-after (:after (get order-map y))
            order-map (assoc order-map y (assoc (get order-map y)
                                                :after (conj old-after x)))]
        (recur order-map (rest rules))))))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        order-rules (->> lines
                         (filter #(str/includes? % "|"))
                         (map parse-order-rule))
        updates (->> lines
                     (filter #(str/includes? % ","))
                     (map parse-update))]
    {:order-map (create-order-map order-rules)
     :updates updates}))

(defn test-page [order-map update n]
  (let [page (nth update n)
        rules (get order-map page)
        before (set (take n update))
        after (set (drop (inc n) update))
        invalid-before (set/intersection before (:before rules))
        invalid-after (set/intersection after (:after rules))]
    (and (= (count invalid-before) 0)
         (= (count invalid-after) 0))))

(def data (parse-input test-input))
(println (:updates data))
(test-page (:order-map data) (nth (:updates data) 0) 1)
(defn test-update [order-map update]
  (every? #(test-page order-map update %) (range (count update))))

(test-update (:order-map data) (nth (:updates data) 0))
(test-update (:order-map data) (nth (:updates data) 1))
(test-update (:order-map data) (nth (:updates data) 2))
(test-update (:order-map data) (nth (:updates data) 3))
(test-update (:order-map data) (nth (:updates data) 4))
(test-update (:order-map data) (nth (:updates data) 5))

(defn get-middle-number [xs]
  (let [mid (quot (count xs) 2)]
    (nth xs mid)))

(get-middle-number [1 2 3 4 5])
(get-middle-number [1 2 3 4 5 6])

(defn problem-1 [input]
  (let [data (parse-input input)
        updates (:updates data)
        order-map (:order-map data)
        valid-updates (filter #(test-update order-map %) updates)
        solution (reduce + (map #(get-middle-number %) valid-updates))]
    solution))

(problem-1 test-input)

(defn pick-page
  ([order-map update] (pick-page order-map update 0))
  ([order-map update n]
   (if (= n (count update))
     (do (println "Got to the end without finding a valid page")
         nil)

     (let [page (nth update n)
           rest (concat (take n update) (drop (inc n) update))
           after (:after (get order-map page))]
       (if (empty? (set/intersection after (set rest)))
         n
         (pick-page order-map update (inc n)))))))

(pick-page (:order-map data) (nth (:updates data) 3))

(defn reorder-update [update n]
  (let [page (nth update n)
        rest (concat (take n update) (drop (inc n) update))]
    (concat [page] rest)))

(defn fix-page
  ([order-map update] (fix-page order-map update []))
  ([order-map update prev]
   (if (empty? update)
     prev
     (let [n (pick-page order-map update)
           next-page (nth update n)
           new-update (concat (take n update) (drop (inc n) update))]
       (recur order-map new-update (conj prev next-page))))))

(fix-page (:order-map data) (nth (:updates data) 4))

(defn problem-2 [input]
  (let [data (parse-input input)
        updates (:updates data)
        order-map (:order-map data)
        invalid-updates (filter #(not (test-update order-map %)) updates)
        _ (println "Invalid updates" invalid-updates)
        fixed-updates (map #(fix-page order-map %) invalid-updates)
        _ (println "Fixed Updates" fixed-updates)
        solution (reduce + (map #(get-middle-number %) fixed-updates))]
    solution))

(println (problem-2 input))


