(ns aoc-clojure.day3)

(defn get-matrix [path-to-file]
  (let [input (slurp path-to-file)
        rows  (mapv #(clojure.string/split % #"")
                    (clojure.string/split input #"\n"))]
    (mapv (fn [r] (mapv #(Integer/parseInt %) r)) rows)))

(defn gamma-rate [trp-mtx]
  (let [r (map (fn [x]
                 (let [sum (apply + x)]
                   (if (> sum (/ (count x) 2))
                     1
                     0))) trp-mtx)]
    (Integer/parseInt (clojure.string/join "" r) 2)))

(defn epsilon-rate [trp-mtx]
  (let [r (map (fn [x]
                 (let [sum (apply + x)]
                   (if-not (> sum (/ (count x) 2))
                     1
                     0))) trp-mtx)]
    (Integer/parseInt (clojure.string/join "" r) 2)))

(defn part-one [path-to-file]
  (let [mtx     (get-matrix path-to-file)
        trp     (apply mapv vector mtx)
        gamma   (gamma-rate trp)
        epsilon (epsilon-rate trp)]
    (* gamma epsilon)))

(defn calc-bit [mtx i]
  (let [lst (map #(get % i) mtx)
        sum (apply + lst)
        len (count lst)]
    (cond
      (= (- len sum) sum) -1
      (> sum (- len sum)) 1
      :else 0)))

(defn gamma-rate2 [mtx]
  (loop [m mtx
         i 0]
    (let [bit (calc-bit m i)
          filter-fn (if (or (= bit 1) (= bit -1))
                      #(= (get % i) 1)
                      #(= (get % i) 0))
          next (filter filter-fn m)]
      (if (= (count next) 1)
        (Integer/parseInt (clojure.string/join "" (first next)) 2)
        (recur next (inc i))))))

(defn epsilon-rate2 [mtx]
  (loop [m mtx
         i 0]
    (let [bit (calc-bit m i)
          filter-fn (if (or (= bit 1) (= bit -1))
                      #(= (get % i) 0)
                      #(= (get % i) 1))
          next (filter filter-fn m)]
      (if (= (count next) 1)
        (Integer/parseInt (clojure.string/join "" (first next)) 2)
        (recur next (inc i))))))


(defn part-two [path-to-file]
  (let [mtx     (get-matrix path-to-file)
        gamma   (gamma-rate2 mtx)
        epsilon (epsilon-rate2 mtx)]
    (* gamma epsilon)))


(part-one "./resources/day3.txt")
(part-two "./resources/day3.txt")

