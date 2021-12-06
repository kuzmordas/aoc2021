(ns aoc-clojure.day1)

(defn part-one [path-to-file]
  (let [input (slurp path-to-file)
        list-of-string (clojure.string/split input #"\n")
        numbers        (mapv #(Integer/parseInt %) list-of-string)]
    (reduce (fn [acc curr]
              (if (> curr (:prev acc) )
                {:sum (-> acc :sum inc) :prev curr}
                {:sum (-> acc :sum) :prev curr}))
            {:sum 0 :prev (first numbers)}
            numbers)))

(defn part-two [path-to-file]
  (let [input           (slurp path-to-file)
        list-of-string  (clojure.string/split input #"\n")
        numbers         (mapv #(Integer/parseInt %) list-of-string)
        sums (mapv #(apply + %) (partition 3 1 numbers))]
    (reduce (fn [acc curr]
              (if (> curr (:prev acc) )
                {:sum (-> acc :sum inc) :prev curr}
                {:sum (-> acc :sum) :prev curr}))
            {:sum 0 :prev (first sums)}
            sums)))

(:sum (part-one "./resources/day1.txt"))
(:sum (part-two "./resources/day1.txt"))

