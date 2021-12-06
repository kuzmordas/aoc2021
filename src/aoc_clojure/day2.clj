(ns aoc-clojure.day2)

(defn to-command [s]
  (let [[cmd val] (clojure.string/split s #" ")]
    {:cmd cmd :val (Integer/parseInt val)}))

(defn part-one [path-to-file]
  (let [input            (slurp path-to-file)
        list-of-commands (clojure.string/split input #"\n")
        commands         (mapv to-command list-of-commands)]
    (reduce (fn [acc curr]
              (case (:cmd curr)
                "down" (update acc :depth + (:val curr))
                "up"   (update acc :depth - (:val curr))
                       (update acc :horizontal + (:val curr))))
            {:horizontal 0 :depth 0}
            commands)))

(let [result (part-one "./resources/day2.txt")]
  (* (:horizontal result)
     (:depth result)))

(defn part-two [path-to-file]
  (let [input            (slurp path-to-file)
        list-of-commands (clojure.string/split input #"\n")
        commands         (mapv to-command list-of-commands)]
    (reduce (fn [acc curr]
              (case (:cmd curr)
                "down" (update acc :aim + (:val curr))
                "up"   (update acc :aim - (:val curr))
                { :horizontal (+ (:horizontal acc) (:val curr) )
                  :depth (+ (:depth acc) (* (:aim acc) (:val curr)))
                  :aim (:aim acc) }))
            {:horizontal 0 :depth 0 :aim 0}
            commands)))

(let [result (part-two "./resources/day2.txt")]
  (* (:horizontal result)
     (:depth result)))



