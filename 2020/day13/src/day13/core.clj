(ns day13.core
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:gen-class))


(defn read-input [name]
  (slurp name))

(defn parse-input [input]
  (let [[line1 line2] (str/split-lines input)
        start (read-string line1)
        ids (str/split line2 #",")]
    (->>
     ids
     (keep-indexed
      (fn [i id]
        (when (not (= "x" id)) [i (read-string id)])))
     (assoc { :start start } :ids))))

(defn calc-departures [ids t]
  (map #(->
         { :t t }
         (assoc :id %)
         (assoc :rem (mod t %))) ids))

(defn find-first-departure [ids t]
  (let [departs (calc-departures ids t)
        result (filter #(= 0 (:rem %)) departs)]
    (if (empty? result)
      (recur ids (inc t))
      (first result))))

(defn solve-a [input]
  (let [start (:start input)
        ids (map second (:ids input))
        result (find-first-departure ids start)]
    (* (:id result) (- (:t result) start))))

(defn leave-together [ids target-t [t id]]
  (let [dt (- target-t t)]
    (and (>= dt 0) ;take only smaller times
         (= 0 (mod dt id)))))

(defn fold-ids [ids state t]
  (let [done (:done state)
        agg (:agg state)
        matches (filter #(leave-together ids t %) ids)
        bus-ids (apply * (map second matches))
        done-ids (map first matches)]
    { :done (set/union done (set done-ids)) :agg (conj agg [t bus-ids]) }))

(defn aggregate-simultaneous-departures [ids]
  (let [departsMap (reduce
                    (fn [state id]
                      (if ((:done state) (first id))
                        state
                        (fold-ids ids state (first id))))
                    { :done #{}, :agg [] }
                    (sort-by first > ids)) ;ids desc by t delta
        departs (:agg departsMap)
        most-busy (apply max-key second departs)
        delta (first most-busy)
        step (second most-busy)
        departs-translated (map (fn [[t id]] [(- t delta) id]) departs)]
    ;; sort departs by bus id aggregate desc
    [delta step (sort-by second > departs-translated)]))

(defn meets-departure-constraints [departs t]
  (->>
    departs
    (map (fn [[d bid]] (= 0 (mod (+ t d) bid))))
    (every? identity)))

(defn solve-b [lower-t-bound ids]
  ;; delta is a time delta (shift or translation)
  ;; step is bus ids multiplied per most busy t which corresponds to delta
  ;; departs - aggregated departs
  (let [[delta step departs] (aggregate-simultaneous-departures ids)
        ;;we treat step as a bus id below
        start (find-first-departure [step] lower-t-bound)
        constraints (filter #(not (= step (second %))) departs)] 
    (loop [t (:t start)]
      (if (meets-departure-constraints constraints t)
        (- t delta)
        (recur (+ t step))))))

(defn -main
  "Solve AoC2020 Day13"
  [& args]
  (let [[fname lower-t-bound] ["input.txt" 100000000000000]
  ;;(let [[fname lower-t-bound] ["sample.txt" 1000]
        input (parse-input (read-input fname))]
    ;; 410
    (println "Solving Day13A...")
    (println (solve-a input))
    ;; 600691418730595
    (println "Solving Day13B...")
    (println (solve-b lower-t-bound (:ids input)))))
