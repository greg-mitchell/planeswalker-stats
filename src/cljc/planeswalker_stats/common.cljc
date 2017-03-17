(ns planeswalker-stats.common
  (:require
   [clojure.string :as s]
   #?@(:cljs [[cljs.tools.reader :as js-reader]
              [cljs.js :as js]]
       :clj  [[clojure.edn :as edn]])))


(def tournament-matcher #"^(?<date>\d{4}-\d{2}-\d{2})(?<title>[A-Za-z\d ](?:(?<![a-z)\d])[A-Z]|[a-z\d –.&'()#\/:-]|StarCityGames\.com)+)(?<location>(?:[A-Z][a-zA-Z .()&',-]+|-))(?<lp>(?:\d{1,3}|-))(?<pp>(?:\d{1,2}|-))$")

(def match-history-pattern #"^(\d+)\t([A-Za-z]+)\t\(\+(\d+)\)\t(.*)$")

(defn parse-tournament-title [t]
  (let [[match date title location lp pp] (re-matches tournament-matcher t)]
    (when match
      {:date date
       :lifetime-p lp
       :location location
       :pro-p pp
       :raw-title match
       :title title})))

(defn drop-header [history-lines]
  "Remove leading lines from the planeswalker points history line seq"
  (->> history-lines
       (drop-while (complement (partial re-matches #"DateTitleLocationLifetime PointsPro Points")))
       (drop 1)))

(defn parse-points [val]
  "Parse a field in the tournament title line to an int (default to 0)"
  (if (or (nil? val) (= val "-"))
    0
    #?(:cljs (js/parseInt val)
       :clj  (Integer/parseInt val))))

(defn parse-matches
  "Parse the lines of a single tournament into a match history record"
  [lines]
  (->> lines
       (map (partial re-matches match-history-pattern))
       (remove nil?)
       (mapv (fn [[_ round result points name]]
               {:round (parse-points round)
                :result result
                :name name}))))

(defn update-with-lines
  "Parse lines from record and merge in parsed values"
  [r]
  (let [p-lines (->> (:lines r)
                     (filter #(s/includes? % ": "))
                     (map #(s/split % #": "))
                     (into {}))
        matches (parse-matches (:lines r))]
    (merge
     r
     {:format (p-lines "Format")
      :lifetime-p (or (parse-points (p-lines "Lifetime")) (:lifetime-p r))
      :matches matches
      :multiplier (if-let [mul-str (p-lines "Event Multiplier")]
                    (->> mul-str
                         (re-matches #"x (\d+)")
                         second
                         parse-points))
      :participation-p (parse-points (p-lines "Participation Points"))
      :place (parse-points (p-lines "Place"))
      :players (parse-points (p-lines "Players"))
      :pro-p (or (parse-points (p-lines "Pro")) (:pro-p r))})))

(defn process-record
  "Process a raw record by parsing fields and removing redundant info"
  [r]
  (-> r
      (update-in [:lifetime-p] parse-points)
      (update-in [:pro-p] parse-points)
      (update-with-lines)
      (dissoc :lines)))

(defn parse-tournament-records
  "Parse a sequence of tournament lines (including header) into a seq of tournament
   records"
  [history-lines]
  (->> history-lines
       drop-header
       ;; Parse lines into a map from title to a record of the tournament
       (reduce (fn [{:keys [curr] :as emap} line]
                 (if-let [match (parse-tournament-title line)]
                   (let [id (inc curr)]
                     (assoc emap id (assoc match :lines [])
                            :curr id))
                   (update-in emap [curr :lines] #(conj % line))))
               {:curr -1})
       ;; Remove helper state from map
       (#(dissoc % :curr))
       ;; Get seq of tournament records (id is arbitrary)
       (map val)
       ;; Sorts from earliest to latest
       (sort-by :date)
       reverse
       ;; Finish processing a record
       (map process-record)))

(defn results
  "Get a seq of results, from lowest to highest point totals, where each result looks like
   [{:wins w :draws d :byes b :losses l :matches m} occurrences]"
  [pp-records]
  (->> pp-records
       (map (fn [{:keys [matches]}]
              (let [{:strs [Win Draw Bye Loss]} (frequencies (map :result matches))]
                {:wins (or Win 0)
                 :draws (or Draw 0)
                 :byes (or Bye 0)
                 :losses (or Loss 0)
                 :matches (count matches)})))
       frequencies
       (sort-by (fn [[{:keys [wins draws]} _]]
                  (+ (* 3 wins) draws)))))

(defn summarize-results
  "From a results seq, calculate the summary statistics in a map with keys:
   :win-percent, :win-percent-no-draws, :wins (not including byes), :losses,
   :draws, :byes, :matches (including byes)"
  [results]
  (let [{:keys [wins byes draws matches] :as sum-records}
        (if (empty? results)
          {:wins 0
           :draws 0
           :byes 0
           :losses 0
           :matches 0}
          (->> results
               (mapcat (fn [[result occurrences]] (repeat occurrences result)))
               (apply merge-with +)))
        non-bye-matches (- matches byes)
        winp (if (> non-bye-matches 0) (/ wins non-bye-matches) 0)
        winp-no-draws (if (> (- non-bye-matches draws) 0)
                        (/ wins (- non-bye-matches draws))
                        0)]
    (assoc sum-records :win-percent (* 100. winp)
           :win-percent-no-draws (* 100. winp-no-draws))))

(defn results-by-player
  "Build a sorted list of tuples from player name to summarized results
  (see summarize-results) from a seq of tournament records"
  [records]
  (->> records
       (mapcat :matches)
       (group-by :name)
       (map (fn [[player matches :as grouped]]
              (let [dummy-tournament {:matches matches}
                    results (results [dummy-tournament])]
                [player (summarize-results results)])))
       (sort-by (comp :matches second))
       reverse))

;; Argument parsing
#?(:clj (defn parse-predicates
          "Reads EDN strings into predicate functions"
          [pred-strs]
          (->> pred-strs
               (map #(try
                       (eval (edn/read-string %)))
                    (catch Exception e
                      (println "Error parsing predicate, ignoring:" %)
                      (println "Message:" (.getMessage e))))
               (filter #(and % (fn? %))))))

(defn contains-arg?
  "Returns a seq of values following instances of flag in args if any are present,
   otherwise nil"
  [args flag]
  (let [values (->> (partition 2 1 args)
                    (filter (fn [[a1 a2]] (and (= flag a1) a2)))
                    (map second))]
    (if (empty? values) nil values)))

(defn preds-for
  [field values]
  (map (fn [v]
         (fn [record]
           (= v (get record field))))
       values))

(defn player-pred-for
  [player-lists]
  (let [player-set (apply hash-set (mapcat #(s/split % #";") player-lists))]
    [(fn [{:keys [matches] :as _record}]
       (->> matches (map :name) (some player-set)))]))

(defn parse-args
  [args]
  (let [argset (set args)
        argmap (volatile! {:predicates [(constantly true)]
                           :source (last args)})]
    (when (or (empty? argset)
              (contains? argset "-h")
              (contains? argset "--help"))
      (vswap! argmap assoc :help true))
    (when (contains? argset "-r")
      (vswap! argmap assoc :detail-results true))
    (when (contains? argset "-d")
      (vswap! argmap assoc :detail-players true))
    #?(:clj (when-let [values (contains? args "-?")]
              (vswap! argmap update-in [:predicates] concat (parse-predicates values))))
    (when-let [values (contains-arg? args "-l")]
      (vswap! argmap update-in [:predicates] concat (preds-for :location values)))
    (when-let [values (contains-arg? args "-f")]
      (vswap! argmap update-in [:predicates] concat (preds-for :format values)))
    (when-let [values (contains-arg? args "-p")]
      (vswap! argmap update-in [:predicates] concat (player-pred-for values)))
    @argmap))

(defn execute
  [argmap]
  (if (:help argmap)
    {:help true}
    (let [records (parse-tournament-records (:lines argmap))
          matching-records (filter (apply every-pred (:predicates argmap)) records)
          detailed-results (results matching-records)]
      {:matching-records (count matching-records)
       :summary (summarize-results detailed-results)
       :detail-results (when (:detail-results argmap) detailed-results)
       :detail-players (when (:detail-players argmap)
                         (results-by-player matching-records))
       })))
