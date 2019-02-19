(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defrecord Cell [x y])

(defn render-row [cells row cols]
  (str
    (apply str
      (for [col (range cols)] 
        (if (contains? cells (Cell. col row)) "#" ".")
      )
    )
    "\n"
  )
)

(defn render-row [cells row cols]
  (->> 
    (for [col (range cols)] (if (contains? cells (Cell. col row)) "#" "."))
    (apply str)
    (str)))

(defn print-grid [living-cells rows cols]
  (->> (for [y (range rows)] 
    (->> (render-row living-cells y cols)
      str))
    (str/join "\n")
    println))

(defn parse-living [s]
  (->> 
    (for [[row row-elt]
      (map-indexed vector (str/split-lines s))]
      (map #(vector (first %) row (second %)) (map-indexed vector row-elt))
    )
    (apply concat)
    (filter #(= \# (get % 2)))
    (map #(Cell. (first %) (second %)))
    (set)))

(defn enumerate-neighbors [cell]
  (for [x (range -1 2) 
        y (range -1 2)
        :when (not (and (= x 0) (= y 0)))]
    [(+ (:x cell) x) (+ (:y cell) y)]))

(defn should-live? [living-cells coord num-living-neighbors]
  (or
    (= num-living-neighbors 3)
    (and (= num-living-neighbors 2) (contains? living-cells (Cell. (first coord) (second coord))))
  )
)

(defn in-bounds? [coord rows cols]
  (and
    (>= (first coord) 0)
    (>= (second coord) 0)
    (< (first coord) cols)
    (< (second coord) rows)
  )
)

(defn next-generation "Compute the next generation from the current one. Limits range to (0,0) to (cols,rows)." [current rows cols]
  (set (map 
    #(Cell. (first (first %)) (second (first %)))
    (filter 
      #(should-live? current (first %) (second %))
      (filter 
        #(in-bounds? (first %) rows cols)
        (frequencies
          (apply concat (map enumerate-neighbors current))
        )
      )
    )
  ))
)

; blinker
(def blinker (str/join "\n" [
"     "
"  #  "
"  #  "
"  #  "
"     "
]))

; glider
(def glider (str/join "\n" [
" #   "
"  #  "
"###  "
"     "
"     "
]))

; pulsar
(def pulsar (str/join "\n" [
"                 "
"                 "
"    ###   ###    "
"                 "
"  #    # #    #  "
"  #    # #    #  "
"  #    # #    #  "
"    ###   ###    "
"                 "
"    ###   ###    "
"  #    # #    #  "
"  #    # #    #  "
"  #    # #    #  "
"                 "
"    ###   ###    "
"                 "
"                 "
]))

(def initial-state pulsar)

; count rows and columns from input
(def rows (inc (count (filter 
  #(= \newline %) 
  initial-state)
)))
(def cols (count 
  (first 
    (str/split-lines initial-state)
  )
))

; main loop
(loop [cur-gen (parse-living initial-state) i 0]
  (print-grid cur-gen rows cols)
  (println (apply str (repeat 10 "-")))
  (if (= i 10)
    i
    (recur (next-generation cur-gen rows cols) (inc i))
  )
)

