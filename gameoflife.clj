(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn println-passthrough [s] (do (println (pr-str (doall s))) s))


(defrecord Cell [x y])

(defn render-row [cells row cols]
  (->> 
    (for [col (range cols)] (if (contains? cells (Cell. col row)) "#" "."))
    (apply str)
    (str)))

(defn print-grid [living-cells rows cols]
  (->> (range rows)
    (map #(render-row living-cells % cols))
    (str/join "\n")
    println))

(defn map-indexed-with-row [rownum rowval]
  (map-indexed #(vector rownum %1 %2) rowval)
)

(defn parse-living [s]
  (->> 
    ; split into lines
    (str/split-lines s)
    ; get map [row, line]
    (map-indexed vector)
    ; get map [row, col, char]
    (map #(map-indexed-with-row (get % 0) (get % 1)))
    ; concat all together
    (apply concat)
    ; filter for hashmarks (living)
    (filter #(= \# (get % 2)))
    ; make Cells
    (map #(Cell. (first %) (second %)))
    ; make set
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
  (->> (map enumerate-neighbors current)
    (apply concat)
    frequencies
    (filter #(in-bounds? (first %) rows cols))
    (filter #(should-live? current (first %) (second %)))
    (map #(->Cell (first (first %)) (second (first %))))
    set))

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

