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

(defn print-grid [living-cells rows cols]
  (print (str (apply str (for [y (range rows)] (render-row living-cells y cols)))  "\n"))
)

(defn parse-living [s]
  (set 
    (map 
      #(Cell. (first %) (second %))
      (filter
        #(= \# (get % 2))
        (apply concat
          (for [[row row-elt]
            (map-indexed vector (str/split-lines s))]
            (map #(vector (first %) row (second %)) (map-indexed vector row-elt))
          )
        )
      )
    )
  )
)

(defn count-neighbors [cell]
  (for [x (range -1 2) 
        y (range -1 2)
        :when (not (and (= x 0) (= y 0)))] 
    [(+ (:x cell) x) (+ (:y cell) y)]
  )
)

(defn should-live? [current coord num-living-neighbors]
  (or
    (= num-living-neighbors 3)
    (and (= num-living-neighbors 2) (contains? current (Cell. (first coord) (second coord))))
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

(defn next-generation [current rows cols]
  (set (map 
    #(Cell. (first (first %)) (second (first %)))
    (filter 
      #(should-live? current (first %) (second %))
      (filter 
        ; #(do (println (first %) (in-bounds? (first %) rows cols)))
        #(in-bounds? (first %) rows cols)
        (frequencies
          (apply concat (map count-neighbors current))
        )
      )
    )
  ))
)



; ; blinker
; (def initial-state (str/join "\n" [
; "     "
; "  #  "
; "  #  "
; "  #  "
; "     "
; ]))

; glider
(def initial-state (str/join "\n" [
" #   "
"  #  "
"###  "
"     "
"     "
]))

; pulsar
(def initial-state (str/join "\n" [
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
"                 "


]))

(def rows (inc (count (filter 
  #(= \newline %) 
  initial-state)
)))
(def cols (count 
  (first 
    (str/split-lines initial-state)
  )
))


(loop [cur-gen (parse-living initial-state) i 0]
  (print-grid cur-gen rows cols)
  (println (apply str (repeat 10 "-")))
  (if (= i 10)
    i
    (recur (next-generation cur-gen rows cols) (inc i))
  )
)

