(comment
  ;; To be played via the REPL. Example:
  (load-file "in-a-row.clj")
  (start)
  (play :b2))

(def dim 3)      ;; 3x3, 3-in-a-row = tic-tac-toe
(def in-a-row 3) ;; 19x19, 5-in-a-row = gomoku
(def board (atom nil))
(def mark (atom nil))

(defn- int->letter [x] (char (+ (int \A) x)))
(defn- letter->int [c] (- (int c) (int \A)))
(defn- to-int [x] (try (Integer. x) (catch Exception _ nil)))

(defn render-board [board]
  (apply str
         "    " (apply str (interpose " " (map int->letter (range dim)))) "\n"
         (map #(format "%2d %s\n" %1 (apply str %2))
              (iterate inc 1)
              (partition dim (map {:X " X" :O " O" :empty " ."} board)))))

(defn in-a-row? [val n coll]
  (some #(every? (partial = val) %)
        (partition n 1 coll)))

(defn winner? [board mark]
  (let [rows (partition dim board)
        cols (apply map vector rows)
        diags (mapcat #(for [i %1] (map get (map vec rows) (iterate %2 i)))
                      [(range (- 1 dim) dim) (range (dec (* 2 dim)))]
                      [inc dec])]
    (some #(in-a-row? mark in-a-row %) (concat rows cols diags))))

(defn start []
  (reset! board (vec (repeat (* dim dim) :empty)))
  (reset! mark :X)
  (print (render-board @board)))

(defn play [coord]
  (let [x (letter->int (.charAt (.toUpperCase (name coord)) 0))
        y (dec (to-int (.substring (name coord) 1)))]
    (swap! board assoc (+ (* y dim) x) @mark)
    (print (render-board @board))
    (when (winner? @board @mark)
      (println "\n ***" (name @mark) "wins! ***"))
    (swap! mark {:X :O :O :X})
    nil))