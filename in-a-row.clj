(comment
  ;; To be played via the REPL. Example:
  (load-file "in-a-row.clj")
  (start)
  (play :b2))

(use '[clojure.contrib.seq-utils :only [positions]])

(def dim 3)      ;; 3x3, 3-in-a-row = tic-tac-toe
(def in-a-row 3) ;; 19x19, 5-in-a-row = gomoku
(def board (atom nil))
(def mark (atom nil))

(defn- int->letter [x] (char (+ (int \A) x)))
(defn- letter->int [c] (- (int c) (int \A)))
(defn- to-int [x] (try (Integer. x) (catch Exception _ nil)))
;(defn- indexed [v] (map vector (iterate inc 0) v))
;(defn- indexes-of [x v] (map first (filter #(= x (second %)) (indexed v))))

(defn render-board [board]
  (apply str
         "    " (apply str (interpose " " (map int->letter (range dim)))) "\n"
         (map #(format "%2d %s\n" %1 (apply str %2))
              (iterate inc 1)
              (partition dim (map {:x " X" :o " O" :e " ."} board)))))

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

;; :a2 => [0 1]
(defn parse-coord [coord]
  [(letter->int (.charAt (.toUpperCase (name coord)) 0))
   (dec (to-int (.substring (name coord) 1)))])

(defn opposite [mark]
  (mark {:x :o :o :x}))

;;  1 on win for mark
;; -1 on win for (opposite mark)
;;  0 otherwise
(defn evaluate [board mark]
  (condp #(winner? %2 %1) board
    mark 1
    (opposite mark) -1
    0))

(defn next-moves [board]
  (positions (partial = :e) board))

;; contender positions: (for [m (next-moves @board)] (assoc @board m @mark))
;; contender evals: (for [m (next-moves @board)] (minimax (assoc @board m @mark) @mark 10))
(defn minimax [board mark depth]
  (let [moves (next-moves board)
	opp-mark (opposite mark)
	val (evaluate board mark)]
     (if (or (= 1 val) (= -1 val) (empty? moves) (zero? depth))
       val
       (apply max
	      (cons -9999 (map #(- (minimax (assoc board % opp-mark)
					    opp-mark (dec depth)))
			       moves))))))

(defn start []
  (reset! board (vec (repeat (* dim dim) :e)))
  (reset! mark :x)
  (print (render-board @board)))

(defn play [coord]
  (let [[x y] (parse-coord coord)]
    (swap! board assoc (+ (* y dim) x) @mark)
    (print (render-board @board))
    (when (winner? @board @mark)
      (println "\n ***" (name @mark) "wins! ***"))
    (swap! mark opposite)
    nil))


;; breadth-first search of all future game positions
;; not used
(defn game-branches [start-board start-mark]
  (loop [branches []
	 queue (conj clojure.lang.PersistentQueue/EMPTY
		     [start-board start-mark])]
    (if-let [[board mark] (peek queue)]
      (let [empty-positions (positions (partial = :e) board)]
	(recur (conj branches board)
	       (reduce conj
		       (pop queue)
		       (map #(vector (assoc board % mark) (opposite mark))
			    empty-positions))))
      branches)))

