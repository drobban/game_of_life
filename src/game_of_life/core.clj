(ns game-of-life.core
  (:require [clojure.pprint :refer [pprint]])
  ;; (:gen-class)
  )

(def seed [[1 0 1 0 0 0 0 0 0 0]
           [1 0 0 0 1 0 0 1 1 1]
           [1 0 0 1 0 0 0 0 0 0]
           [0 0 0 1 1 0 0 0 1 0]
           [0 0 0 1 0 1 1 0 1 0]
           [0 0 0 0 1 0 0 1 1 0]
           [0 0 1 1 1 1 1 0 0 0]
           [0 0 0 0 1 0 1 0 0 0]
           [1 1 1 1 1 0 1 0 0 0]
           [0 0 0 0 1 0 1 0 0 0]])

(defn neighbours
  "returns list of x y tupples of neighbours"
  [x y world]
  (filterv #(not (or (nil? (get-in world %)) (= 0 (get-in world %))))
           (for [nx (range (- x 1) (+ x 2))
                 ny (range (- y 1) (+ y 2))
                 :when (not (and (= x nx) (= y ny)))]
             [ny nx])))

(defn alive?
  "counts neighbours
  cell<2 dies
  cell>3 dies
  and cell contains life
  "
  [x y world]
  (let [n-alive (neighbours x y world)]
    (if (= 1 (get-in world [y x]))
      (cond
        (< (count n-alive) 2) false
        (> (count n-alive) 3) false
        :else true)
      false)))

(defn new-life?
  "counts neighbours
  dead cell with 3 neighbours starts to live
  "
  [x y world]
  (let [n-alive (neighbours x y world)]
    (if (= 0 (get-in world [y x]))
      (cond
        (= (count n-alive) 3) true
        :else false)
      false)))

(defn dead-or-alive
  [x y world]
  (cond
    (alive? x y world) 1
    (new-life? x y world) 1
    :else 0))




(defn render
  "Run game of life"
  [world]
  (let [cells (for [nx (range (count (first world)))
                    ny (range (count world))]
                [ny nx])]
    (loop [[y x] (first cells) cells (rest cells)
           new-world world
           old-world world]
      (if (nil? y)
        new-world
        (recur
         (first cells)
         (rest cells)
         (assoc-in new-world [y x] (dead-or-alive x y old-world))
         old-world)))))


(defn trim_into [board_selection brick_selection pos]
  "Trim into returns a new brick data that will fit into board at pos"
  (if (< (count board_selection) (+ pos (count brick_selection)))
    (subvec brick_selection
            0
            (max 0 (- (count brick_selection)
                      (- (+ pos (count brick_selection))
                         (count board_selection)))))
    (subvec brick_selection
            (max 0 (min (count brick_selection)
                        (- (count brick_selection)
                           (+ (count brick_selection) pos))))
            (count brick_selection))))

(defn mask_in [board_row brick_row pos_col]
  "Merges a brick row into board row at a given column"
  (let [beginning (subvec board_row
                          0
                          (max 0 pos_col))
        selected (subvec board_row
                         (max 0 pos_col)
                         (+ (count (trim_into board_row brick_row pos_col)) (max 0 pos_col)))
        end (subvec board_row
                    (+ (max 0 pos_col) (count (trim_into board_row brick_row pos_col)))
                    (count board_row))]
    (vec (concat beginning (map bit-or selected (trim_into board_row brick_row pos_col)) end))))


(defn brick_position [gameboard brick pos_row pos_col]
  "Merges brick into gameboard at position row & col"
  (let [game_row (min (count gameboard) (max 0 pos_row))
        trimmed_brick (trim_into gameboard brick (min (count gameboard) pos_row))]
    (loop [processed_rows (subvec gameboard 0 game_row)
           selected_row (nth gameboard game_row)
           rest_rows (subvec gameboard (+ game_row 1) (count gameboard))
           row_idx 0]
      (if (= row_idx (count trimmed_brick))
        (if (nil? selected_row)
          (into processed_rows rest_rows)
          (into (conj processed_rows selected_row) rest_rows))
        (recur (conj processed_rows (mask_in selected_row (nth trimmed_brick row_idx) pos_col))
               (first rest_rows)
               (rest rest_rows)
               (inc row_idx))))))

;; (pprint (brick_position (vec (repeat 32 (vec (repeat 32 0)))) world 15 15))

;; (run world)
;; (pprint (last (take 129 (iterate #(render %) world))))
;; (pprint (last (take 179 (iterate #(render %) (brick_position (vec (repeat 32 (vec (repeat 32 0)))) world 15 15) ))))
;; (alive? 4 3 world)
