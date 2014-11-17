(ns rts.colourshift
  (:use rts.engine))

(def directions {:east [1 0]
                 :west [-1 0]
                 :north [0 -1]
                 :south [0 1]})

(def blendings {#{} :gray
                #{:red} :red
                #{:blue} :blue
                #{:green} :green
                #{:red :blue} :magenta
                #{:red :green} :yellow
                #{:blue :green} :cyan
                #{:red :blue :green} :white})

(defn fmap [f m]
  (into {} (map (fn [[k v]]
                  [k (f k v)]) m)))

(defn ensure-vector [obj]
  (if (vector? obj)
    obj
    [obj]))

(defn add-revref-to [board]
  (fmap (fn [pos tile]
          (assoc tile :pos pos)) board))

(defn pos-add [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ x1 x2) (+ y1 y2)]))

(defn find-by-pos [pos tile-list]
  (first (filter #(= pos (:pos %)) tile-list)))

(defn subtract-by-pos [total unwanted]
  (let [unwanted-pos-set (into (hash-set) (map :pos unwanted))
        remaining (remove #(contains? unwanted-pos-set (:pos %)) total)]
    remaining))

(defn get-neighbors-by-pos-diffs [tile tile-list]
  (let [connection-dirs (ensure-vector (:connection tile))
        pos (:pos tile)
        pos-diffs (map #(% directions) connection-dirs)
        connected-poses (into (hash-set) (map #(pos-add pos %) pos-diffs))]
    (filter #(contains? connected-poses (:pos %)) tile-list)))

(defn breadth-first-traverse [start-tile all-tiles get-neighbors-of subtract]
  (let [breadth-first-recur
        (fn [open-list closed-list raw-list]
          (let [seed (first open-list)
                raw-neighbors (get-neighbors-of seed raw-list)
                unvisited-neighbors (subtract raw-neighbors (concat open-list closed-list))
                new-open-list (concat unvisited-neighbors (rest open-list))
                new-closed-list (conj closed-list seed)
                new-raw-list (subtract raw-list unvisited-neighbors)]
            (if (empty? unvisited-neighbors)
              (concat open-list closed-list)
              (recur new-open-list new-closed-list new-raw-list))))
        unused-tiles (subtract all-tiles [start-tile])]
    (breadth-first-recur [start-tile] [] all-tiles)))

(defn find-connected-subgraphs [all-tiles get-neighbors-of subtract]
  (let [find-connected-subgraphs-recur
        (fn [remaining-tiles subgraphs]
          (if (empty? remaining-tiles)
            subgraphs
            (let [seed-tile (first remaining-tiles)
                  new-subgraph (breadth-first-traverse seed-tile
                                                       (rest remaining-tiles)
                                                       get-neighbors-of
                                                       subtract)
                  unused-tiles (subtract-by-pos remaining-tiles new-subgraph)]
              (recur unused-tiles (conj subgraphs new-subgraph)))))]
    (find-connected-subgraphs-recur all-tiles [])))

(defn dye-subgraph [tile-list]
  (let [sources (filter #(= :source (:type %)) tile-list)
        colours (into (hash-set) (map :colour sources))
        merged-colour (get blendings colours)]
    (map (fn [tile]
           (if (= :source (:type tile))
             tile
             (assoc tile :colour merged-colour)))
         tile-list)))

(defn dye-board [board]
  (let [subgraphs (find-connected-subgraphs board
                                            get-neighbors-by-pos-diffs
                                            subtract-by-pos)
        dyed-subgraphs (map dye-subgraph subgraphs)]
    (apply concat dyed-subgraphs)))

