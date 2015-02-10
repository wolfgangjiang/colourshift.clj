(ns rts.colourshift
  (:use rts.engine)
  (:import [java.awt BasicStroke Color]))

;;;; ============= tile-set ===================

(defn map-over-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn ts-make-tileset [tile-list]
  (reduce (fn [ts tile]
            (let [pos (:pos tile)]
              (if (contains? ts pos)
                (update-in ts [pos] conj tile)
                (assoc ts pos #{tile}))))
          {}
          tile-list))

(defn ts-flat-tile-list [ts]
  (let [values (vals ts)
        value-vecs (map vec values)
        all-values-list (reduce concat value-vecs)]
    all-values-list))

(defn ts-map [f ts]
  (map-over-values (fn [set-of-tiles]
                     (set (map f set-of-tiles)))
                   ts))

(defn ts-filter [f ts]
  (filter f (ts-flat-tile-list ts)))

(defn ts-find-by-id [id ts]
  (first (ts-filter #(= id (:id %)) ts)))

(defn ts-remove-by-id [ts tile]
  (let [{:keys [id pos]} tile
        same-pos-set (get ts pos)
        same-pos-set-without-unwanted-tile
        (set (remove #(= id (:id %)) same-pos-set))]
    (if (empty? same-pos-set-without-unwanted-tile)
      (dissoc ts pos)
      (assoc ts pos same-pos-set-without-unwanted-tile))))

(defn ts-subtract-by-id [ts unwanted-ts]
  (reduce ts-remove-by-id ts (ts-flat-tile-list unwanted-ts)))

(defn ts-rand-nth [ts]
  (let [picked-key (rand-nth (keys ts))]
    (first (get ts picked-key))))

(defn ts-conj [ts tile]
  (let [{:keys [pos]} tile]
    (if (contains? ts pos)
      (assoc ts pos (conj (get ts pos) tile))
      (assoc ts pos #{tile}))))

(defn ts-concat [ts1 ts2]
  (reduce ts-conj ts1 (ts-flat-tile-list ts2)))

;;;; ================ dyeing ===================

(def directions {:east [1 0]
                 :west [-1 0]
                 :north [0 -1]
                 :south [0 1]})

(def direction-names {[1 0] :east
                      [-1 0] :west
                      [0 -1] :north
                      [0 1] :south})

(def opposites {:east :west
                :west :east
                :north :south
                :south :north})

(def blendings {#{} :gray
                #{:red} :red
                #{:blue} :blue
                #{:green} :green
                #{:red :blue} :magenta
                #{:red :green} :yellow
                #{:blue :green} :cyan
                #{:red :blue :green} :white})

(def colour-rgbs {:gray Color/LIGHT_GRAY
                  :red Color/RED
                  :green Color/GREEN
                  :blue Color/BLUE
                  :magenta Color/MAGENTA
                  :cyan Color/CYAN
                  :yellow Color/YELLOW
                  :white Color/WHITE})

(def dir-rotations {:north :east
                    :west :north
                    :south :west
                    :east :south})

(defn fmap [f m]
  (into {} (map (fn [[k v]]
                  [k (f k v)]) m)))

(defn add-revref-to [board]
  (fmap (fn [pos tile]
          (assoc tile :pos pos)) board))

(defn pos-add [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ x1 x2) (+ y1 y2)]))

(defn pos-scale-multiply [[x y] efficient]
  [(* x efficient) (* y efficient)])

(defn pos-diff [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(- x1 x2) (- y1 y2)]))

(defn list-find-by-id [id tile-list]
  (first (filter #(= id (:id %)) tile-list)))

;;; CAUTION: incorrect in the face of twin-wires
;; (defn find-by-pos [pos tile-list]
;;   (first (filter #(= pos (:pos %)) tile-list)))

(defn list-subtract-by-id [total unwanted]
  (let [unwanted-id-set (into (hash-set) (map :id unwanted))
        remaining (remove #(contains? unwanted-id-set (:id %)) total)]
    remaining))

(defn list-find-by-connection [dir tile tile-list]
  (let [connection-dirs (into (hash-set) (:connection tile))
        pos (:pos tile)]
    (if (not (contains? connection-dirs dir))
      nil  ;; active non-connection
      (let [pos-diff (directions dir)
            pos-on-dir (pos-add pos pos-diff)
            opposite-dir (opposites dir)
            tiles-on-dir (filter #(= pos-on-dir (:pos %)) tile-list)
            connected (filter (fn [t]
                                (let [t-connection (:connection t)]
                                  (some #{opposite-dir} t-connection))) tiles-on-dir)]
        (if (empty? connected)
          nil
          (first connected))))))

(defn ts-find-by-connection [dir tile ts]
  (let [connection-dirs (into (hash-set) (:connection tile))
        pos (:pos tile)]
    (if (not (contains? connection-dirs dir))
      nil  ;; active non-connection
      (let [pos-diff (directions dir)
            pos-on-dir (pos-add pos pos-diff)
            tiles-on-dir (get ts pos-on-dir)
            opposite-dir (opposites dir)
            connected (filter (fn [t]
                                (let [t-connection (:connection t)]
                                  (some #{opposite-dir} t-connection))) tiles-on-dir)]
        (if (empty? connected)
          nil
          (first connected))))))


(defn is-connected [t1 t2]
  (some #(not (nil? (ts-find-by-connection % t1 [t2])))
        (:connection t1)))

(defn is-adjacent [t1 t2]
  (let [dpos (pos-diff (:pos t1) (:pos t2))]
    (contains? direction-names dpos)))

(defn is-of-type [tile type]
  (= (:type tile) type))

(defn is-bulb [tile]
  (is-of-type tile :bulb))

(defn is-source [tile]
  (is-of-type tile :source))

(defn is-twin-wire [tile]
  (is-of-type tile :twin-wire))

(defn is-bulb-lit [tile]
  (and (:expected-colour tile)
       (= (:colour tile) (:expected-colour tile))))

(defn victory? [dyed-board]
  (let [bulbs (ts-filter is-bulb dyed-board)]
    (every? is-bulb-lit bulbs)))

(defn get-tiles-on-pos [pos tile-list]
  (filter #(= pos (:pos %)) tile-list))

(defn is-shared-pos [pos tile-list]
  (let [tiles-count-on-pos (count (get-tiles-on-pos pos tile-list))]
    (> tiles-count-on-pos 1)))

(defn list-get-neighbors-by-connection [tile tileset]
  (let [connected-tiles (map #(list-find-by-connection % tile tileset) (keys directions))]
    (remove nil? connected-tiles)))

(defn ts-get-neighbors-by-connection [tile tileset]
  (let [connected-tiles (map #(ts-find-by-connection % tile tileset) (keys directions))]
    (ts-make-tileset (remove nil? connected-tiles))))

(defn rotate-dir [dir]
  (dir-rotations dir))

(defn rotate-tile-once [tile]
  (update-in tile
             [:connection]
             (fn [old-con]
               (map rotate-dir old-con))))

(defn rotate-tile-multiple-times [tile times]
  (if (<= times 0)
    tile
    (recur (rotate-tile-once tile) (dec times))))

;; (defn rotate-tiles-at-pos-multiple-times [pos times tile-list]
;;   (let [tiles-on-pos (get-tiles-on-pos pos tile-list)
;;         remaining (list-subtract-by-id tile-list tiles-on-pos)
;;         rotated-tiles (map #(rotate-tile-multiple-times % times) tiles-on-pos)]
;;     (concat rotated-tiles remaining)))

(defn ts-rotate-tiles-at-pos-multiple-times [pos times ts]
  (let [tiles-on-pos (vec (get ts pos))
        rotated-tiles (set (map #(rotate-tile-multiple-times % times) tiles-on-pos))]
    (assoc ts pos rotated-tiles)))

(defn get-pos-on-dir [origin-pos dir]
  (let [pos-diff (directions dir)
        pos-on-dir (pos-add origin-pos pos-diff)]
    pos-on-dir))

(defn breadth-first-traverse [start-tile all-tiles]
  (let [breadth-first-recur
        (fn [open-list closed-list raw-list]
          (if (empty? open-list)
            closed-list
            (let [seed (ts-rand-nth open-list)
                  raw-neighbors (ts-get-neighbors-by-connection seed raw-list)
                  unvisited-neighbors (ts-subtract-by-id raw-neighbors (ts-concat open-list closed-list))
                  new-open-list (ts-concat (ts-remove-by-id open-list seed) unvisited-neighbors)
                  new-closed-list (ts-conj closed-list seed)
                  new-raw-list (ts-subtract-by-id raw-list unvisited-neighbors)]
              (recur new-open-list new-closed-list new-raw-list))))
        unused-tiles (ts-subtract-by-id all-tiles (ts-make-tileset [start-tile]))]
    (breadth-first-recur (ts-make-tileset [start-tile]) (ts-make-tileset []) unused-tiles)))

(defn find-connected-subgraphs [all-tiles]
  (let [find-connected-subgraphs-recur
        (fn [remaining-tiles subgraphs]
          (if (empty? remaining-tiles)
            subgraphs
            (let [seed-tile (ts-rand-nth remaining-tiles)
                  new-subgraph (breadth-first-traverse seed-tile
                                                       (ts-remove-by-id remaining-tiles seed-tile))
                  unused-tiles (ts-subtract-by-id remaining-tiles new-subgraph)]
              (recur unused-tiles (conj subgraphs new-subgraph)))))]
    (find-connected-subgraphs-recur all-tiles [])))

(defn find-connected-subgraphs-default-setting [all-tiles]
  (find-connected-subgraphs all-tiles))


(defn get-subwire-colour [source-tile neighbor]
  (cond
   (nil? neighbor) (:colour source-tile)
   (is-source neighbor) (get blendings (set [(:colour source-tile)
                                             (:colour neighbor)]))
   :t (:colour neighbor)))


(defn dye-subwires-of-a-source [source-tile tile-list]
  (let [subwire-colour-pairs (map (fn [dir]
                                    (let [neighbor-on-dir (ts-find-by-connection dir source-tile tile-list)
                                          subwire-colour (get-subwire-colour source-tile neighbor-on-dir)]
                                      [dir subwire-colour]))
                                  (:connection source-tile))
        valid-pairs (remove #(nil? (second %)) subwire-colour-pairs)]
    (into (hash-map) valid-pairs)))

(defn blended-colour-of-subgraph [subgraph]
  (let [sources (ts-filter is-source subgraph)
        colours (set (map :colour sources))
        blended-colour (get blendings colours)]
    blended-colour))

(defn dye-subgraph-wires-and-bulbs [tileset]
  (let [blended-colour (blended-colour-of-subgraph tileset)]
    (ts-map (fn [tile]
              (if (is-source tile)
                tile
                (assoc tile :colour blended-colour)))
            tileset)))

(defn dye-subgraph-source-subwires [tileset]
  (ts-map (fn [tile]
            (if (is-source tile)
              (assoc tile :subwires (dye-subwires-of-a-source tile tileset))
              tile))
          tileset))

;;; implemented in two passes, to ensure that all neighbors are
;;; properly dyed when dyeing subwires of a source.
(defn dye-subgraph [tileset]
  (-> tileset
      dye-subgraph-wires-and-bulbs
      dye-subgraph-source-subwires))

(defn dye-board [board]
  (let [subgraphs (find-connected-subgraphs-default-setting board)
        dyed-subgraphs (map dye-subgraph subgraphs)]
    (apply concat dyed-subgraphs)))

;; ;;;; ============== generation =========================

(defn remove-leading-adjacent [picked remaining]
  (drop-while (fn [t]
                (some #(is-adjacent t %) picked))
              remaining))

(defn pick-at-most-n-non-adjacent-tiles-recur [picked remaining n]
  (let [good-remaining (remove-leading-adjacent picked remaining)]
    (cond
     (>= (count picked) n) picked
     (empty? good-remaining) picked
     :t (recur (conj picked (first good-remaining))
               (rest good-remaining)
               n))))

(defn pick-at-most-n-non-adjacent-tiles [tile-list n]
  (pick-at-most-n-non-adjacent-tiles-recur #{}
                                           (shuffle tile-list)
                                           n))

(defn random-twinwire-connection []
  (let [options [[[:north :east] [:south :west]]
                 [[:north :west] [:south :east]]
                 [[:north :south] [:west :east]]]]
    (rand-nth options)))

;; (defn random-normal-wire-connection []
;;   (let [options [[:north :south]
;;                  [:north :east]
;;                  [:north :west]
;;                  [:east :west]
;;                  [:east :south]
;;                  [:west :south]
;;                  [:north :south :east]
;;                  [:north :south :west]
;;                  [:east :west :north]
;;                  [:east :west :south]
;;                  [:north :south :east :west]]]
;;     (rand-nth options)))

(defn new-counter []
  (let [n (atom 0)]
    (fn []
      (swap! n inc')
      @n)))

(def generate-id (new-counter))

(defn generate-empty-board [x-size y-size]
  (mapcat (fn [x]
            (map (fn [y]
                   {:id (generate-id)
                    :pos [x y]})
                 (range y-size)))
          (range x-size)))

(defn generate-seeds [x-size y-size seed-count]
  (let [all-tiles (generate-empty-board x-size y-size)
        picked (pick-at-most-n-non-adjacent-tiles all-tiles seed-count)
        seeds (map (fn [proto-tile]
                     {:id (:id proto-tile)
                      :pos (:pos proto-tile)
                      :connection []})
                   picked)
        unoccupied (list-subtract-by-id all-tiles seeds)]
    [seeds unoccupied]))

(defn get-all-pairings [col-1 col-2]
  (mapcat (fn [item-1]
            (map (fn [item-2]
                   [item-1 item-2])
                 col-2))
          col-1))

(defn get-candidate-pairs-for-growth [subgraph proto-tiles]
  (let [all-pairs (get-all-pairings subgraph proto-tiles)]
    (filter (fn [[sg-tile p-tile]]
              (is-adjacent sg-tile p-tile)) all-pairs)))

(defn pick-for-growth [subgraph proto-tiles]
  (let [candidate-pairs (get-candidate-pairs-for-growth subgraph proto-tiles)]
    (if (empty? candidate-pairs)
      [nil nil]
      (rand-nth candidate-pairs))))

(defn substantiate-for-growth [seed picked-proto-tile]
  (let [dpos (pos-diff (:pos picked-proto-tile) (:pos seed))
        dir (direction-names dpos)
        seed-new-connection (vec (distinct (conj (:connection seed) dir)))
        opposite-dir (opposites dir)
        picked-proto-tile-connection [opposite-dir]
        grown-seed (assoc seed :connection seed-new-connection)
        substantiated-tile (assoc picked-proto-tile :connection
                                  picked-proto-tile-connection)]
    [grown-seed substantiated-tile]))

(defn subgraph-grow-with-seed-and-picked [proto-pool subgraph seed picked-proto-tile]
  (let [[grown-seed substantiated-tile] (substantiate-for-growth seed picked-proto-tile)
        subgraph-without-seed (list-subtract-by-id subgraph [seed])
        new-subgraph (concat subgraph-without-seed [grown-seed substantiated-tile])
        remaining-proto-pool (list-subtract-by-id proto-pool [picked-proto-tile])]
    [remaining-proto-pool new-subgraph]))

(defn subgraph-one-spurt-grow [proto-pool subgraph]
  (let [[seed picked-proto-tile] (pick-for-growth subgraph proto-pool)]
    (if (nil? picked-proto-tile)
      [proto-pool subgraph]
      (subgraph-grow-with-seed-and-picked proto-pool subgraph seed picked-proto-tile))))

(defn grow-subgraph-info [proto-pool old-subgraph-info]
  (let [{:keys [destiny-number subgraph-tiles]} old-subgraph-info
        [new-proto-pool new-subgraph-tiles]
        (subgraph-one-spurt-grow proto-pool subgraph-tiles)
        new-subgraph-info {:destiny-number destiny-number
                           :subgraph-tiles new-subgraph-tiles}]
    [new-proto-pool new-subgraph-info]))

(defn grow-subgraph-info-if-destiny-permits [proto-pool subgraph-info]
  (let [{:keys [destiny-number subgraph-tiles]} subgraph-info
        destiny-trial (rand)]
    ;; Unregard to destiny-trial, a subgraph is always allowed to grow
    ;; if it is too small.
    (if (or (< (count subgraph-tiles) 2)
            (< destiny-trial destiny-number))
      (grow-subgraph-info proto-pool subgraph-info)
      [proto-pool subgraph-info])))

(defn generate-solution-each-subgraph-recur [proto-pool new-subgraph-infos old-subgraph-infos]
  (if (empty? old-subgraph-infos)
    [proto-pool new-subgraph-infos]
    (let [old-info (first old-subgraph-infos)
          [new-proto-pool new-info] (grow-subgraph-info-if-destiny-permits proto-pool old-info)]
      (recur new-proto-pool
             (conj new-subgraph-infos new-info)
             (rest old-subgraph-infos)))))

(defn generate-solution-recur [proto-pool subgraph-info-list]
  (if (empty? proto-pool)
    subgraph-info-list
    (let [[remaining-proto-pool new-subgraph-info-list]
          (generate-solution-each-subgraph-recur proto-pool [] subgraph-info-list)]
      (recur remaining-proto-pool new-subgraph-info-list))))

(defn change-foursome-to-twinwire-maybe [plain-tile]
  (let [maybe (rand)]
    (if (or (< (count (:connection plain-tile)) 4)
            (< maybe 0.25))
      [plain-tile]
      (let [[con1 con2] (random-twinwire-connection)]
        [{:id (:id plain-tile)
          :pos (:pos plain-tile)
          :type :twin-wire
          :connection con1}
         {:id (generate-id)
          :pos (:pos plain-tile)
          :type :twin-wire
          :connection con2}]))))

(defn change-some-foursome-to-twinwires [plain-solution]
  (flatten
   (map change-foursome-to-twinwire-maybe plain-solution)))

(defn tag-sources [subgraph]
  (let [available-blendings (remove empty? (keys blendings))
        chosen-blending (rand-nth available-blendings)
        prepared-colours (shuffle (vec chosen-blending))
        available-tiles (remove is-twin-wire subgraph)
        picked-tiles (pick-at-most-n-non-adjacent-tiles available-tiles
                                                        (count prepared-colours))
        remaining-tiles (list-subtract-by-id subgraph picked-tiles)
        sources (mapcat (fn [tile colour]
                          (map (fn [dir]
                                 {:id (generate-id)
                                  :pos (:pos tile)
                                  :type :source
                                  :colour colour
                                  :connection [dir]})
                               (:connection tile)))
                        picked-tiles
                        (take (count picked-tiles) prepared-colours))]
    (concat sources remaining-tiles)))

(defn is-single-ended [tile]
  (<= (count (:connection tile)) 1))

(defn is-multi-ended [tile]
  (not (is-single-ended tile)))

(defn tag-one-bulb-or-wire [tile]
  (cond
   (= (:type tile) :source) tile
   (= (:type tile) :twin-wire) tile
   (is-multi-ended tile) (assoc tile :type :wire)
   :t (assoc tile :type :bulb)))

(defn tag-bulbs-and-wires [list-subgraph]
  (map tag-one-bulb-or-wire list-subgraph))

(defn tag-expected-colours [list-subgraph]
  (let [blended-colour (blended-colour-of-subgraph (ts-make-tileset list-subgraph))]
    (map (fn [tile]
           (if (is-bulb tile)
             (assoc tile :expected-colour blended-colour)
             tile))
         list-subgraph)))

(defn ensure-at-least-one-bulb-in-subgraph-if-possible [subgraph whole-board]
  (let [bulbs (filter is-bulb subgraph)]
    (if (>= (count bulbs) 1)
      subgraph
      (let [candidates (filter (fn [tile]
                                 (and (is-source tile)
                                      (is-single-ended tile)
                                      (not (is-shared-pos (:pos tile) whole-board))))
                               subgraph)]
        (if (empty? candidates)
          subgraph
          (let [picked (rand-nth candidates)
                remaining (list-subtract-by-id subgraph [picked])
                new-bulb (assoc picked :type :bulb)]
            (conj remaining new-bulb)))))))

(defn tag-solution [board]
  (let [ts-board (ts-make-tileset board)
        subgraphs (map ts-flat-tile-list (find-connected-subgraphs-default-setting ts-board))
        source-splitted-subgraphs (map tag-sources subgraphs)
        reunited-board (apply concat source-splitted-subgraphs)
        ts-reunited-board (ts-make-tileset reunited-board)
        redivided-subgraphs (map ts-flat-tile-list (find-connected-subgraphs-default-setting ts-reunited-board))
        type-tagged-subgraphs (map tag-bulbs-and-wires redivided-subgraphs)
        bulb-ensured-subgraphs (map #(ensure-at-least-one-bulb-in-subgraph-if-possible % reunited-board)
                                    type-tagged-subgraphs)
        full-tagged-subgraphs (map tag-expected-colours bulb-ensured-subgraphs)]
    (apply concat full-tagged-subgraphs)))

(defn generate-solution [x-size y-size seed-count]
  (let [[seed-list proto-pool]
        (generate-seeds x-size y-size seed-count)
        subgraph-info-list (map (fn [seed]
                                  {:destiny-number (rand)
                                   :subgraph-tiles [seed]})
                                seed-list)
        mature-subgraph-info-list
        (generate-solution-recur proto-pool subgraph-info-list)
        mature-subgraph-list (map :subgraph-tiles mature-subgraph-info-list)
        plain-solution (apply concat mature-subgraph-list)
        topological-solution (change-some-foursome-to-twinwires plain-solution)
        solution (tag-solution topological-solution)]
    solution))

(defn scramble-tiles-at-pos [pos ts-board]
  (let [maybe (rand)
        times (cond
               (< maybe 0.1) 0
               (< maybe 0.4) 1
               (< maybe 0.7) 2
               :t 3)]
    (ts-rotate-tiles-at-pos-multiple-times pos times ts-board)))

(defn scramble-board [ts-board]
  (let [all-poses (keys ts-board)]
    (reduce (fn [ts-board-acc pos]
              (scramble-tiles-at-pos pos ts-board-acc))
            ts-board
            all-poses)))

(defn generate-question [x-size y-size seed-count]
  (let [ts-solution (ts-make-tileset (generate-solution x-size y-size seed-count))]
    (scramble-board ts-solution)))

;; ;;;; ================== rendering ======================

(def line-width 5)

(def default-stroke (BasicStroke. line-width))

(defmulti draw-wire
  (fn [g connections start-x start-y tile-size]
    connections))

(defmethod draw-wire #{:east :west}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               start-x (+ start-y half)
               (+ start-x tile-size) (+ start-y half))))

(defmethod draw-wire #{:north :south}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               (+ start-x half) start-y
               (+ start-x half) (+ start-y tile-size))))

(defmethod draw-wire #{:west :south}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawArc g
              (- start-x half) (+ start-y half)
              tile-size tile-size
              0 90)))

(defmethod draw-wire #{:east :south}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawArc g
              (+ start-x half) (+ start-y half)
              tile-size tile-size
              90 90)))

(defmethod draw-wire #{:east :north}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawArc g
              (+ start-x half) (- start-y half)
              tile-size tile-size
              180 90)))

(defmethod draw-wire #{:west :north}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawArc g
              (- start-x half) (- start-y half)
              tile-size tile-size
              270 90)))

(defmethod draw-wire #{:west :east :south}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               start-x (+ start-y half)
               (+ start-x tile-size) (+ start-y half))
    (.drawLine g
               (+ start-x half) (+ start-y half)
               (+ start-x half) (+ start-y tile-size))))

(defmethod draw-wire #{:west :east :north}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               start-x (+ start-y half)
               (+ start-x tile-size) (+ start-y half))
    (.drawLine g
               (+ start-x half) start-y
               (+ start-x half) (+ start-y half))))

(defmethod draw-wire #{:south :east :north}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               (+ start-x half) (+ start-y half)
               (+ start-x tile-size) (+ start-y half))
    (.drawLine g
               (+ start-x half) start-y
               (+ start-x half) (+ start-y tile-size))))

(defmethod draw-wire #{:south :west :north}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)]
    (.drawLine g
               start-x (+ start-y half)
               (+ start-x half) (+ start-y half))
    (.drawLine g
               (+ start-x half) start-y
               (+ start-x half) (+ start-y tile-size))))

(defmethod draw-wire #{:south :west :north :east}
  [g connections start-x start-y tile-size]
  (let [half (/ tile-size 2)
        center-radius (/ tile-size 5)
        center-x (+ start-x half)
        center-y (+ start-y half)
        end-x (+ start-x tile-size)
        end-y (+ start-y tile-size)]
    (.drawOval g
               (- center-x center-radius) (- center-y center-radius)
               (* 2 center-radius) (* 2 center-radius))
    (.drawLine g
               start-x center-y
               (- center-x center-radius) center-y)
    (.drawLine g
               (+ center-x center-radius) center-y
               end-x center-y)
    (.drawLine g
               center-x start-y
               center-x (- center-y center-radius))
    (.drawLine g
               center-x (+ center-y center-radius)
               center-x end-y)))

(defn draw-lit-bulb [g palette-colour
                     start-x start-y tile-size]
  (let [bulb-size (/ tile-size 2.3)
        bulb-offset (/ (- tile-size bulb-size) 2)]
    (.setColor g palette-colour)
    (.fillOval g
               (+ start-x bulb-offset)
               (+ start-y bulb-offset)
               bulb-size
               bulb-size)))

(defn draw-unlit-bulb [g expected-palette-colour
                       start-x start-y tile-size]
  (let [bulb-size (/ tile-size 3)
        bulb-offset (/ (- tile-size bulb-size) 2)]
    (.setColor g (colour-rgbs :gray))
    (.fillOval g
               (+ start-x bulb-offset)
               (+ start-y bulb-offset)
               bulb-size
               bulb-size)
    (.setColor g expected-palette-colour)
    (.drawOval g
               (+ start-x bulb-offset)
               (+ start-y bulb-offset)
               bulb-size
               bulb-size)))

(defn draw-source-or-bulb-connections
  [g tile start-x start-y tile-size get-colour-for-con]
  (.setStroke g default-stroke)
  (let [half (/ tile-size 2)
        center-x (+ start-x half)
        center-y (+ start-y half)]
    (doseq [con (:connection tile)]
      (let [colour (get-colour-for-con con tile)
            palette-colour (colour-rgbs colour)
            pos-diff (directions con)
            con-dest (pos-add [center-x center-y]
                              (pos-scale-multiply pos-diff (/ tile-size 2)))
            [con-dest-x con-dest-y] con-dest]
        (.setColor g palette-colour)
        (.drawLine g
                   center-x center-y
                   con-dest-x con-dest-y)))))

(defmulti draw-specific-tile
  (fn [g tile start-x start-y tile-size]
    (:type tile)))

(defmethod draw-specific-tile :source
  [g tile start-x start-y tile-size]
  (draw-source-or-bulb-connections g tile start-x start-y tile-size
                                   (fn [dir tile]
                                     ((tile :subwires) dir)))
  (let [palette-colour (colour-rgbs (:colour tile))
        source-size (/ tile-size 3)
        source-start-x (+ start-x source-size)
        source-start-y (+ start-y source-size)]
    (.setColor g palette-colour)
    (.fillRect g
               source-start-x source-start-y
               source-size source-size)))

(defmethod draw-specific-tile :wire
  [g tile start-x start-y tile-size]
  (let [connections (into (hash-set) (:connection tile))
        palette-colour (colour-rgbs (:colour tile))]
    (.setColor g palette-colour)
    (.setStroke g default-stroke)
    (draw-wire g connections start-x start-y tile-size)))

(defmethod draw-specific-tile :twin-wire
  [g tile start-x start-y tile-size]
  (let [connections (into (hash-set) (:connection tile))
        palette-colour (colour-rgbs (:colour tile))]
    (.setColor g palette-colour)
    (.setStroke g default-stroke)
    (draw-wire g connections start-x start-y tile-size)))

(defmethod draw-specific-tile :bulb
  [g tile start-x start-y tile-size]
  (draw-source-or-bulb-connections g tile start-x start-y tile-size
                                   (fn [_ tile]
                                     (:colour tile)))
  (let [palette-colour (colour-rgbs (:colour tile))]
    (.setColor g palette-colour)
    (if (is-bulb-lit tile)
      (draw-lit-bulb g
                     palette-colour
                     start-x start-y tile-size)
      (draw-unlit-bulb g
                       (colour-rgbs (:expected-colour tile))
                       start-x start-y tile-size))))

(defn draw-tile-border [g start-x start-y tile-size]
  (.setStroke g (BasicStroke. 1))
  (.setColor g Color/WHITE)
  (.drawRect g start-x start-y tile-size tile-size))

(defn draw-tile [g tile tile-size]
  (let [[pos-x pos-y] (:pos tile)
        start-x (* pos-x tile-size)
        start-y (* pos-y tile-size)]
    (draw-tile-border g start-x start-y tile-size)
    (draw-specific-tile g tile start-x start-y tile-size)))

(defn draw-board [g board tile-size]
  (doseq [tile (sort-by :id (ts-flat-tile-list (dye-board board)))]
    (draw-tile g tile tile-size)))

(defn clear-screen [g max-x max-y]
  (.setBackground g (Color/BLACK))
  (.clearRect g 0 0 max-x max-y))

;;;; ================== mode render ======================

(defmulti mode-render 
  (fn [gs g]
    (:mode gs)))

(defmethod mode-render :playing [gs g]
  (let [config (:config gs)
        board (:board gs)
        {:keys [max-x max-y tile-size]} config]
    (clear-screen g max-x max-y)
    (draw-board g board tile-size)))

;;;; ================ mode input handler ================

(defn handle-mouse-click-and-rotate [gs x y]
  (let [tile-size (get-in gs [:config :tile-size])
        old-board (:board gs)
        pos-x (int (/ x tile-size))
        pos-y (int (/ y tile-size))
        pos [pos-x pos-y]
        new-board (ts-rotate-tiles-at-pos-multiple-times pos 1 old-board)]
    (assoc gs :board new-board)))

(defmulti mode-handle-input
  (fn [gs g]
    (:mode gs)))

(defmethod mode-handle-input :playing [gs input]
  (case (:type input)
    :mouse-clicked (let [info (:info input)]
                     (handle-mouse-click-and-rotate gs (.getX info) (.getY info)))
    gs))

;;;; ================ input handler =====================

(defn handle-one-input [gs input]
  (mode-handle-input gs input))

;;;; ================ engine interface ===================

(defn game-init [config]
  {:config config
   :mode :playing
   :board (:initial-board config)
   :fps -1})

(defn game-reset [gs]
  (-> gs
      (assoc :mode :playing)
      (assoc :board (generate-question 16 16 8))))

(defn game-handle-user-inputs [gs inputs]
  (reduce handle-one-input gs inputs))

(defn game-handle-tick [gs dt]
  gs)

(defn game-put-engine-info [gs engine-info]
  (assoc gs :fps (:fps engine-info)))

(defn game-render [gs g]
  (mode-render gs g))

(defn new-colourshift [config]
  (let [methods {:init game-init
                 :handle-user-inputs game-handle-user-inputs
                 :handle-tick game-handle-tick
                 :put-engine-info game-put-engine-info
                 :render game-render}]
    (new-game config methods)))

(def manual-test-board
  (ts-make-tileset
   [{:id 0 :pos [0 0] :type :source
     :connection [:east :south] :colour :red}
    {:id 1 :pos [1 1] :type :wire
     :connection [:west :east]}
    {:id 2 :pos [2 2] :type :wire
     :connection [:north :south]}
    {:id 3 :pos [2 1] :type :wire
     :connection [:west :south]}
    {:id 4 :pos [0 1] :type :wire
     :connection [:east :south]}
    {:id 5 :pos [0 3] :type :wire
     :connection [:east :north]}
    {:id 6 :pos [2 3] :type :wire
     :connection [:west :north]}
    {:id 7 :pos [0 2] :type :wire
     :connection [:north :south]}
    {:id 8 :pos [1 3] :type :wire
     :connection [:west :east]}
    {:id 9 :pos [4 1] :type :wire
     :connection [:west :east]}
    {:id 10 :pos [5 2] :type :wire
     :connection [:north :south]}
    {:id 11 :pos [5 1] :type :wire
     :connection [:west :south]}
    {:id 12 :pos [3 1] :type :wire
     :connection [:east :south]}
    {:id 13 :pos [3 3] :type :wire
     :connection [:east :north]}
    {:id 14 :pos [5 3] :type :wire
     :connection [:west :north]}
    {:id 15 :pos [3 2] :type :source
     :connection [:north] :colour :blue}
    {:id 16 :pos [4 3] :type :wire
     :connection [:west :east]}
    {:id 17 :pos [1 0] :type :bulb
     :connection [:west] :expected-colour :red}
    {:id 18 :pos [2 0] :type :bulb
     :connection [:east] :expected-colour :red}
    {:id 19 :pos [3 0] :type :source
     :connection [:west] :colour :green}

    {:id 20 :pos [0 4] :type :source
     :connection [:south] :colour :blue}
    {:id 21 :pos [1 4] :type :wire
     :connection [:west :east :south]}
    {:id 22 :pos [2 4] :type :bulb
     :connection [:west] :expected-colour :magenta}
    {:id 23 :pos [1 5] :type :wire
     :connection [:north :west :east]}
    {:id 24 :pos [0 5] :type :wire
     :connection [:north :south :east]}
    {:id 25 :pos [2 5] :type :wire
     :connection [:north :south :west]}
    {:id 26 :pos [4 4] :type :wire
     :connection [:west :east :south]}
    {:id 27 :pos [5 4] :type :bulb
     :connection [:west] :expected-colour :magenta}
    {:id 28 :pos [4 5] :type :wire
     :connection [:north :west :east]}
    {:id 29 :pos [3 5] :type :wire
     :connection [:north :south :east]}
    {:id 30 :pos [5 5] :type :wire
     :connection [:north :south :west]}
    {:id 31 :pos [0 6] :type :wire
     :connection [:north :south :east :west]}
    {:id 32 :pos [3 6] :type :wire
     :connection [:north :south :east :west]}
    {:id 33 :pos [1 6] :type :source
     :connection [:west] :colour :red}

    {:id 34 :pos [0 7] :type :twin-wire
     :connection [:north :south]}
    {:id 35 :pos [0 7] :type :twin-wire
     :connection [:west :east]}
    {:id 36 :pos [2 6] :type :wire
     :connection [:north :south]}
    {:id 37 :pos [2 7] :type :twin-wire
     :connection [:west :north]}
    {:id 38 :pos [2 7] :type :twin-wire
     :connection [:east :south]}
    {:id 39 :pos [1 7] :type :twin-wire
     :connection [:west :south]}
    {:id 40 :pos [1 7] :type :twin-wire
     :connection [:east :north]}
    ]))

(def random-solution-board (generate-question 16 16 8))

(defn benchmark []
  (let [board (generate-question 16 16 8)]
    (let [start-time (System/currentTimeMillis)]
      (doseq [_ (range 100)]
        (prn (count (find-connected-subgraphs board))))
      (let [finish-time (System/currentTimeMillis)]
        (prn [:elapsed (- finish-time start-time)])))))

(defn start []
  (main-loop (new-colourshift {:max-x 850
                               :max-y 850
                               :best-fps 60
                               :tile-size 50
                               :initial-board random-solution-board})))
