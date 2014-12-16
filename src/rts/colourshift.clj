(ns rts.colourshift
  (:use rts.engine)
  (:import [java.awt BasicStroke Color]))

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

(defn find-by-id [id tile-list]
  (first (filter #(= id (:id %)) tile-list)))

;;; CAUTION: incorrect in the face of twin-wires
(defn find-by-pos [pos tile-list]
  (first (filter #(= pos (:pos %)) tile-list)))

(defn subtract-by-id [total unwanted]
  (let [unwanted-id-set (into (hash-set) (map :id unwanted))
        remaining (remove #(contains? unwanted-id-set (:id %)) total)]
    remaining))

(defn find-by-connection [dir tile tile-list]
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

(defn is-connected [t1 t2]
  (some #(not (nil? (find-by-connection % t1 [t2])))
        (:connection t1)))

(defn is-adjacent [t1 t2]
  (let [dpos (pos-diff (:pos t1) (:pos t2))]
    (contains? direction-names dpos)))

(defn get-neighbors-by-connection [tile tile-list]
  (let [connected-tiles (map #(find-by-connection % tile tile-list) (keys directions))]
    (remove nil? connected-tiles)))

(defn get-pos-on-dir [origin-pos dir]
  (let [pos-diff (directions dir)
        pos-on-dir (pos-add origin-pos pos-diff)]
    pos-on-dir))

(defn breadth-first-traverse [start-tile all-tiles get-neighbors-of subtract]
  (let [breadth-first-recur
        (fn [open-list closed-list raw-list]
          (if (empty? open-list)
            closed-list
            (let [seed (first open-list)
                  raw-neighbors (get-neighbors-of seed raw-list)
                  unvisited-neighbors (subtract raw-neighbors (concat open-list closed-list))
                  new-open-list (concat (rest open-list) unvisited-neighbors)
                  new-closed-list (conj closed-list seed)
                  new-raw-list (subtract raw-list unvisited-neighbors)]
              (recur new-open-list new-closed-list new-raw-list))))
        unused-tiles (subtract all-tiles [start-tile])]
    (breadth-first-recur [start-tile] [] unused-tiles)))

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
                  unused-tiles (subtract remaining-tiles new-subgraph)]
              (recur unused-tiles (conj subgraphs new-subgraph)))))]
    (find-connected-subgraphs-recur all-tiles [])))

(defn find-connected-subgraphs-default-setting [all-tiles]
  (find-connected-subgraphs all-tiles
                            get-neighbors-by-connection
                            subtract-by-id))
  

(defn dye-subwires-of-a-source [source-tile tile-list]
  (let [subwire-colour-pairs (map (fn [dir]
                                    (let [neighbor-on-dir (find-by-connection dir source-tile tile-list)
                                          subwire-colour (if neighbor-on-dir
                                                           (:colour neighbor-on-dir)
                                                           (:colour source-tile))]
                                      [dir subwire-colour]))
                                  (:connection source-tile))
        valid-pairs (remove #(nil? (second %)) subwire-colour-pairs)]
    (into (hash-map) valid-pairs)))

(defn dye-subgraph-wires-and-bulbs [tile-list]
  (let [sources (filter #(= :source (:type %)) tile-list)
        colours (into (hash-set) (map :colour sources))
        merged-colour (get blendings colours)]
    (map (fn [tile]
           (if (= :source (:type tile))
             tile
             (assoc tile :colour merged-colour)))
         tile-list)))  

(defn dye-subgraph-source-subwires [tile-list]
  (map (fn [tile]
         (if (= :source (:type tile))
           (assoc tile :subwires (dye-subwires-of-a-source tile tile-list))
           tile))
       tile-list))

;;; implemented in two passes, to ensure that all neighbors are
;;; properly dyed when dyeing subwires of a source.
(defn dye-subgraph [tile-list]
  (-> tile-list
      dye-subgraph-wires-and-bulbs
      dye-subgraph-source-subwires))

(defn dye-board [board]
  (let [subgraphs (find-connected-subgraphs-default-setting board)
        dyed-subgraphs (map dye-subgraph subgraphs)]
    (apply concat dyed-subgraphs)))

;;;; ============== generation =========================

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

(defn random-normal-wire-connection []
  (let [options [[:north :south]
                 [:north :east]
                 [:north :west]
                 [:east :west]
                 [:east :south]
                 [:west :south]
                 [:north :south :east]
                 [:north :south :west]
                 [:east :west :north]
                 [:east :west :south]
                 [:north :south :east :west]]]
    (rand-nth options)))

(defn new-counter []
  (let [n (atom 0)]
    (fn []
      (swap! n inc)
      @n)))

(defn generate-empty-board [x-size y-size generate-id]
  (mapcat (fn [x]
            (map (fn [y]
                   {:id (generate-id)
                    :pos [x y]})
                 (range y-size)))
          (range x-size)))

(defn generate-seeds [x-size y-size twinwire-count normal-wire-count]
  (let [generate-id (new-counter)
        all-tiles (generate-empty-board x-size y-size generate-id)
        seed-count (+ twinwire-count normal-wire-count)
        picked (pick-at-most-n-non-adjacent-tiles all-tiles seed-count)
        twinwire-tiles (mapcat (fn [proto-tile]
                                 (let [[con1 con2] (random-twinwire-connection)]
                                   [{:id (:id proto-tile)
                                     :pos (:pos proto-tile)
                                     :type :twin-wire
                                     :connection con1}
                                    {:id (generate-id)
                                     :pos (:pos proto-tile)
                                     :type :twin-wire
                                     :connection con2}]))
                               (take twinwire-count picked))
        normal-wire-tiles (map (fn [proto-tile]
                                 {:id (:id proto-tile)
                                  :pos (:pos proto-tile)
                                  :type :wire
                                  :connection (random-normal-wire-connection)})
                               (drop twinwire-count picked))
        seeds (concat twinwire-tiles normal-wire-tiles)
        unoccupied (subtract-by-id all-tiles seeds)]
    [seeds unoccupied]))

(defn get-candidate-pairs-for-growth [subgraph proto-tiles]
  (let [all-pairs (mapcat (fn [sg-tile]
                            (map (fn [p-tile]
                                   [sg-tile p-tile])
                                 proto-tiles))
                          subgraph)]
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
        subgraph-without-seed (subtract-by-id subgraph [seed])
        new-subgraph (concat subgraph-without-seed [grown-seed substantiated-tile])
        remaining-proto-pool (subtract-by-id proto-pool [picked-proto-tile])]
    [remaining-proto-pool new-subgraph]))

(defn subgraph-one-spurt-grow [proto-pool subgraph]
  (let [[seed picked-proto-tile] (pick-for-growth subgraph proto-pool)]
    (if (nil? picked-proto-tile)
      [proto-pool subgraph]
      (subgraph-grow-with-seed-and-picked proto-pool subgraph seed picked-proto-tile))))

(defn initialize-subgraph-with-required-poses [proto-pool seed required-poses]
  ;; there is no twin-wires in proto-pool, so find-by-pos is safe here.
  (reduce (fn [[proto-pool subgraph] new-pos]
            (let [new-tile (find-by-pos new-pos proto-pool)]
              (subgraph-grow-with-seed-and-picked proto-pool subgraph seed new-tile)))
          [proto-pool [seed]]
          required-poses))

(defn give-up-subgraph [proto-pool seed]
  ;; When one of a twin-wire tile is given-up, the other must be
  ;; given-up as well.
  (let [seed-as-proto-tile {:id (:id seed)
                            :pos (:pos seed)}
        new-proto-pool (conj proto-pool seed-as-proto-tile)]
    [new-proto-pool []]))

(defn initialize-subgraph [proto-pool seed]
  (let [proto-pool-pos-set (into (hash-set) (map :pos proto-pool))
        required-poses (map #(get-pos-on-dir (:pos seed) %)
                            (:connection seed))]
    (if (every? #(contains? proto-pool-pos-set %) required-poses)
      (initialize-subgraph-with-required-poses proto-pool seed required-poses)
      (give-up-subgraph proto-pool seed))))

(defn initialize-subgraph-list-recur [proto-pool subgraphs seed-list]
  (if (empty? seed-list)
    [proto-pool subgraphs]
    (let [seed (first seed-list)
          [remaining-proto-pool subg] (initialize-subgraph proto-pool seed)]
      (recur remaining-proto-pool (conj subgraphs subg) (rest seed-list)))))

(defn initialize-subgraph-list [proto-pool seed-list]
  (initialize-subgraph-list-recur proto-pool [] seed-list))

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
    (if (< destiny-trial destiny-number)
      [proto-pool subgraph-info]
      (grow-subgraph-info proto-pool subgraph-info))))

(defn generate-solution-subgraph-recur [proto-pool new-subgraph-infos old-subgraph-infos]
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
          (generate-solution-subgraph-recur proto-pool [] subgraph-info-list)]
      (recur remaining-proto-pool new-subgraph-info-list))))

(defn generate-solution [x-size y-size twinwire-count normal-wire-count]
  (let [[seed-list initial-proto-pool]
        (generate-seeds x-size y-size twinwire-count normal-wire-count)
        [second-proto-pool subgraph-list]
        (initialize-subgraph-list initial-proto-pool seed-list)
        subgraph-info-list (map (fn [subg]
                                  {:destiny-number (rand)
                                   :subgraph-tiles subg})
                                subgraph-list)
        mature-subgraph-info-list (generate-solution-recur second-proto-pool
                                                           subgraph-info-list)
        mature-subgraph-list (map :subgraph-tiles mature-subgraph-info-list)]
    (apply concat mature-subgraph-list)))

;;;; ================== rendering ======================

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
    (if (= (:colour tile) (:expected-colour tile))
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
  (doseq [tile (sort-by :id (dye-board board))]
    (draw-tile g tile tile-size)))

(defn clear-screen [g max-x max-y]
  (.setBackground g (Color/BLACK))
  (.clearRect g 0 0 max-x max-y))

;;;; ================ engine interface ===================

(defn game-init [config]
  {:config config
   :board (:initial-board config)
   :fps -1})

(defn game-handle-user-inputs [gs inputs]
  gs)

(defn game-handle-tick [gs dt]
  gs)

(defn game-put-engine-info [gs engine-info]
  (assoc gs :fps (:fps engine-info)))

(defn game-render [gs g]
  (let [config (:config gs)
        board (:board gs)
        {:keys [max-x max-y tile-size]} config]
    (clear-screen g max-x max-y)
    (draw-board g board tile-size)))

(defn new-colourshift [config]
  (let [methods {:init game-init
                 :handle-user-inputs game-handle-user-inputs
                 :handle-tick game-handle-tick
                 :put-engine-info game-put-engine-info
                 :render game-render}]
    (new-game config methods)))

(def manual-test-board
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
   ])

(def random-solution-board (generate-solution 10 10 9 8))

(defn start []
  (main-loop (new-colourshift {:max-x 500
                               :max-y 500
                               :best-fps 60
                               :tile-size 50
                               :initial-board random-solution-board})))
