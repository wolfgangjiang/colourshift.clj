(ns rts.colourshift
  (:use rts.engine)
  (:import [java.awt BasicStroke Color]))

;;;; ================ dyeing ===================

(def directions {:east [1 0]
                 :west [-1 0]
                 :north [0 -1]
                 :south [0 1]})

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

(defn find-by-id [id tile-list]
  (first (filter #(= id (:id %)) tile-list)))

(defn subtract-by-id [total unwanted]
  (let [unwanted-pos-set (into (hash-set) (map :id unwanted))
        remaining (remove #(contains? unwanted-pos-set (:id %)) total)]
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

(defn get-neighbors-by-connection [tile tile-list]
  (let [connected-tiles (map #(find-by-connection % tile tile-list) (keys directions))]
    (remove nil? connected-tiles)))

(defn breadth-first-traverse [start-tile all-tiles get-neighbors-of subtract]
  (let [breadth-first-recur
        (fn [open-list closed-list raw-list]
          (if (empty? open-list)
            closed-list
            (let [seed (first open-list)
                  raw-neighbors (get-neighbors-of seed raw-list)
                  unvisited-neighbors (subtract raw-neighbors (concat open-list closed-list))
                  new-open-list (concat unvisited-neighbors (rest open-list))
                  new-closed-list (conj closed-list seed)
                  new-raw-list (subtract raw-list unvisited-neighbors)]
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
                  unused-tiles (subtract remaining-tiles new-subgraph)]
              (recur unused-tiles (conj subgraphs new-subgraph)))))]
    (find-connected-subgraphs-recur all-tiles [])))

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
  (let [subgraphs (find-connected-subgraphs board
                                            get-neighbors-by-connection
                                            subtract-by-id)
        dyed-subgraphs (map dye-subgraph subgraphs)]
    (apply concat dyed-subgraphs)))

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
  (doseq [tile (dye-board board)]
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
   ])

(defn start []
  (main-loop (new-colourshift {:max-x 500
                               :max-y 500
                               :best-fps 60
                               :tile-size 50
                               :initial-board manual-test-board})))
