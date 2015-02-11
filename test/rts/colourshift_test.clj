(ns rts.colourshift-test
  (:require [clojure.test :refer :all]
            [rts.engine :refer :all])
  (:use rts.colourshift))

(def quick-check-times 1)

(defn get-all-non-reflexive-pairings [greater-than-or-equal-to-fn list]
  (mapcat (fn [t1]
            (map (fn [t2]
                   #{t1 t2})
                 (remove #(greater-than-or-equal-to-fn % t1) list)))
          list))

(defn ge-by-id [t1 t2]
  (>= (:id t1) (:id t2)))

(defn no-adjacent-pairs-in [tile-list]
  (let [pairs (get-all-non-reflexive-pairings ge-by-id tile-list)]
    (not-any? (fn [pair]
                (let [[t1 t2] (vec pair)]
                  (is-adjacent t1 t2)))
              pairs)))

(defn get-id-set [tile-list]
  (into (hash-set) (map :id tile-list)))

(defn has-cycle-recur [visited-path all-tiles]
  (if (< (count (get-id-set visited-path)) (count visited-path))
   true
   (let [seed (first visited-path)
         parent (second visited-path)
         neighbors (list-get-neighbors-by-connection seed all-tiles)
         unknown-neighbors (list-subtract-by-id neighbors [parent])]
     (some #(has-cycle-recur (cons % visited-path) all-tiles)
           unknown-neighbors))))

(defn has-cycle [subgraph]
  (has-cycle-recur [(first subgraph)] subgraph))

(defn generate-test-solution []
  (generate-solution 10 10 4))

(defn generate-test-question []
  (generate-question 10 10 4))

;; (defn all-single-ended-tiles-in-subgraph-is-multi-shared [subgraph whole-board]
;;   (let [single-ended-tiles (filter is-single-ended subgraph)
;;         single-ended-poses (distinct (map :pos single-ended-tiles))]
;;     (every? (fn [pos]
;;               (let [tiles-on-pos (filter #(= pos (:pos %)) whole-board)]
;;                 (> (count tiles-on-pos) 1)))
;;             single-ended-poses)))

(defn ts-all-single-ended-tiles-in-subgraph-is-multi-shared [subgraph whole-board]
  (let [single-ended-tiles (ts-filter is-single-ended subgraph)
        single-ended-poses (distinct (map :pos single-ended-tiles))]
    (every? (fn [pos]
              (let [tiles-on-pos (get whole-board pos)]
                (> (count tiles-on-pos) 1)))
            single-ended-poses)))

(deftest ts-make-tileset-works-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] }
                {:id 1 :pos [1 0] :type :wire
                 :connection [:west :east]}
                {:id 2 :pos [0 0] :type :source
                 :connection [:west] :colour :red}])]
    (is (= (set (keys board)) #{[0 0] [1 0]}))
    (is (= (set (map :id (get board [0 0]))) #{0 2}))))

(deftest ts-find-by-id-works-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] }
                {:id 1 :pos [1 0] :type :wire
                 :connection [:west :east]}])
        tile-1 (ts-find-by-id 1 board)]
    (is (= [1 0] (:pos tile-1)))))

(deftest ts-map-works-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] }
                {:id 1 :pos [1 0] :type :wire
                 :connection [:west :east]}])
        mapped-board (ts-map (fn [t]
                               (assoc t :colour :red))
                             board)
        tile-0 (ts-find-by-id 0 mapped-board)
        tile-1 (ts-find-by-id 1 mapped-board)]
    (is (= :red (:colour tile-0)))
    (is (= :red (:colour tile-1)))))

(deftest ts-filter-returns-flat-list
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] :colour :red}
                {:id 1 :pos [1 0] :type :wire
                 :connection [:west :east] :colour :red}
                {:id 2 :pos [2 0] :type :wire
                 :connection [:west :east] :colour :blue}])
        filtered-board (ts-filter (fn [t]
                                    (= :red (:colour t)))
                                  board)]
    (is (= #{0 1} (set (map :id filtered-board))))))

(deftest ts-remove-by-id-works-well-when-pos-is-shared
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] :colour :red}
                {:id 1 :pos [0 0] :type :wire
                 :connection [:west] :colour :red}
                {:id 2 :pos [2 0] :type :wire
                 :connection [:west :east] :colour :blue}])
        tile-1 (ts-find-by-id 1 board)
        lesser-board (ts-remove-by-id board tile-1)]
    (is (= 1 (count (get lesser-board [0 0]))))
    (is (= 0 (:id (first (vec (get lesser-board [0 0]))))))))

(deftest ts-remove-by-id-works-well-when-pos-is-single-occupied
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] :colour :red}
                {:id 1 :pos [0 0] :type :wire
                 :connection [:west] :colour :red}
                {:id 2 :pos [2 0] :type :wire
                 :connection [:west :east] :colour :blue}])
        tile-2 (ts-find-by-id 2 board)
        lesser-board (ts-remove-by-id board tile-2)]
    (is (= 1 (count (keys lesser-board))))
    (is (= [0 0] (first (vec (keys lesser-board)))))))

(deftest ts-conj-works-well-when-pos-is-not-occupied-yet
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] :colour :red}])
        new-tile {:id 1 :pos [0 1] :type :bulb
                  :connection [:east] :colour :red}
        bigger-board (ts-conj board new-tile)]
    (is (= 1 (count (get bigger-board [0 1]))))
    (is (= 2 (count (keys bigger-board))))))

(deftest ts-conj-works-well-when-pos-is-to-be-shared
  (let [board (ts-make-tileset
               [{:id 0 :pos [0 0] :type :bulb
                 :connection [:east] :colour :red}])
        new-tile {:id 1 :pos [0 0] :type :bulb
                  :connection [:east] :colour :red}
        bigger-board (ts-conj board new-tile)
        added-tile (ts-find-by-id 1 bigger-board)]
    (is (= 2 (count (get bigger-board [0 0]))))
    (is (= added-tile new-tile))))

(deftest dye-one-source-one-bulb-one-bi-wire
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east] }
                    {:id 1 :pos [1 0] :type :wire
                     :connection [:west :east]}
                    {:id 2 :pos [2 0] :type :source
                     :connection [:west] :colour :red}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb-colour :red))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-one-bulb-one-bi-wire-wrong-direction
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                    :connection [:north] }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-two-bulbs-one-tri-wire
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :bulb
                    :connection [:north]}])
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (ts-find-by-id 0 dyed-board))
        bulb2-colour (:colour (ts-find-by-id 3 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :red))
    (is (= wire-colour :red))))

(deftest dye-two-sources-one-bulb-one-tri-wire-blending
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection [:north] :colour :blue}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))
        source1-colour (:colour (ts-find-by-id 2 dyed-board))
        source2-colour (:colour (ts-find-by-id 3 dyed-board))]
    (is (= bulb-colour :magenta))
    (is (= wire-colour :magenta))
    (is (= source1-colour :red))
    (is (= source2-colour :blue))))

(deftest dye-three-sources-one-bulb-one-quat-wire-blending
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection [:north] :colour :blue}
                   {:id 4 :pos [1 -1] :type :source
                    :connection [:south] :colour :green}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb-colour :white))
    (is (= wire-colour :white))))

(deftest dye-two-sources-two-bulbs-one-quat-wire-blending
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east]}
                    {:id 1 :pos [1 0] :type :wire
                     :connection [:west :east :south :north]}
                    {:id 2 :pos [2 0] :type :source
                     :connection [:west] :colour :red}
                    {:id 3 :pos [1 1] :type :source
                     :connection [:north] :colour :green}
                    {:id 4 :pos [1 -1] :type :bulb
                     :connection [:south]}])
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (ts-find-by-id 0 dyed-board))
        bulb2-colour (:colour (ts-find-by-id 4 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb1-colour :yellow))
    (is (= bulb2-colour :yellow))
    (is (= wire-colour :yellow))))

(deftest dye-bulbs-and-wire-with-no-colour
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east] }
                    {:id 1 :pos [1 0] :type :wire
                     :connection [:west :east]}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire-colour (:colour (ts-find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :gray))))

(deftest dye-two-sources-two-bulbs-one-twin-wire-expecting-two-colours
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east]}
                    {:id 1 :pos [1 0] :type :twin-wire
                     :connection [:west :east]}
                    {:id 2 :pos [1 0] :type :twin-wire
                     :connection [:north :south]}
                    {:id 3 :pos [2 0] :type :source
                     :connection [:west] :colour :red}
                    {:id 4 :pos [1 1] :type :source
                     :connection [:north] :colour :green}
                    {:id 5 :pos [1 -1] :type :bulb
                     :connection [:south]}])
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (ts-find-by-id 0 dyed-board))
        bulb2-colour (:colour (ts-find-by-id 5 dyed-board))
        wire1-colour (:colour (ts-find-by-id 1 dyed-board))
        wire2-colour (:colour (ts-find-by-id 2 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :green))
    (is (= wire1-colour :red))
    (is (= wire2-colour :green))))

(deftest dye-two-bulbs-one-source-one-twin-wire-partial-lit
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east]}
                    {:id 1 :pos [1 0] :type :twin-wire
                     :connection [:west :east]}
                    {:id 2 :pos [1 0] :type :twin-wire
                     :connection [:north :south]}
                    {:id 3 :pos [2 0] :type :source
                     :connection [:west] :colour :red}
                    {:id 5 :pos [1 -1] :type :bulb
                     :connection [:south]}])
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (ts-find-by-id 0 dyed-board))
        bulb2-colour (:colour (ts-find-by-id 5 dyed-board))
        wire1-colour (:colour (ts-find-by-id 1 dyed-board))
        wire2-colour (:colour (ts-find-by-id 2 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :gray))
    (is (= wire1-colour :red))
    (is (= wire2-colour :gray))))

(deftest dye-empty-twin-wires
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east]}
                    {:id 1 :pos [1 0] :type :twin-wire
                     :connection [:west :east]}
                    {:id 2 :pos [1 0] :type :twin-wire
                     :connection [:north :south]}])
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (ts-find-by-id 0 dyed-board))
        wire1-colour (:colour (ts-find-by-id 1 dyed-board))
        wire2-colour (:colour (ts-find-by-id 2 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire1-colour :gray))
    (is (= wire2-colour :gray))))

(deftest dye-issue-on-not-full-dyeing-for-multiple-dead-ends
  (let [raw-board (ts-make-tileset
                   [{:id 9 :pos [4 1] :type :wire
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
                     :connection [:west :east]}])
        dyed-board (dye-board raw-board)
        wire1-colour (:colour (ts-find-by-id 10 dyed-board))
        wire2-colour (:colour (ts-find-by-id 11 dyed-board))]
    (is (= wire1-colour :blue))
    (is (= wire2-colour :blue))))

(deftest finding-by-connection-handles-twin-wire-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [1 1] :type :wire
                 :connection [:east :west]}
                {:id 1 :pos [0 1] :type :twin-wire
                 :connection [:north :east]}
                {:id 2 :pos [0 1] :type :twin-wire
                 :connection [:south :west]}])
        from-tile (ts-find-by-id 0 board)]
    (is (= 1 (:id (ts-find-by-connection :west from-tile board))))))

;; active non-connection: from-tile does not have a connection to the
;; direction of the target tile.
(deftest finding-by-connection-handles-active-non-connection-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [1 1] :type :wire
                 :connection [:east :west]}
                {:id 1 :pos [1 0] :type :wire
                 :connection [:north :south]}])
        from-tile (ts-find-by-id 0 board)]
    (is (= nil (ts-find-by-connection :west from-tile board)))))

;; active non-connection: from-tile has a connection to the
;; direction of the target tile, but target tile does not have a
;; connection to the from tile.
(deftest finding-by-connection-handles-passive-non-connection-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [1 1] :type :wire
                 :connection [:east :west]}
                {:id 1 :pos [0 1] :type :wire
                 :connection [:north :south]}])
        from-tile (ts-find-by-id 0 board)]
    (is (= nil (ts-find-by-connection :west from-tile board)))))

(deftest finding-by-connection-handles-normal-connection-well
  (let [board (ts-make-tileset
               [{:id 0 :pos [1 1] :type :wire
                 :connection [:east :west]}
                {:id 1 :pos [0 1] :type :wire
                 :connection [:north :east]}])
        from-tile (ts-find-by-id 0 board)]
    (is (= 1 (:id (ts-find-by-connection :west from-tile board))))))


;;; A subwire should be of same colour of connected wire or bulb,
;;; if there is no such connected tile, the subwire should be of same
;;; colour as the source itself.
(deftest dyeing-subwires-of-a-source-behaves-well
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :source
                     :connection [:east :south] :colour :red}
                    {:id 1 :pos [1 0] :type :wire
                     :connection [:west :east]}
                    {:id 2 :pos [2 0] :type :source
                     :connection [:west] :colour :green}])
        dyed-board (dye-board raw-board)
        source-1-subwires (:subwires (ts-find-by-id 0 dyed-board))]
    (is (= source-1-subwires {:east :yellow :south :red}))))

(deftest check-getting-all-non-reflexive-pairings
  (is (= (into (hash-set) (get-all-non-reflexive-pairings >= [1 2 3 4]))
         #{#{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4}})))

(deftest pick-at-most-n-non-adjacent-tiles-on-cramped-subgraph
  (let [tile-list [{:id 0 :pos [0 0] :type :wire
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:east :west :south]}
                   {:id 2 :pos [2 0] :type :wire
                    :connection [:east :west]}
                   {:id 3 :pos [3 0] :type :wire
                    :connection [:west]}
                   {:id 4 :pos [1 1] :type :wire
                    :connection [:north]}]]
    (dotimes [_ quick-check-times]
      (let [picked-tiles (pick-at-most-n-non-adjacent-tiles tile-list 3)]
        (is (<= (count picked-tiles) 3))
        (is (no-adjacent-pairs-in picked-tiles))))))

(deftest correctly-assert-no-adjacent-pairs-positive
  (let [tile-list [{:id 0 :pos [0 0] :type :wire
                    :connection [:north]}
                   {:id 1 :pos [2 0] :type :wire
                    :connection [:west]}]]
    (is (no-adjacent-pairs-in tile-list))))

(deftest correctly-assert-no-adjacent-pairs-negative
  (let [tile-list [{:id 0 :pos [0 0] :type :wire
                    :connection [:north]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west]}]]
    (is (not (no-adjacent-pairs-in tile-list)))))

(deftest any-two-seeds-are-not-adjacent
  (dotimes [_ quick-check-times]
    (let [[seeds unoccupied] (generate-seeds 10 10 11)]
      (is (= 11 (count seeds)))
      (is (= 89 (count unoccupied)))
      (is (no-adjacent-pairs-in seeds)))))

(deftest one-spurt-growth-of-subgraph-normal
  (let [subgraph-before [{:id 0 :pos [0 0] :type :wire
                          :connection [:east]}
                         {:id 1 :pos [1 0] :type :wire
                          :connection [:west]}]
        proto-pool-before [{:id 2 :pos [0 1]}]
        [proto-pool-after subgraph-after]
        (subgraph-one-spurt-grow proto-pool-before subgraph-before)
        substantiated-tile (list-find-by-id 2 subgraph-after)
        seed-after-growth (list-find-by-id 0 subgraph-after)
        seed-after-growth-connection (into (hash-set) (:connection seed-after-growth))]
    (is (= (count subgraph-after) 3))
    (is (= substantiated-tile {:id 2 :pos [0 1]
                               :connection [:north]}))
    (is (= seed-after-growth-connection #{:east :south}))
    (is (empty? proto-pool-after))))

(deftest one-spurt-growth-of-subgraph-multi-options
  (let [subgraph-before [{:id 0 :pos [0 0] :type :wire
                          :connection [:east]}
                         {:id 1 :pos [1 0] :type :wire
                          :connection [:west]}]
        proto-pool-before [{:id 2 :pos [0 1]}
                          {:id 3 :pos [2 0]}]
        [proto-pool-after subgraph-after]
        (subgraph-one-spurt-grow proto-pool-before subgraph-before)
        subgraph-after-ids (get-id-set subgraph-after)]
    (is (or (= subgraph-after-ids #{0 1 2})
            (= subgraph-after-ids #{0 1 3})))
    (is (= (count proto-pool-after) 1))))
           
(deftest one-spurt-growth-of-subgraph-no-room-for-growth
  (let [subgraph-before [{:id 0 :pos [0 0] :type :wire
                          :connection [:east :south]}
                         {:id 1 :pos [1 0] :type :wire
                          :connection [:west :east]}]
        proto-pool-before [{:id 5 :pos [10 10]}]
        [proto-pool-after subgraph-after]
        (subgraph-one-spurt-grow proto-pool-before subgraph-before)
        subgraph-after-ids (get-id-set subgraph-after)]
    (is (= subgraph-after-ids #{0 1}))
    (is (= (count proto-pool-before) 1))))

(deftest there-is-expected-number-of-poses-in-generated-solution-board
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          distinct-ids (get-id-set solution)]
      (is (= (count distinct-ids) (count solution))) ;; no duplicate ids
      (is (= (count (distinct (map :pos solution))) 100)))))

(deftest all-connections-are-two-sided-in-generated-solution-board
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)]
      (doseq [tile solution]
        (let [connection (:connection tile)
              connected-neighbors (ts-get-neighbors-by-connection tile ts-solution)]
          (is (= (count connection)
                 (count (distinct connection))))
          (is (= (count connected-neighbors)
                 (count (distinct connection)))))))))

(deftest there-is-no-such-subgraph-that-has-only-one-tile
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (is (> (count subg) 1))))))

(deftest biggest-subgraph-is-at-least-twice-size-as-smallest-one
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)
          subgraph-sizes (sort (map count subgraphs))
          min-size (first subgraph-sizes)
          max-size (last subgraph-sizes)]
      (is (>= max-size (* 2 min-size))))))
  
(deftest correctly-detects-graph-cycle-positive
  (let [tile-list [{:id 0 :pos [0 0]
                   :connection [:east :south]}
                  {:id 1 :pos [1 0]
                   :connection [:west :south]}
                  {:id 2 :pos [0 1]
                   :connection [:east :north]}
                  {:id 3 :pos [1 1]
                   :connection [:west :north]}]]
    (is (has-cycle tile-list))))

(deftest correctly-detects-graph-cycle-negative
  (let [tile-list [{:id 0 :pos [0 0]
                   :connection [:east :south]}
                  {:id 1 :pos [1 0]
                   :connection [:west :south]}
                  {:id 2 :pos [0 1]
                   :connection [:east :north]}]]
    (is (not (has-cycle tile-list)))))

(deftest no-cycles-in-any-subgraphs-in-generated-solution-board
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (is (not (has-cycle (ts-flat-tile-list subg))))))))

(deftest single-ended-tile-must-be-bulb-or-source
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)]
      (doseq [tile solution]
        (when (= (count (:connection tile)) 1)
          (is (or (= (:type tile) :bulb)
                  (= (:type tile) :source))))))))

(deftest multiple-ended-tile-must-be-wire-or-twinwire
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)]
      (doseq [tile solution]
        (when (> (count (:connection tile)) 1)
          (is (or (= (:type tile) :wire)
                  (= (:type tile) :twin-wire))))))))

(deftest subgraph-colour-is-correct
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (let [bulbs (ts-filter is-bulb subg)
              subg-colour (blended-colour-of-subgraph subg)]
          (is (every? #(= (:expected-colour %) subg-colour) bulbs)))))))

(deftest no-duplicate-coloured-source-in-same-subgraph
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (let [sources (ts-filter is-source subg)
              source-colours (into (hash-set) (map :colour sources))]
          (is (= (count sources) (count source-colours))))))))

(deftest not-any-two-sources-are-directly-connected
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          sources (filter is-source solution)
          source-pairs (map vec (get-all-non-reflexive-pairings ge-by-id sources))]
      (doseq [[s1 s2] source-pairs]
        (is (not (is-connected s1 s2)))))))

(deftest there-is-at-least-one-source-in-each-subgraph
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (let [sources (ts-filter is-source subg)]
          (is (>= (count sources) 1)))))))

(deftest shared-poses-should-be-either-all-sources-or-all-twin-wires
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          all-poses (distinct (map :pos solution))
          shared-poses (filter (fn [pos]
                                 (let [tiles-on-pos (filter #(= pos (:pos %)) solution)]
                                   (> (count tiles-on-pos) 1)))
                               all-poses)]
      (doseq [pos shared-poses]
        (let [tiles-on-pos (filter #(= pos (:pos %)) solution)]
          (is (or (every? (fn [tile] (and (= :source (:type tile))
                                          (= 1 (count (:connection tile))))) tiles-on-pos)
                  (every? (fn [tile] (and (= :twin-wire (:type tile))
                                          (= 2 (count (:connection tile))))) tiles-on-pos))))))))

(deftest there-is-at-least-one-bulb-in-each-subgraph-if-possible
  (dotimes [_ quick-check-times]
    (let [solution (generate-test-solution)
          ts-solution (ts-make-tileset solution)
          subgraphs (find-connected-subgraphs-default-setting ts-solution)]
      (doseq [subg subgraphs]
        (when-not (ts-all-single-ended-tiles-in-subgraph-is-multi-shared subg ts-solution)
          (let [bulbs (ts-filter is-bulb subg)]
            (is (>= (count bulbs) 1))))))))

(deftest single-tile-can-rotate-once-properly
  (let [tile {:id 0
              :type :wire
              :connection [:south :east]}
        rotated-tile (rotate-tile-once tile)]
    (is (= (into (hash-set) (:connection rotated-tile))
           #{:west :south}))
    (is (= :wire (:type rotated-tile)))))

(deftest single-tile-can-rotate-multiple-times-property
  (let [tile {:id 0
              :type :wire
              :connection [:south :east]}
        rotated-tile (rotate-tile-multiple-times tile 2)]
    (is (= (into (hash-set) (:connection rotated-tile))
           #{:north :west}))
    (is (= :wire (:type rotated-tile)))))

(deftest multiple-tiles-can-rotate-synchronizedly
  (let [tile-1 {:id 0
                :pos [0 0]
                :type :wire
                :connection [:south]}
        tile-2 {:id 1
                :pos [0 0]
                :type :wire
                :connection [:west]}
        tile-3 {:id 2
                :pos [0 1]
                :type :wire
                :connection [:east :north]}
        subgraph (ts-make-tileset [tile-1 tile-2 tile-3])
        rotated-ts (ts-rotate-tiles-at-pos-multiple-times [0 0] 3 subgraph)]
    (is (= (into (hash-set) (map #(into (hash-set) (:connection %)) (ts-flat-tile-list rotated-ts)))
           #{#{:south} #{:east} #{:east :north}}))))

(deftest question-board-has-enough-tiles
  (dotimes [_ quick-check-times]
    (let [question (ts-flat-tile-list (generate-test-question))
          all-poses (distinct (map :pos question))]
      (is (= (count all-poses) 100)))))

(deftest tiles-on-same-pos-should-not-have-intersected-connections-in-question-board
  (dotimes [_ quick-check-times]
    (let [question (ts-flat-tile-list (generate-test-question))
          all-poses (distinct (map :pos question))]
      (doseq [pos all-poses]
        (let [tiles (get-tiles-on-pos pos question)
              all-connections (apply concat (map :connection tiles))]
          (is (= (count all-connections)
                 (count (distinct all-connections)))))))))

(deftest tiles-on-same-pos-should-have-same-type-in-question-board
  (dotimes [_ quick-check-times]
    (let [question (ts-flat-tile-list (generate-test-question))
          all-poses (distinct (map :pos question))]
      (doseq [pos all-poses]
        (let [tiles (get-tiles-on-pos pos question)
              all-types (distinct (map :type tiles))]
          (is (= (count all-types) 1))
          (when (> (count tiles) 1)
            (is (contains? #{:source :twin-wire} (first all-types)))))))))

(deftest issue-directly-connected-sources-have-their-subwires-dyed-correctly
  (let [tileset (ts-make-tileset
                 [{:id 0 :pos [0 0] :type :source
                   :connection [:east] :colour :red}
                  {:id 1 :pos [1 0] :type :source
                   :connection [:west] :colour :green}])
        subwires-of-tile-0 (dye-subwires-of-a-source 
                            (ts-find-by-id 0 tileset)
                            tileset)]
    (is (= subwires-of-tile-0 {:east :yellow}))))

(deftest victory-check-negative
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east] :expected-colour :red}
                    {:id 1 :pos [0 1] :type :source
                     :connection [:west :east] :colour :red}
                    {:id 2 :pos [0 2] :type :bulb
                     :connection [:west] :expected-colour :blue}])
        dyed-board (dye-board raw-board)]
    (is (not (victory? dyed-board)))))

(deftest victory-check-positive
  (let [raw-board (ts-make-tileset
                   [{:id 0 :pos [0 0] :type :bulb
                     :connection [:east] :expected-colour :red}
                    {:id 1 :pos [1 0] :type :source
                     :connection [:west :east] :colour :red}
                    {:id 2 :pos [2 0] :type :bulb
                     :connection [:west] :expected-colour :red}])
        dyed-board (dye-board raw-board)]
    (is (victory? dyed-board))))

(deftest message-queue-works-well
  (queue-register :test-handler
                  (fn [gs message]
                    (assoc gs :q-info (:info message))))
  (queue-send :test-handler {:info "test-q"})
  (let [gs {:already-has "some content"}
        changed-gs (queue-poll-once gs)]
    (is (= changed-gs
           {:already-has "some content"
            :q-info "test-q"}))))
                  
(deftest message-queue-can-handle-recurred-poll
  (queue-register :recurring
                  (fn [gs message]
                    (queue-send :next {:info 2})
                    (update-in gs [:handled] conj (:info message))))
  (queue-register :next
                  (fn [gs message]
                    (update-in gs [:handled] conj (:info message))))
  (queue-send :recurring {:info 1})
  (let [gs {:handled [0]}
        changed-gs (queue-poll gs)]
    (is (= changed-gs
           {:handled [0 1 2]}))))

(deftest message-queue-broadcasts-same-message-to-multiple-handlers
  (queue-register :broadcast
                  (fn [gs message]
                    (assoc gs :handled-1 (:info message))))
  (queue-register :broadcast
                  (fn [gs message]
                    (assoc gs :handled-2 (:info message))))
  (queue-send :broadcast {:info "x"})
  (let [gs {:already-has "some content"}
        changed-gs (queue-poll gs)]
    (is (= changed-gs
           {:already-has "some content"
            :handled-1 "x"
            :handled-2 "x"}))))
