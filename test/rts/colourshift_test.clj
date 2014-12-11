(ns rts.colourshift-test
  (:require [clojure.test :refer :all]
            [rts.engine :refer :all])
  (:use rts.colourshift))

(def quick-check-times 100)

(defn get-all-non-reflexive-pairings [greater-than-or-equal-to-fn list]
  (mapcat (fn [t1]
            (map (fn [t2]
                   #{t1 t2})
                 (remove #(greater-than-or-equal-to-fn % t1) list)))
          list))

(defn no-adjacent-pairs-in [tile-list]
  (let [ge-by-id (fn [t1 t2]
                   (>= (:id t1) (:id t2)))
        pairs (get-all-non-reflexive-pairings ge-by-id tile-list)]
    (not-any? (fn [pair]
                (let [[t1 t2] (vec pair)]
                  (is-adjacent t1 t2)))
              pairs)))

(deftest dye-one-source-one-bulb-one-bi-wire
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east] }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :red))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-one-bulb-one-bi-wire-wrong-direction
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:north] }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-two-bulbs-one-tri-wire
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :bulb
                    :connection [:north]}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 3 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :red))
    (is (= wire-colour :red))))

(deftest dye-two-sources-one-bulb-one-tri-wire-blending
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection [:north] :colour :blue}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))
        source1-colour (:colour (find-by-id 2 dyed-board))
        source2-colour (:colour (find-by-id 3 dyed-board))]
    (is (= bulb-colour :magenta))
    (is (= wire-colour :magenta))
    (is (= source1-colour :red))
    (is (= source2-colour :blue))))

(deftest dye-three-sources-one-bulb-one-quat-wire-blending
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection [:north] :colour :blue}
                   {:id 4 :pos [1 -1] :type :source
                    :connection [:south] :colour :green}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :white))
    (is (= wire-colour :white))))

(deftest dye-two-sources-two-bulbs-one-quat-wire-blending
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection [:north] :colour :green}
                   {:id 4 :pos [1 -1] :type :bulb
                    :connection [:south]}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 4 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb1-colour :yellow))
    (is (= bulb2-colour :yellow))
    (is (= wire-colour :yellow))))

(deftest dye-bulbs-and-wire-with-no-colour
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east] }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :gray))))

(deftest dye-two-sources-two-bulbs-one-twin-wire-expecting-two-colours
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
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
                    :connection [:south]}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 5 dyed-board))
        wire1-colour (:colour (find-by-id 1 dyed-board))
        wire2-colour (:colour (find-by-id 2 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :green))
    (is (= wire1-colour :red))
    (is (= wire2-colour :green))))

(deftest dye-two-bulbs-one-source-one-twin-wire-partial-lit
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :twin-wire
                    :connection [:west :east]}
                   {:id 2 :pos [1 0] :type :twin-wire
                    :connection [:north :south]}
                   {:id 3 :pos [2 0] :type :source
                    :connection [:west] :colour :red}
                   {:id 5 :pos [1 -1] :type :bulb
                    :connection [:south]}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 5 dyed-board))
        wire1-colour (:colour (find-by-id 1 dyed-board))
        wire2-colour (:colour (find-by-id 2 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :gray))
    (is (= wire1-colour :red))
    (is (= wire2-colour :gray))))

(deftest dye-empty-twin-wires
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection [:east]}
                   {:id 1 :pos [1 0] :type :twin-wire
                    :connection [:west :east]}
                   {:id 2 :pos [1 0] :type :twin-wire
                    :connection [:north :south]}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire1-colour (:colour (find-by-id 1 dyed-board))
        wire2-colour (:colour (find-by-id 2 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire1-colour :gray))
    (is (= wire2-colour :gray))))

(deftest dye-issue-on-not-full-dyeing-for-multiple-dead-ends
  (let [raw-board [{:id 9 :pos [4 1] :type :wire
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
                    :connection [:west :east]}]
        dyed-board (dye-board raw-board)
        wire1-colour (:colour (find-by-id 10 dyed-board))
        wire2-colour (:colour (find-by-id 11 dyed-board))]
    (is (= wire1-colour :blue))
    (is (= wire2-colour :blue))))

(deftest finding-by-connection-handles-twin-wire-well
  (let [board [{:id 0 :pos [1 1] :type :wire
                :connection [:east :west]}
               {:id 1 :pos [0 1] :type :twin-wire
                :connection [:north :east]}
               {:id 2 :pos [0 1] :type :twin-wire
                :connection [:south :west]}]
        from-tile (find-by-id 0 board)]
    (is (= 1 (:id (find-by-connection :west from-tile board))))))

;; active non-connection: from-tile does not have a connection to the
;; direction of the target tile.
(deftest finding-by-connection-handles-active-non-connection-well
  (let [board [{:id 0 :pos [1 1] :type :wire
                :connection [:east :west]}
               {:id 1 :pos [1 0] :type :wire
                :connection [:north :south]}]
        from-tile (find-by-id 0 board)]
    (is (= nil (find-by-connection :west from-tile board)))))

;; active non-connection: from-tile havs a connection to the
;; direction of the target tile, but target tile does not have a
;; connection to the from tile.
(deftest finding-by-connection-handles-passive-non-connection-well
  (let [board [{:id 0 :pos [1 1] :type :wire
                :connection [:east :west]}
               {:id 1 :pos [0 1] :type :wire
                :connection [:north :south]}]
        from-tile (find-by-id 0 board)]
    (is (= nil (find-by-connection :west from-tile board)))))

(deftest finding-by-connection-handles-normal-connection-well
  (let [board [{:id 0 :pos [1 1] :type :wire
                :connection [:east :west]}
               {:id 1 :pos [0 1] :type :wire
                :connection [:north :east]}]
        from-tile (find-by-id 0 board)]
    (is (= 1 (:id (find-by-connection :west from-tile board))))))


;;; A subwire should be of same colour of connected wire or bulb,
;;; if there is no such connected tile, the subwire should be of same
;;; colour as the source itself.
(deftest dyeing-subwires-of-a-source-behaves-well
  (let [raw-board [{:id 0 :pos [0 0] :type :source
                    :connection [:east :south] :colour :red}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection [:west] :colour :green}]
        dyed-board (dye-board raw-board)
        source-1-subwires (:subwires (find-by-id 0 dyed-board))]
    (is (= source-1-subwires {:east :yellow :south :red}))))

(deftest check-getting-all-non-reflexive-pairings
  (is (= (into (hash-set) (get-all-non-reflexive-pairings >= [1 2 3 4]))
         #{#{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4}})))

(deftest pick-at-most-n-non-adjacent-tiles-on-cramped-subgraph
  (let [subgraph [{:id 0 :pos [0 0] :type :wire
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
      (let [picked-tiles (pick-at-most-n-non-adjacent-tiles subgraph 3)]
        (is (<= (count picked-tiles) 3))
        (is (no-adjacent-pairs-in picked-tiles))))))

(deftest correctly-assert-no-adjacent-pairs-positive
  (let [tile-set [{:id 0 :pos [0 0] :type :wire
                   :connection [:north]}
                  {:id 1 :pos [2 0] :type :wire
                   :connection [:west]}]]
    (is (no-adjacent-pairs-in tile-set))))

(deftest correctly-assert-no-adjacent-pairs-negative
  (let [tile-set [{:id 0 :pos [0 0] :type :wire
                   :connection [:north]}
                  {:id 1 :pos [1 0] :type :wire
                   :connection [:west]}]]
    (is (not (no-adjacent-pairs-in tile-set)))))

(deftest any-two-seeds-are-not-adjacent
  (dotimes [_ quick-check-times]
    (let [[seeds unoccupied] (generate-seeds 10 10 9 8)]
      (is (= 26 (count seeds))) ;; 9 * 2 + 8
      (is (= 83 (count unoccupied))) ;; 10 * 10 - 9 - 8
      (is (no-adjacent-pairs-in seeds)))))
