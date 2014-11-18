(ns rts.colourshift-test
  (:require [clojure.test :refer :all]
            [rts.engine :refer :all])
  (:use rts.colourshift))

(deftest dye-one-source-one-bulb-one-bi-wire
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :red))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-one-bulb-one-bi-wire-wrong-direction
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :north }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-two-bulbs-one-tri-wire
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 3 :pos [1 1] :type :bulb
                    :connection :north}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 3 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :red))
    (is (= wire-colour :red))))

(deftest dye-two-sources-one-bulb-one-tri-wire-blending
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection :north :colour :blue}]
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
                    :connection :east}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection :north :colour :blue}
                   {:id 4 :pos [1 -1] :type :source
                    :connection :south :colour :green}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :white))
    (is (= wire-colour :white))))

(deftest dye-two-sources-two-bulbs-one-quat-wire-blending
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east}
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:id 2 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 3 :pos [1 1] :type :source
                    :connection :north :colour :green}
                   {:id 4 :pos [1 -1] :type :bulb
                    :connection :south}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-id 0 dyed-board))
        bulb2-colour (:colour (find-by-id 4 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb1-colour :yellow))
    (is (= bulb2-colour :yellow))
    (is (= wire-colour :yellow))))

(deftest dye-bulbs-and-wire-with-no-colour
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east }
                   {:id 1 :pos [1 0] :type :wire
                    :connection [:west :east]}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-id 0 dyed-board))
        wire-colour (:colour (find-by-id 1 dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :gray))))

(deftest dye-two-sources-two-bulbs-one-twin-wire-expecting-two-colours
  (let [raw-board [{:id 0 :pos [0 0] :type :bulb
                    :connection :east}
                   {:id 1 :pos [1 0] :type :twin-wire
                    :connection [:west :east]}
                   {:id 2 :pos [1 0] :type :twin-wire
                    :connection [:north :south]}
                   {:id 3 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 4 :pos [1 1] :type :source
                    :connection :north :colour :green}
                   {:id 5 :pos [1 -1] :type :bulb
                    :connection :south}]
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
                    :connection :east}
                   {:id 1 :pos [1 0] :type :twin-wire
                    :connection [:west :east]}
                   {:id 2 :pos [1 0] :type :twin-wire
                    :connection [:north :south]}
                   {:id 3 :pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:id 5 :pos [1 -1] :type :bulb
                    :connection :south}]
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
                    :connection :east}
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
