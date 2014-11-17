(ns rts.colourshift-test
  (:require [clojure.test :refer :all]
            [rts.engine :refer :all])
  (:use rts.colourshift))

(deftest dye-one-source-one-bulb-one-bi-wire
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east }
                   {:pos [1 0] :type :wire
                    :connection [:west :east]}
                   {:pos [2 0] :type :source
                    :connection :west :colour :red}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-pos [0 0] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb-colour :red))
    (is (= wire-colour :red)))) 

(deftest dye-one-source-two-bulbs-one-tri-wire
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east}
                   {:pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:pos [1 1] :type :bulb
                    :connection :north}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-pos [0 0] dyed-board))
        bulb2-colour (:colour (find-by-pos [1 1] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb1-colour :red))
    (is (= bulb2-colour :red))
    (is (= wire-colour :red))))

(deftest dye-two-sources-one-bulb-one-tri-wire-blending
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east}
                   {:pos [1 0] :type :wire
                    :connection [:west :east :south]}
                   {:pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:pos [1 1] :type :source
                    :connection :north :colour :blue}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-pos [0 0] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb-colour :magenta))
    (is (= wire-colour :magenta))))

(deftest dye-three-sources-one-bulb-one-quat-wire-blending
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east}
                   {:pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:pos [1 1] :type :source
                    :connection :north :colour :blue}
                   {:pos [1 -1] :type :source
                    :connection :south :colour :green}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-pos [0 0] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb-colour :white))
    (is (= wire-colour :white))))

(deftest dye-two-sources-two-bulbs-one-quat-wire-blending
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east}
                   {:pos [1 0] :type :wire
                    :connection [:west :east :south :north]}
                   {:pos [2 0] :type :source
                    :connection :west :colour :red}
                   {:pos [1 1] :type :source
                    :connection :north :colour :green}
                   {:pos [1 -1] :type :bulb
                    :connection :south}]
        dyed-board (dye-board raw-board)
        bulb1-colour (:colour (find-by-pos [0 0] dyed-board))
        bulb2-colour (:colour (find-by-pos [1 -1] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb1-colour :yellow))
    (is (= bulb2-colour :yellow))
    (is (= wire-colour :yellow))))

(deftest dye-bulbs-and-wire-with-no-colour
  (let [raw-board [{:pos [0 0] :type :bulb
                    :connection :east }
                   {:pos [1 0] :type :wire
                    :connection [:west :east]}]
        dyed-board (dye-board raw-board)
        bulb-colour (:colour (find-by-pos [0 0] dyed-board))
        wire-colour (:colour (find-by-pos [1 0] dyed-board))]
    (is (= bulb-colour :gray))
    (is (= wire-colour :gray))))
