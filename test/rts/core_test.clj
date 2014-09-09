(ns rts.core-test
  (:require [clojure.test :refer :all]
            [rts.core :refer :all]))

(deftest parkour-mock-normal
  (let [game (new-parkour-mock :normal)
        game-loop (fn [] (main-loop game))
        game-thread (Thread. game-loop)]
    (.setDaemon game-thread true)
    (.start game-thread)
    (Thread/sleep 100) ;; for swing to init

    (Thread/sleep 1000)
    (let [robot (java.awt.Robot.)]
      (.keyPress robot java.awt.event.KeyEvent/VK_SPACE)
      (.keyRelease robot java.awt.event.KeyEvent/VK_SPACE))
    (Thread/sleep 2500)

    (let [gs (game :gs)
          x (gs :x)
          y (gs :y)
          fps (gs :fps)]
      (is (< 190 x 210))
      (is (= y 0))
      (is (< 57 fps 63)))))
