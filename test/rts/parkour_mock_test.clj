(ns rts.parkour-mock-test
  (:require [clojure.test :refer :all]
            [rts.engine :refer :all])
  (:use rts.parkour-mock))

(deftest parkour-mock-normal
  (let [game (new-parkour-mock :normal 480 360 60)
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
    (game :stop)

    (let [gs (game :gs)
          x (gs :x)
          y (gs :y)
          fps (gs :fps)]
      (is (< 180 x 220))
      (is (= y 0))
      (is (< 57 fps 63)))))

(deftest parkour-mock-slow-render
  (let [game (new-parkour-mock :slow 480 360 60)
        game-loop (fn [] (main-loop game))
        game-thread (Thread. game-loop)]
    (.setDaemon game-thread true)
    (.start game-thread)
    (Thread/sleep 100) ;; for swing to init

    (Thread/sleep 3000)
    (game :stop)
    
    (let [gs (game :gs)
          fps (gs :fps)]
      (is (< 20 fps 50)))))
