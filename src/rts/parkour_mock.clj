(ns rts.parkour-mock
  (:use rts.engine))

(defn put-engine-info [gs engine-info]
  (assoc gs :fps (:fps engine-info)))

(defn handle-user-inputs [gs inputs]
  (if (empty? inputs)
    gs
    (assoc gs :direction [0 -1])))

(defn handle-tick [gs dt]
  (let [dir (get gs :direction)
        [x y] [(get gs :x) (get gs :y)]
        config (get gs :config)
        max-x (get config :max-x)
        max-y (get config :max-y)
        delta (* (/ 200.0 1000) dt)
        [dx dy] (map * dir [delta delta])
        nx (max 0 (min max-x (+ x dx)))
        ny (max 0 (min max-y (+ y dy)))]
    (-> gs
        (assoc :x nx)
        (assoc :y ny))))

(defn init [config]
  {:config config
   :x 0
   :y 300
   :fps -1
   :direction [1 0]})

(defn render [gs g]
  (let [x (get gs :x)
        y (get gs :y)]
    (let [test-tag (get-in gs [:config :test-tag])]
      (when (= test-tag :slow)
        (Thread/sleep 25)))
    (.setColor g java.awt.Color/BLUE)
    (.fillRect g x y 10 10)))

(defn new-parkour-mock [test-tag max-x max-y best-fps]
  (let [config {:test-tag test-tag
                :max-x max-x
                :max-y max-y
                :best-fps best-fps}
        methods {:init init
                 :handle-user-inputs handle-user-inputs
                 :handle-tick handle-tick
                 :put-engine-info put-engine-info
                 :render render}]
    (new-game config methods)))

(defn start []
  (main-loop (new-parkour-mock :normal 480 360 60)))

