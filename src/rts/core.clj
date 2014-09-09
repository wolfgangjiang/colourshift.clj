(ns rts.core
  (:import [java.util Date]))

(def specified-fps 60)
(def specified-width 480)
(def specified-height (* 0.75 specified-width))

  

(defn new-timestep []
  (let [best-frame-interval (/ 1000.0 specified-fps)
        cur-time (fn [] (System/currentTimeMillis))
        last-frame-head-time (atom (cur-time))
        this-frame-head-time (atom (cur-time))
        rolling-fps (atom 0)
        dt (fn []
             (max 1 (- @this-frame-head-time
                       @last-frame-head-time)))
        update-fps (fn [old]
                     (let [eff 0.9]
                       (+ (* eff old)
                          (* (- 1 eff) (/ 1000.0 (dt))))))]
    (fn [method & args]
      (case method
        :frame-start
        (reset! this-frame-head-time (cur-time))
        :get-dt (dt)
        :get-sleep-time 
        (max 0 (- best-frame-interval
                  (- (cur-time) @this-frame-head-time)))
        :get-fps @rolling-fps
        :frame-finish (do
                        (swap! rolling-fps update-fps)
                        (reset! last-frame-head-time
                                @this-frame-head-time))
        :else (throw (RuntimeException. (str method
                                             " is not a method in timestep")))))))

(defn new-inputs-listener []
  (let [event-queue (atom [])
        push-event (fn [e-data]
                     (swap! event-queue conj e-data))]
    (fn [method & args]
      (case method
        :collect (let [events @event-queue]
                   (reset! event-queue [])
                   events)
        :listen (proxy [java.awt.event.KeyAdapter] []
                  (keyPressed [e]
                    (push-event (.getKeyCode e))))
        :else (throw (RuntimeException. (str method
                                             " is not a method in inputs-listener")))))))

(defn init-swing [inputs-listener]
  (let [frame (javax.swing.JFrame.)
        panel (proxy [javax.swing.JPanel] []
                (getPreferredSize []
                  (java.awt.Dimension. specified-width
                                       specified-height)))]
    (doto frame
      (.add panel)
      (.pack)
      (.addKeyListener (inputs-listener :listen))
      (.setDefaultCloseOperation
       javax.swing.JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    (.getGraphics panel)))

(defn main-loop [game]
  (let [inputs-listener (new-inputs-listener)
        board-graphics (init-swing inputs-listener)
        timestep (new-timestep)]
    (loop []
      (timestep :frame-start)
      (game :handle-user-inputs (inputs-listener :collect))
      (game :handle-tick (timestep :get-dt))
      (game :put-engine-info {:fps (timestep :get-fps)})
      (.clearRect board-graphics 0 0
                  specified-width specified-height)
      (game :render board-graphics)
      (Thread/sleep (timestep :get-sleep-time))
      (timestep :frame-finish)
      (recur))))

;;;; =================================


(defn put-engine-info [gs engine-info]
  (assoc gs :fps (:fps engine-info)))

(defn handle-user-inputs [gs inputs]
  (if (empty? inputs)
    gs
    (assoc gs :direction [0 -1])))

(defn handle-tick [gs dt]
  (let [dir (get gs :direction)
        [x y] [(get gs :x) (get gs :y)]
        delta (* (/ 200.0 1000) dt)
        [dx dy] (map * dir [delta delta])
        nx (max 0 (min specified-width (+ x dx)))
        ny (max 0 (min specified-height (+ y dy)))]
    (-> gs
        (assoc :x nx)
        (assoc :y ny))))

(defn init []
  {:x 0
   :y 300
   :fps -1
   :direction [1 0]})

(defn render [gs g]
  (let [x (get gs :x)
        y (get gs :y)]
    (.setColor g java.awt.Color/BLUE)
    (.fillRect g x y 10 10)))

(defn new-parkour-mock [test-tag]
  (let [gs (atom (init))]
    (fn [method & args]
      (case method
        :gs @gs
        :handle-user-inputs 
        (swap! gs (fn [gs] 
                    (apply handle-user-inputs gs args)))
        :handle-tick
        (swap! gs (fn [gs]
                    (apply handle-tick gs args)))
        :put-engine-info
        (swap! gs (fn [gs]
                    (apply put-engine-info gs args)))
        :render (apply render @gs args)))))

(defn -main [& args]
  (main-loop (new-parkour-mock :normal)))

