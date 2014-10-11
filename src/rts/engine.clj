(ns rts.engine
  (:import [java.util Date]))

(defn new-timestep [best-fps]
  (let [best-frame-interval (/ 1000.0 best-fps)
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
        :else (throw (RuntimeException. (str method " is not a method in timestep")))))))

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
        :else (throw (RuntimeException. (str method " is not a method in inputs-listener")))))))

(defn init-swing [inputs-listener width height]
  (let [frame (proxy [javax.swing.JFrame] []
                (getPreferredSize []
                  (java.awt.Dimension. width height)))]
    (doto frame
      (.pack)
      (.addKeyListener (inputs-listener :listen))
      (.setDefaultCloseOperation
       javax.swing.JFrame/EXIT_ON_CLOSE)
      (.setVisible true)
      (.setIgnoreRepaint true)
      (.createBufferStrategy 2))
    frame))

(defmacro with-graphics-2d [window g & body]
  `(let [bs# (.getBufferStrategy ~window)
         ~g (.getDrawGraphics bs#)]
     (try
       ~@body
       (finally (.dispose ~g)))
     (.show bs#)
     (.sync (java.awt.Toolkit/getDefaultToolkit))))

(defn main-loop [game]
  (let [config (game :config)
        inputs-listener (new-inputs-listener)
        board-frame (init-swing inputs-listener
                                (:max-x config)
                                (:max-y config))
        timestep (new-timestep (:best-fps config))]
    (loop []
      (when-not (game :should-stop)
        (timestep :frame-start)
        (game :handle-user-inputs (inputs-listener :collect))
        (game :handle-tick (timestep :get-dt))
        (game :put-engine-info {:fps (timestep :get-fps)})
        (with-graphics-2d board-frame g
          (.clearRect g 0 0
                      (.getWidth board-frame)
                      (.getHeight board-frame))
          (game :render g))
        (Thread/sleep (timestep :get-sleep-time))
        (timestep :frame-finish)
        (recur)))
    (.dispose board-frame)))

(defn new-game [config methods]
  (let [{:keys [init handle-user-inputs handle-tick put-engine-info render]} methods
        gs (atom (init config))]
    (fn [method & args]
      (case method
        :gs @gs
        :config (:config @gs)
        :should-stop (:should-stop @gs)
        :stop (swap! gs assoc-in [:should-stop] true)
        :handle-user-inputs 
        (swap! gs #(apply handle-user-inputs % args))
        :handle-tick
        (swap! gs #(apply handle-tick % args))
        :put-engine-info
        (swap! gs #(apply put-engine-info % args))
        :render (apply render @gs args)))))
