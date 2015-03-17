(ns rts.widgets
  (:import [java.awt BasicStroke]))

;;;; ==== utilities ============

(def default-font-size 20)

(defn set-font-size [g size]
  (let [font (.getFont g)
        new-font (java.awt.Font. (.getFontName font) 0 size)]
    (.setFont g new-font)))

(defn get-widget-in-gs [gs widget-name]
  (let [widget-dict (:widget-dict gs)
        widget (get widget-dict widget-name)]
    widget))

(defn get-widget-kind-in-gs [gs widget-name]
  (:kind (get-widget-in-gs gs widget-name)))

(defn estimate-width [g string]
  (let [font-metrics (.getFontMetrics g)]
    (.stringWidth font-metrics string)))

(defn estimate-height [g string]
  (let [font-metrics (.getFontMetrics g)]
    (.getHeight font-metrics)))

(defn half [value]
  (/ value 2))

(defn draw-text-in-box [g text font-size x y w h]
  (set-font-size g font-size)
  (let [text-w (estimate-width g text)
        text-h (estimate-height g text)
        text-x (+ x (half (- w text-w)))
        text-y (+ y (half (- h text-h)))]
    (.drawString g text (int text-x) (int text-y))))

;;;; ==== multimethods =================

(defmulti wd-render
  (fn [gs widget-name g]
    (get-widget-kind-in-gs gs widget-name)))

(defmulti wd-hit-test
  (fn [gs widget-name mouse-pos]
    (get-widget-kind-in-gs gs widget-name)))

(defmulti wd-click
  (fn [gs widget-name mouse-pos]
    (get-widget-kind-in-gs gs widget-name)))

(defmulti wd-get-value
  (fn [gs widget-name]
    (get-widget-kind-in-gs gs widget-name)))

;;;; ===== default methods =============

(defmethod wd-render :default [gs widget-name g]
  (throw (RuntimeException. "no default renderer for widgets")))

(defmethod wd-hit-test :default [gs widget-name mouse-pos]
  (let [widget (get-widget-in-gs gs widget-name)
        {:keys [x y w h]} (:dimension widget)
        [mx my] mouse-pos]
    (and (<= x mx (+ x w))
         (<= y my (+ y h)))))

(defmethod wd-click :default [gs widget-name mouse-pos]
  (let [widget (get-widget-in-gs gs widget-name)]
    [widget gs]))

(defmethod wd-get-value :default [gs widget-name]
  nil)

;;;; ===== label ======================

(defn wd-create-label [x y w h text font-size]
  {:kind :label
   :dimension {:x x :y y :w w :h h}
   :text text
   :font-size font-size})

(defmethod wd-render :label [gs widget-name g]
  (let [widget (get-widget-in-gs gs widget-name)
        {:keys [x y w h]} (:dimension widget)
        text (:text widget)]
    (set-font-size g (:font-size widget))
    (draw-text-in-box g text (:font-size widget)
                      x y w h)))

(defmethod wd-hit-test :label [gs widget-name mouse-pos]
  false)

;;; click and get-value are inherited from default.

;;;; ====== button ====================

(defn wd-create-button [x y w h text]
  {:kind :button
   :dimension {:x x :y y :w w :h h}
   :text text})

(defmethod wd-render :button [gs widget-name g]
  (let [widget (get-widget-in-gs gs widget-name)
        {:keys [x y w h]} (:dimension widget)
        text (:text widget)]
    (.setStroke g (BasicStroke. 4))
    (.drawRect g x y w h)
    (draw-text-in-box g text default-font-size
                      x y w h)))

(defmethod wd-click :button [gs widget-name mouse-pos]
  (let [widget (get-widget-in-gs gs widget-name)
        size-value (wd-get-value gs "size")
        wrapped-value (wd-get-value gs "is-wrapped")
        [x-tiles y-tiles seed-count] (get {0 [3 3 2]
                                           1 [8 8 3]
                                           2 [16 16 8]
                                           3 [30 18 14]} size-value)
        wrapped (get {0 false 1 true} wrapped-value)
        board-config {:x-tiles x-tiles
                      :y-tiles y-tiles
                      :seed-count seed-count
                      :wrapped wrapped}
        new-gs (-> gs
                   (update-in [:config :board-config] (fn [_] board-config))
                   (assoc :mode :ready-to-generate))]
    [widget new-gs]))

;;; hit-test and get-value are inherited from default.

;;;; ============== radio ====================

(defn wd-create-radio [x y w h text-bars]
  {:kind :radio
   :dimension {:x x :y y :w w :h h}
   :bar-list text-bars
   :value 0})

(defmethod wd-render :radio [gs widget-name g]
  (let [widget (get-widget-in-gs gs widget-name)
        {:keys [x y w h]} (:dimension widget)
        bar-list (:bar-list widget)
        bar-w (/ w (count bar-list))]
    (doseq [i (range (count bar-list))]
      (let [bar-text (get bar-list i)
            bar-x (+ x (* i bar-w))]
        (draw-text-in-box g bar-text default-font-size
                          bar-x y bar-w h)
        (when (= i (:value widget))
          (.setStroke g (BasicStroke. 2))
          (.drawRect g bar-x y bar-w h))))))

(defmethod wd-click :radio [gs widget-name mouse-pos]
  (let [widget (get-widget-in-gs gs widget-name)
        {:keys [x y w h]} (:dimension widget)
        bar-list (:bar-list widget)
        bar-w (/ w (count bar-list))
        [mx my] mouse-pos
        bar-index (int (/ (- mx x) bar-w))
        new-widget (assoc widget :value bar-index)]
    [new-widget gs]))

(defmethod wd-get-value :radio [gs widget-name]
  (let [widget (get-widget-in-gs gs widget-name)]
    (:value widget)))

;;;; hit-test is inherited from default
