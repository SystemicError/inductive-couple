(ns inductive-couple.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:time 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:time (+ (:time state) 0.05)})

(defn coil [winding-width xr yr t]
  "Parameterization of coil."
  [(+ (/ (* winding-width t) 2.0 Math/PI) (* xr (Math/cos t)))
   (* yr (Math/sin t))])

(defn draw-coil-segment [winding-width xr yr t delta]
  "Draws a small segment starting at angle t with change delta."
  (q/stroke (int (+ 128 (* 80 (Math/cos t)))))
  (q/line (coil winding-width xr yr t)
          (coil winding-width xr yr (+ t delta))))

(defn draw-coil
  "Takes in winding width, x radius, y radius, number of windings and draws coil."
  ([winding-width xr yr windings front]
   (do
     ; start and end terminals
     (if front
       (do
         (q/stroke 208)
         (q/line (+ (* winding-width windings) xr) 0
                 (+ (* winding-width windings) xr) 100))
       (do
         (q/stroke 48)
         (q/line (- (/ winding-width -2.0) xr) 0
                 (- (/ winding-width -2.0) xr) 100)))
     ; recur for spiral segment
     (draw-coil winding-width xr yr windings front (map #(* % (/ Math/PI 180.0)) (range -180 (* windings 360) 4)))))
  ([winding-width xr yr windings front angles]
   (if (not (empty? angles))
     (do
       (if (or (and front (< 0 (Math/cos (first angles))))
               (and (not front) (> 0 (Math/cos (first angles)))))
         (draw-coil-segment winding-width xr yr (first angles) 0.01))
       (recur winding-width xr yr windings front (rest angles))))))

(defn draw-arrow [x]
  "Draws an arrow of length x, pointing to the right, centered on the origin."
  (if (< 0 x)
    (do
      (q/line 0 0 x 0)
      (q/line x 0 (- x 5) -5)
      (q/line x 0 (- x 5) 5))
    (if (> 0 x)
      (do
        (q/line 0 0 x 0)
        (q/line x 0 (+ 5 x) -5)
        (q/line x 0 (+ 5 x) 5)))))

(defn draw-state [state]
  (let [t (:time state)
        mag (Math/cos t)]
    ; Clear the sketch by filling it with light-grey color.
    (q/background 255)

    (q/stroke-weight 10)
    ; primary coil back
    (q/with-translation [300
                         (/ (q/height) 2)]
      (draw-coil 40 20 50 5 false))
    ; secondary coil back
    (q/with-translation [600
                         (/ (q/height) 2)]
      (draw-coil 40 20 50 5 false))

    (q/stroke-weight 5)
    ; magnetic field
    (q/stroke 0 0 255)
    (q/with-translation [(- 400 (/ (* 700 mag) 2))
                         (/ (q/height) 2)]
      (draw-arrow (* 700 mag)))

    (q/stroke-weight 10)
    ; primary coil front
    (q/with-translation [300
                         (/ (q/height) 2)]
      (draw-coil 40 20 50 5 true))
    ; secondary coil front
    (q/with-translation [600
                         (/ (q/height) 2)]
      (draw-coil 40 20 50 5 true))

    (q/stroke-weight 5)
    ; primary current
    (q/stroke 255 0 0)
    (q/with-translation [260
                         (+ 50 (/ (q/height) 2) (* mag 20))]
      (q/with-rotation [(/ Math/PI -2.0)]
        (draw-arrow (* 40 mag))))
    (q/with-translation [520
                         (- (+ 50 (/ (q/height) 2)) (* mag 20))]
      (q/with-rotation [(/ Math/PI 2.0)]
        (draw-arrow (* 40 mag))))
    ; secondary current
    (q/stroke 255 0 0)
    (q/with-translation [560
                         (+ 50 (/ (q/height) 2) (* mag -20))]
      (q/with-rotation [(/ Math/PI 2.0)]
        (draw-arrow (* 40 mag))))
    (q/with-translation [820
                         (+ 50 (/ (q/height) 2) (* mag 20))]
      (q/with-rotation [(/ Math/PI -2.0)]
        (draw-arrow (* 40 mag))))
    ))


(q/defsketch inductive-couple
  :title "Inductive Coupling"
  :size [900 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
