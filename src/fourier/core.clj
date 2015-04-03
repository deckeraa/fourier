(ns fourier.core
  (:require [incanter
             [charts :as charts]
             [core :as core]]))

(defn an1 [n]
  (/ 4 (* Math/PI (- (* 2 n) 1))))

(defn sine-term [n x]
  (let [T 2
        an an1]
  (* (an n) (Math/sin (/ (* n Math/PI x) T)))))

(sine-term 1 2)

(defn sine-terms [x]
  (eval (cons + (map #(sine-term % x) (range 1 10000)))))


(sine-terms 3)

;; execute the following line to make the graph
(core/view (charts/function-plot sine-terms -11 11))

;(defn fourier [a0 an bn num-iters]
;  3)

;((fn [x] (+ (Math/sin x) (Math/sin x))) (/ Math/PI 2))

