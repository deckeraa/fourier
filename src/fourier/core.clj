(ns fourier.core
  (:require [incanter
             [charts :as charts]
             [core :as core]]))

(defn bn1 [n]
  (/ 8 (* Math/PI (- (* 2 n) 1))))

(defn cosine-term [T an n x]
  (* (an n) (Math/cos (/ (* n Math/PI x) T))))

(defn sine-term [T bn n x]
  (* (bn n) (Math/sin (/ (* n Math/PI x) T))))

(defn cosine-terms [T an num-iters x]
  (eval (cons + (map #(cosine-term T an % x) (range 1 (+ num-iters 1))))))

(defn sine-terms [T bn num-iters x]
  (eval (cons + (map #(sine-term T bn % x) (range 1 (+ num-iters 1))))))

(defn fourier [T a0 an bn num-iters x]
  (let [an (if (nil? an) (fn [x] 0) an)
        bn (if (nil? bn) (fn [x] 0) bn)]
    (+ a0 (cosine-terms T an num-iters x) (sine-terms T bn num-iters x))))

(defn view-fourier-graph [fourier-series]
  (core/view (charts/function-plot fourier-series -10 10)))

;; ((partial fourier 5 0 nil bn1 30) 3) ;; computing using the fourier series

;; execute the following line to make the graph
(view-fourier-graph (partial fourier 5 0 nil bn1 30))

