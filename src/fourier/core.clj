;; fourier
;; Graphs a user-supplied Fourier series.
;; (http://en.wikipedia.org/wiki/Fourier_series)
;; Intended to be used via a REPL.
(ns fourier.core
  (:require [incanter
             [charts :as charts]
             [core :as core]]))

;; user-supplied function
(defn bn1 [n]
  (if (even? n)
    0
    (/ 4 (* n Math/PI))))

;; (defn bn1 [n]
;;   (/ 8 (* Math/PI (- (* 2 n) 1))))

(defn an2_b [n]
  (if (even? n)
    0
    (/ 8 (* n n Math/PI Math/PI))))

; Compute an individual cosine term: a_n * cos( (n pi x)/T )
(defn cosine-term [T an n x]
  (* (an n) (Math/cos (/ (* n Math/PI x) T))))

; Compute an individual sine term: a_n * sin( (n pi x)/T )
(defn sine-term [T bn n x]
  (* (bn n) (Math/sin (/ (* n Math/PI x) T))))

; Computes the partial sum of num-iters number of cosine terms
(defn cosine-terms [T an num-iters x]
  (eval (cons + (map #(cosine-term T an % x) (range 1 (+ num-iters 1))))))

(defn sine-terms [T bn num-iters x]
  (eval (cons + (map #(sine-term T bn % x) (range 1 (+ num-iters 1))))))

; Returns the Fourier series approximation at any point
(defn fourier [T a0 an bn num-iters x]
  (let [an (if (nil? an) (fn [x] 0) an)
        bn (if (nil? bn) (fn [x] 0) bn)]
    (+ a0 (cosine-terms T an num-iters x) (sine-terms T bn num-iters x))))

; Plots the Fourier series
; fourier-series needs to be a function that takes in one parameter, x
(defn view-fourier-graph [fourier-series]
  (core/view (charts/function-plot fourier-series -10 10)))

;; ((partial fourier 5 0 nil bn1 30) 3) ;; computing using the fourier series

;; execute the following line to make the graph
(view-fourier-graph (partial fourier 5 0 nil bn1 30))
(view-fourier-graph (partial fourier 2 0 an2_b nil 100))

