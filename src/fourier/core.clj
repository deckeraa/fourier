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

(defn bn2_b [n]
  (if (even? n)
    (/ 4 (* n Math/PI))
    0))

(defn an2_b [n]
  (if (even? n)
    0
    (/ 8 (* n n Math/PI Math/PI))))

(defn an2_c [n]
  (* (/ 2 (* n Math/PI))
     (- (Math/sin (/ (* 2 n Math/PI) 3))
        (Math/sin (/ (* 1 n Math/PI) 3)))))

(defn bn2_c [n]
  (* (/ 2 (* -1 n Math/PI))
     (- (Math/cos (/ (* 2 n Math/PI) 3))
        (Math/cos (/ (* 1 n Math/PI) 3)))))

(defn bn_5 [n]
  (if (odd? n)
    (/ 400 (* n Math/PI))
    0))

(defn an_5 [n]
  (if (odd? n)
    (/ -400 (* n n Math/PI Math/PI))
    0))

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
  (core/view (charts/function-plot fourier-series -5 5)))

;; ((partial fourier 5 0 nil bn1 30) 3) ;; computing using the fourier series

;; execute the following line to make the graph
;; (view-fourier-graph (partial fourier 5 0 nil bn1 10))
;; (view-fourier-graph (partial fourier 2 0 an2_b nil 10))
;; (view-fourier-graph (partial fourier 2 0 nil bn2_b 20))
;; (view-fourier-graph (partial fourier 3 0 an2_c nil 20))
;; (view-fourier-graph (partial fourier 3 0 nil bn2_c 30))
;; (view-fourier-graph (partial fourier 40 0 nil bn_5 30))
(view-fourier-graph (partial fourier 50 0 an_5 nil 30))

