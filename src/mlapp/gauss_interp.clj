(ns mlapp.gauss-interp
  "Try to reproduce 4.3.2.2, figs 4.10"
  (:use clojure.test
        clojure.core.matrix.protocols
        [incanter.charts :only [xy-plot add-points]]
        [incanter.core   :only [view]])
  (:require [clatrix.core :as c]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as p]))

(defn L-matrix
  "Create the interpolation matrix.  This is GROSS"
  [n]
  (let [m0 (m/mul (m/identity-matrix :clatrix n) -1)
        m1 (reduce (fn [im i] (set-2d im i (inc i) 2)) m0 (range (dec n)))
        m2 (reduce (fn [im i] (set-2d im i (+ i 2) -1)) m1 (range (- n 2)))]
    (m/reshape m2 [(- n 2) n])))

(defn setup
  "Return a map of the problem setup.  Hidden, observed data and values"
  [n n-observed lambda]
  (let [i (shuffle (range n))]
    {:L        (m/mul (L-matrix n) lambda)
     :observed (take n-observed i)
     :hidden   (drop n-observed i)
     :observed-values (m/matrix :clatrix
                                (repeatedly n-observed rand))}))

(defn solve
  "Return the MAP for each hidden point"
  [{:keys [L observed hidden observed-values] :as m}]
  (let [nc  (m/column-count L)
        nr  (m/row-count L)
        L1  (c/get L (range nr) hidden)
        L2  (c/get L (range nr) observed)
        l11 (m/mul (m/transpose L1) L1)
        l12 (m/mul (m/transpose L1) L2)]
    (assoc m
      :hidden-values
      (m/scale
       (m/mul (m/mul (m/inverse l11) l12) observed-values)
       -1))))

;; Run
(let [s (solve (setup 150 10 30))]
  (view
   (add-points
    (xy-plot (concat (:hidden s) (:observed s))
             (concat (:hidden-values s) (:observed-values s)))
    (:observed s) (:observed-values s))))
