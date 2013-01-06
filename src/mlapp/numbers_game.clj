;; This is the number game, from 3.2 in the book.
;;
(ns mlapp.numbers-game
  (:use [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]]))

;; -----------------------------------------------------------------------------
;; Some helpful utilities
(def max-num 100)

(defn <=Max [xs]
  (->> xs
       (take-while #(<= % max-num))
       (map double)))

(defn to-hyp [xs]
  (apply sorted-set (<=Max xs)))

;; -----------------------------------------------------------------------------
;; Our collection of hypotheses.  These are just sorted sets, so inclusion of
;; data in the set is a simple function application of the data the set.

;; Build some hypotheses
(def h-even (to-hyp (filter even? (range))))
(def h-odd  (to-hyp (filter odd? (range))))

(defn h-mult [n]
  (to-hyp (map #(* n %) (range))))

(defn h-pow [n]
  (to-hyp (map #(pow n %) (range))))

(defn h-exp [n]
  (to-hyp (map #(pow % n) (range))))

(defn h-ends-with [n]
  (to-hyp (range n max-num 10)))

(defn h-all []
  (to-hyp (range)))

;; -----------------------------------------------------------------------------
;;  Inference functions

;; Ye basic, flat prior
(defn flat-prior [hypotheses hypothesis] (/ 1.0 (count hypotheses)))

;; This is defined in the matlab code as normalised [0.5 0.5 0.1...0.1 0.001 0.001]
;; I don't like the way this is arranged as a vector, ie order counts.  Not good enough.
(defn basic-prior [hypotheses hypothesis]
  (let [unnorm-prior (concat
                      [0.5 0.5]
                      (take (- (count hypotheses) 4) (repeat 0.1))
                      [0.001 0.001])
        norm         (apply + unnorm-prior)
        prior-fns    (map #(/ % norm) unnorm-prior)
        prior-lookup (zipmap hypotheses prior-fns)]
    (prior-lookup hypothesis)))

;; 1/|hypothesis|^|data| or 0
(defn flat-likelihood [data hypothesis]
  (if (every? hypothesis data)
    (pow (/ 1.0 (count hypothesis)) (count data))
    0.0))

;; Unnormalised posterior
(defn un-posterior [prior-fn likelihood-fn hypothesis data]
  (* (prior-fn hypothesis)
     (likelihood-fn data hypothesis)))

;; Normalised posterior
(defn posteriors
  "Normalisation is expensive and we need to calc all posteriors to do it."
  [prior-fn likelihood-fn hypotheses data]
  (let [posts (map #(un-posterior prior-fn likelihood-fn % (map double data)) hypotheses)
        norm  (apply + posts)]
    (map #(/ % norm) posts)))

;; -----------------------------------------------------------------------------
;; Play the numbers game
(defn numbers-game [prior-fn likelihood-fn hypotheses data]
  (let [posterior-set (posteriors prior-fn likelihood-fn hypotheses data)]
    (view (bar-chart (range (count posterior-set)) posterior-set))
    posterior-set))

(def game-hypotheses
  (concat
   [h-even
    h-odd
    (h-exp 2)]
   (map h-mult      (range 3 11))
   (map h-ends-with (range 1 10))
   (map h-pow       (range 2 11))
   [(h-all)
    (conj (h-pow 2) 37)
    (disj (h-pow 2) 32)]))

(def my-data [8 16 2 64])

;; Look at the prior
(view (bar-chart (range (count game-hypotheses))
                 (map (partial basic-prior game-hypotheses) game-hypotheses)))

;; Look at the likelihoods
(view (bar-chart (range (count game-hypotheses))
                 (map (partial flat-likelihood my-data) game-hypotheses)))

;; Look at the posteriors
(numbers-game (partial basic-prior game-hypotheses) flat-likelihood game-hypotheses my-data)
