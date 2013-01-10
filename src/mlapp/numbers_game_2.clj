;; This is the number game, from 3.2 in the book, using the flow engine
;;
(ns mlapp.numbers-game-2
  (:use [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]]
        [clojure.set     :only [rename-keys]])
  (:require
   [mlapp.inflow :as inf]))

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
;; Our collection of hypotheses.
;; They work simply by giving the set of all members of the hypothesis
;; These are just (sorted) sets, so inclusion of
;; data in the set is a simple function application of the data the set.

;; All fns return a vector of a hypothesis identifier and the extension.
;; All fns take one argument, needed or not, to parameterise the hypothesis

(defn- tag [b n]
  (keyword (str (name b) "::" n)))

(defn h-even [n]
  [(tag :h-even n) (to-hyp (filter even? (range)))])

(defn  h-odd  [n]
  [(tag :h-odd n) (to-hyp (filter odd? (range)))])

(defn h-mult [n]
  [(tag :h-mult n) (to-hyp (map #(* n %) (range)))])

(defn h-pow [n]
  [(tag :h-pow n) (to-hyp (map #(pow n %) (range)))])

(defn h-exp [n]
  [(tag :h-exp n) (to-hyp (map #(pow % n) (range)))])

(defn h-ends [n]
  [(tag :h-ends n) (to-hyp (range n max-num 10))])

(defn h-all [n]
  [(tag :h-all n) (to-hyp (range))])

;; These two should be written to be combinators, but lets not bother
(defn h-pow2-and [k]
  [(tag :h-pow2-and k) (conj (to-hyp (map #(pow 2 %) (range))) k)])

(defn h-pow2-but [k]
  [(tag :h-pow2-but k) (disj (to-hyp (map #(pow 2 %) (range))) k)])

;; -----------------------------------------------------------------------------
;;  Inference functions

;; 1/|hypothesis|^|data| or 0.   Equation (3.2) pg 67.
(defn flat-likelihood [extension]
  (fn [data]
    (if (every? extension data)
      (pow (/ 1.0 (count extension)) (count data))
      0.0)))

;; Start by creating the hypothesis set, and giving it a namew
(defn generate-hypothesis [prior hyp-fn param]
  (let [[id extension] (hyp-fn param)]
    {:id id
     :likelihood-fn (flat-likelihood extension)
     :prior prior}))

;; Generate the set of hypotheses given in the book pg 69
;; Note the priors are mapped in directly with the hypotheses-extensions
;; The silly ordering is to make the pics comparable to those in the book
(defn hypotheses-set []
  (vec
   (concat
    [(generate-hypothesis 0.5 h-even nil)
     (generate-hypothesis 0.5 h-odd nil)
     (generate-hypothesis 0.1 h-exp 2)]
    (map (partial generate-hypothesis 0.1 h-mult) (range 3 11))
    (map (partial generate-hypothesis 0.1 h-ends) (range 1 10))
    (map (partial generate-hypothesis 0.1 h-pow)  (range 2 11))
    [(generate-hypothesis 0.1 h-all nil)
     (generate-hypothesis 0.001 h-pow2-and 37)
     (generate-hypothesis 0.001 h-pow2-but 32)])))

(comment
  (use 'clojure.pprint)
  (def q (hypotheses-set))
  (def i (inf/infer q [16]))
  (pprint i)
  (view (bar-chart (map :id i) (map :likelihood i) :vertical false))
  (view (bar-chart (map :id i) (map :posterior i) :vertical false))

  (flow/write-dotfile (construct-inference q) "flow.dot")
  (comment dot -Tpng -o flow.png flow.dot))
