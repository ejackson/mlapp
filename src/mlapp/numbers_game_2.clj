;; This is the number game, from 3.2 in the book, using the flow engine
;;
(ns mlapp.numbers-game-2
  (:use [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]]
        [clojure.core.incubator :only [dissoc-in]]
        [clojure.set     :only [rename-keys union difference]])
  (:require
   [inflow.core :as inf]))

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

(defn- tag
  ([n]
     (keyword (str n)))
  ([b n]
     (keyword (str (name b) "." n))))

(defn h-atom
  "Assume a single value"
  [n]
  [(tag n) (to-hyp [n])])

(defn h-even [n]
  [:even (to-hyp (filter even? (range)))])

(defn  h-odd  [n]
  [:odd (to-hyp (filter odd? (range)))])

(defn h-mult [n]
  [(tag :mult 3) (to-hyp (map #(* n %) (range)))])

(defn h-pow [n]
  [(tag :pow n) (to-hyp (map #(pow n %) (range)))])

(defn h-exp [n]
  [(tag :exp n) (to-hyp (map #(pow % n) (range)))])

(defn h-ends [n]
  [(tag :ends n) (to-hyp (range n max-num 10))])

(defn h-all [n]
  [:all (to-hyp (range))])

;; ----------- Extension operators
(defn union-h
  "Create an extension that is the union of two hypotheses"
  [[n1 e1] [n2 e2]]
  [(keyword (str (name n1) "_U_" (name n2))) (union e1 e2)])

(defn difference-h
  "Create an extension that is the difference of two hypotheses. ORDER MATTERS"
  [[n1 e1] [n2 e2]]
  [(keyword (str (name n1) "_U_" (name n2))) (difference e1 e2)])

;; ---------- Compound extensions
;; These two should be written to be combinators, but lets not bother
(defn h-pow2-and [k]
  (union-h (h-pow 2) (h-atom k)))

(defn h-pow2-but [k]
  (difference-h (h-pow 2) (h-atom k)))


;; -----------------------------------------------------------------------------
;;  Inference functions

;; 1/|hypothesis|^|data| or 0.   Equation (3.2) pg 67.
(defn flat-likelihood [extension]
  (fn [data]
    (if (every? extension data)
      (pow (/ 1.0 (count extension)) (count data))
      0.0)))

;; Combine a likelihood function with a prior.
(defn generate-hypothesis [prior hyp-fn param]
  (let [[id extension] (hyp-fn param)]
    {id {:likelihood-fn (flat-likelihood extension)
         :extension     extension
         :unnorm-prior  prior}}))

(first (hypotheses-set))
;; Generate the set of hypotheses given in the book pg 69
;; Note the priors are mapped in directly with the hypotheses-extensions
(defn hypotheses-set []
  (apply merge
         (concat
          [(generate-hypothesis 0.5 h-even nil)
           (generate-hypothesis 0.5 h-odd nil)
           (generate-hypothesis 0.1 h-exp 2)
           (generate-hypothesis 0.1 h-all nil)
           (generate-hypothesis 0.001 h-pow2-and 37)
           (generate-hypothesis 0.001 h-pow2-but 32)]
          (map (partial generate-hypothesis 0.1 h-mult) (range 3 11))
          (map (partial generate-hypothesis 0.1 h-ends) (range 1 10))
          (map (partial generate-hypothesis 0.1 h-pow)  (range 2 11)))))

;; I've pulled this out because it relies on the whole inference, so feels like a function
;; rather than a flow.  Maybe.  Thoughts.
(defn predictive-distribution
  "Returns the prior- or posterior-predicitive distribution as a map of value -> probability"
  [inference side]
  (let [summed-extension     (fn [[_ {s side e :extension}]] (map vector e (repeat s)))
        flattened-extensions (partial reduce
                                      (fn [m [k v]] (update-in m [k] (fnil (partial + v) 0.0)))
                                      (sorted-map))]
    (flattened-extensions (mapcat summed-extension (:h inference)))))

;;; ---------------
(use 'clojure.pprint)
(pprint (hypotheses-set))

(def partial-inference {:h    (hypotheses-set)
                        :data  [16]})
(def inference (inf/infer partial-inference))

(pprint inference)
(def i (:h inference))
(view (bar-chart (keys i) (map :likelihood (vals i)) :vertical false))
(view (bar-chart (keys i) (map :posterior (vals i))  :vertical false))
(def prior-pred (predictive-distribution inference :prior))
(view (bar-chart (keys prior-pred) (vals prior-pred) :vertical false))
(def post-pred (predictive-distribution inference :posterior))
(view (bar-chart (keys post-pred) (vals post-pred) :vertical false))

(def another-inference (inf/infer (assoc-in inference [:data] [60]) inference))
(def i (:h another-inference))
(view (bar-chart (keys i) (map :likelihood (vals i)) :vertical false))
(view (bar-chart (keys i) (map :posterior (vals i))  :vertical false))
(def post-pred (predictive-distribution another-inference :posterior))
(view (bar-chart (keys post-pred) (vals post-pred) :vertical false))

(comment
  ;; Have a look at the induced inference graph.  HAHAHA ITS HUUUUUUUUUGE.
  (flow/write-dotfile (construct-inference q) "flow.dot")
  "dot -Tpng -o flow.png flow.dot")
