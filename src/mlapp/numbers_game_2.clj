;; This is the number game, from 3.2 in the book.
;;
(ns mlapp.numbers-game-2
  (:use [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]])
  (:require
   [com.stuartsierra.flow :as flow]))

;; Helpers
(defn s-cat [s1 s2]
  (symbol (str s1 (name s2))))

(defn prior-sym [id]
  (s-cat "prior-" id))

(defn prior-key [id]
  (keyword (prior-sym id)))

(defn likelihood-fn-sym [id]
  (s-cat "likelihood-fn-" id))

(defn likelihood-fn-key [id]
  (keyword (likelihood-fn-sym id)))

(defn likelihood-sym [id]
  (s-cat "likelihood-" id))

(defn likelihood-key [id]
  (keyword (likelihood-sym id)))

(defn unnorm-posterior-sym [id]
  (s-cat "unnorm-posterior-" id))

(defn unnorm-posterior-key [id]
  (keyword (unnorm-posterior-sym id)))

(defn posterior-sym [id]
  (s-cat "posterior-" id))

(def posterior-key (comp keyword posterior-sym))

(defn add-hypothesis [flow {:keys [id likelihood-fn prior] :as hypothesis}]
  (-> flow
      (assoc (prior-key id)            (flow/flow-fn [] prior))
      (assoc (likelihood-fn-key id)    (flow/flow-fn [] likelihood-fn))
      (assoc (likelihood-key id)       (flow/with-inputs [(likelihood-fn-key id) :data]
                                         (eval`(fn [{:keys [~(likelihood-fn-sym id) ~'data]}]
                                                 (~(likelihood-fn-sym id) ~'data)))))
      (assoc (unnorm-posterior-key id) (flow/with-inputs [(likelihood-key id) (prior-key id)]
                                         (eval`(fn [{:keys [~(likelihood-sym id) ~(prior-sym id)]}]
                                                 (* ~(likelihood-sym id) ~(prior-sym id))))))
      (assoc (posterior-sym id)        (flow/with-inputs [(unnorm-posterior-key id) :norm]
                                         (eval`(fn [{:keys [~(unnorm-posterior-sym id) ~'norm]}]
                                                 (/ ~(unnorm-posterior-sym id) ~'norm)))))))

(defn norm-fn [ids]
  `(flow/with-inputs ~(vec (map unnorm-posterior-key ids))
     (fn [{:keys ~(vec (map unnorm-posterior-sym ids))}]
       (+ ~@(map unnorm-posterior-sym ids)))))

(defn normalise-flow [flow hypotheses]
  (assoc flow :norm (eval (norm-fn (map :id hypotheses)))))

(defn add-hypotheses [flow hypotheses]
  (reduce add-hypothesis (normalise-flow flow hypotheses) hypotheses))

;; ----------------------------------------------
;; These likelihoods are whacked !
(def the-hypotheses
  [{:id :h1, :likelihood-fn inc :prior 2/3}
   {:id :h2, :likelihood-fn dec :prior 1/3}
   {:id :h3, :likelihood-fn dec :prior 1/3}
   {:id :h4, :likelihood-fn dec :prior 1/3}])

(flow/run
 (add-hypotheses (flow/flow) the-hypotheses)
 {:data 3})

(def inference1
  (add-hypotheses (flow/flow) the-hypotheses))

(flow/write-dotfile inference1 "flow.dot")

(comment dot -Tpng -o flow.png flow.dot)
