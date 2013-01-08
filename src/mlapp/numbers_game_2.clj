;; This is the number game, from 3.2 in the book.
;;
(ns mlapp.numbers-game-2
  (:use [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]])
  (:require
   [com.stuartsierra.flow :as flow]))

;; Helpers
(defn k-cat [s1 s2]
  (keyword (str s1 (name s2))))

(defn prior-key [id]
  (k-cat "prior-" id))

(defn likelihood-fn-key [id]
  (k-cat "likelihood-fn-" id))

(defn likelihood-key [id]
  (k-cat "likelihood-" id))

(defn unnorm-posterior-key [id]
  (k-cat "unnorm-posterior-" id))

(defn posterior-key [id]
  (k-cat "posterior-" id))

(defn add-hypothesis [flow {:keys [id likelihood-fn prior] :as hypothesis}]
  (let [prior-k              (prior-key id)
        likelihood-fn-k      (likelihood-fn-key id)
        likelihood-k         (likelihood-key id)
        unnorm-posterior-k   (unnorm-posterior-key id)
        posterior-k          (posterior-key id)]
   (-> flow
       (assoc prior-k                   (flow/flow-fn [] prior))
       (assoc likelihood-fn-k           (flow/flow-fn [] likelihood-fn))
       (assoc likelihood-k              (flow/with-inputs [likelihood-fn-k :data]
                                          (fn [{data :data :as r}]
                                            ((likelihood-fn-k r) data))))
       (assoc unnorm-posterior-k        (flow/with-inputs [likelihood-k prior-k]
                                          (fn [r]
                                            (* (likelihood-k r) ((prior-key id) r)))))
       (assoc (posterior-key id)        (flow/with-inputs [unnorm-posterior-k :norm]
                                          (fn [r]
                                            (/ (unnorm-posterior-k r) (:norm r))))))))

(defn normalise-flow [flow hypotheses]
  (assoc flow :norm (flow/with-inputs (vec (map (comp unnorm-posterior-key :id) the-hypotheses))
                      (fn [r] (apply + (map #((-> % :id unnorm-posterior-key) r) hypotheses))))))

(defn add-hypotheses [flow hypotheses]
  (reduce add-hypothesis (normalise-flow flow hypotheses) hypotheses))

;; ----------------------------------------------

(defn construct-inference [hypotheses]
  (add-hypotheses (flow/flow) hypotheses))

(defn- inf-keys [id]
  [(likelihood-key id)
   (unnorm-posterior-key id)
   (posterior-key id)])

(defn infer [hypotheses data]
  (let [inf-graph (construct-inference hypotheses)
        inf       (flow/run inf-graph {:data data})]
    (reduce
     (fn [v {id :id :as h}]
       (conj v
             (merge h (select-keys inf (inf-keys id)))))
     []
     hypotheses)))

(comment
  (use 'clojure.pprint)

;; These likelihoods are whacked !
(def the-hypotheses
  [{:id :h1, :likelihood-fn inc :prior 2/3}
   {:id :h2, :likelihood-fn dec :prior 1/3}
   {:id :h3, :likelihood-fn dec :prior 1/3}
   {:id :h4, :likelihood-fn dec :prior 1/3}])

  (pprint (infer the-hypotheses 3))

  (flow/write-dotfile (construct-inference the-hypotheses) "flow.dot")
  (comment dot -Tpng -o flow.png flow.dot))
