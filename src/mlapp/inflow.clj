(ns mlapp.inflow
  (:use
   [clojure.set     :only [rename-keys]])
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
  (assoc flow :norm (flow/with-inputs (vec (map (comp unnorm-posterior-key :id) hypotheses))
                      (fn [r] (apply + (map #((-> % :id unnorm-posterior-key) r) hypotheses))))))

(defn add-hypotheses [flow hypotheses]
  (reduce add-hypothesis (normalise-flow flow hypotheses) hypotheses))

;; ----------------------------------------------

(defn construct-inference
  "Create the flow graph from the set of hypotheses"
  [hypotheses]
  (add-hypotheses (flow/flow) hypotheses))

(defn- inf-keys [id]
  {(likelihood-key id)       :likelihood
   (unnorm-posterior-key id) :unnorm-posterior
   (posterior-key id)        :posterior})

;; Normalise the names of all the hypotheses coming out of the complete graph
(defn- extract-hypotheses-keys [id inf-flow]
  (let [key-xf (inf-keys id)]
    (-> inf-flow
        (select-keys (keys key-xf))
        (rename-keys key-xf))))

(defn infer
  "Performs full inference, up to the normalised posteriors.  Returns a vector of all posteriors."
  [hypotheses data]
  (let [inf-graph (construct-inference hypotheses)
        inf       (flow/run inf-graph {:data data})]
    (reduce
     (fn [v {id :id :as h}]
       (conj v
             (merge h (extract-hypotheses-keys id inf))))
     []
     hypotheses)))
