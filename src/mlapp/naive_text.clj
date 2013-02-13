(ns mlapp.naive-text
  "The text classification using Naive Bayes and beta-binomial model"
  (:use [plumbing.core]
        [incanter.charts :only [bar-chart]]
        [incanter.core   :only [pow view]]
        [clojure.set])
  (:require [fs.core        :as fs]
            [stemmers.core  :as stem]
            [clojure.zip    :as zip]
            [plumbing.graph :as graph]))

;; -----------------------------------------------------------------------------
;; Read the files
(def data
  {:class-0 {:train {:dir "./resources/data/20news/20news-bydate-train/comp.os.ms-windows.misc/"}
             :test  {:dir "./resources/data/20news/20news-bydate-test/comp.os.ms-windows.misc/"}}
   :class-1 {:train {:dir "./resources/data/20news/20news-bydate-train/comp.windows.x/"}
             :test  {:dir "./resources/data/20news/20news-bydate-test/comp.windows.x/"}}})

;; The random variable of interest is x_ij -> whether stem j appears in doc i or not.
;; So want to track, for each document the set of stems, and then, for each stem in the union
;; count how many documents it appears within.

(defn stem-set
  "Returns the set of of stems that appear in the document"
  [doc]
  (->> doc
       stem/stems
       distinct
       (map keyword)
       set))

(defn stem-seqs
  "Return the stem seqs for all fns"
  [fns]
  (map (comp stem-set slurp) fns))

;; A fiddle-factor for the minimum number of docs a stem must appear in to be counted
(def min-count 10)

(defn filter-min
  [min-c stems]
  (->> stems
       (filter (fn [[k v]] (> v min-c)))
       (into {})))

(defn stem-count
  "Return a seq of stems for each doc is dir"
  [fns]
  (->> fns
       stem-seqs                         ;; seq of sets of stems
       (apply concat)                    ;; one long seq, 1 appearance of kw per doc it appears in
       frequencies-fast                  ;; Map of the number of documents in which stem appears
       (filter-min min-count)))

;; The set of stats pertaining to a single category
;; **TODO** Factor out dodgy prior !!
(def category-stats
  (graph/eager-compile
   {:files       (fnk [dir]
                      (map (partial str dir) (fs/list-dir dir)))
    :stem-counts (fnk [dir files]
                      (stem-count files))
    :num-files   (fnk [files]
                      (count files))
    :theta       (fnk [num-files stem-counts]
                      (map-vals #(/ % (+ 2 num-files)) (map-vals inc stem-counts)))}))

;; -----------------------------------------------------------------------------
;;  Want to ensure that the set of keys in theta is the same for all models
(defn- all-stems
  "Return a set of all the stems found in all the models."
  [models]
  (reduce (fn [m [k v]]
            (apply conj m (keys (:theta v))))
          #{}
          models))

;; Prior is still in here
(defn- extend-theta [{t :theta n :num-files :as model} all-keys]
  (assoc model :theta
    (merge
     (zipmap all-keys (repeat (/ 1 (+ n 2))))
     t)))

(defn- extend-models [models]
  (let [all-keys (all-stems models)]
    (reduce (fn [m [k v]] (assoc m k (extend-theta v all-keys))) {} models)))

;; Not using this just yet.
(def overall-stats
  (graph/eager-compile
   {:N_c        (fnk [categories]
                     (map :num-files categories))
    :N          (fnk [categories N_c]
                     (apply + N_c))
    :pi_c       (fnk [categories N N_c]
                     (mapv #(/ % N) N_c))}))

(defn fit-models [data]
  (extend-models
   (reduce
    (fn [m [key _]]
      (assoc m key
             (category-stats (-> data key :test))))
    {}
    data)))


;; -----------------------------------------------------------------------------
;;  Prediction
;; Use maximum-likelihood (p'tak)

(defn log [x] (Math/log x))

(defn ll
  "Return the unnormalised log likelihood for a stem-seq given a stem freq map"
  [theta doc]
  (let [doc-stems doc
        mod-stems (set (keys theta))
        stem-i    (intersection mod-stems doc-stems)
        stem-d    (difference   mod-stems doc-stems)]
    (+ (->> (select-keys theta stem-i)
            vals
            (map log)
            (reduce +))
       (->> (select-keys theta stem-d)
            vals
            (map (comp log (partial - 1)))
            (reduce +)))))

(defn ll-given-model
  "Returns a seq of the ll for each file given a word model"
  [files [_ {theta :theta}]]

  (->> files
       stem-seqs
       (map (partial ll theta))))

(defn- max-i
  "Return index of max element"
  [& xs]
  (.indexOf (vec xs) (apply max xs)))

(def prediction
  (graph/eager-compile
   {:files       (fnk [dir]
                      (map (partial str dir) (fs/list-dir dir)))
    :lls         (fnk [files models]
                      (map (partial ll-given-model files) models))
    :max-ll      (fnk [lls models]
                      (map #(nth (keys models) %) (apply map max-i lls)))}))

(defn predict-class [data models]
  (prediction (merge data {:models models})))

(comment
  (def m (fit-models data))

  (def p (predict-class (-> data :class-0 :test) m))
  (:max-ll p)

  (view (bar-chart (range (count (first (:lls p)))) (first (:lls p))))
)
