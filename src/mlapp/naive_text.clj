(ns mlapp.naive-text
  "The text classification using Naive Bayes and beta-binomial model"
  (:use [plumbing.core])
  (:require [fs.core :as fs]
            [stemmers.core :as stem]
            [clojure.zip :as zip]
            [plumbing.graph :as graph]))

;; -----------------------------------------------------------------------------
;; Read the files
(def data-files
  {:class-0 {:train {:dir "./resources/data/20news/20news-bydate-train/comp.os.ms-windows.misc/"}
             :test  {:dir "./resources/data/20news/20news-bydate-test/comp.os.ms-windows.misc/"}}
   :class-1 {:train {:dir "./resources/data/20news/20news-bydate-train/comp.windows.x/"}
             :test  {:dir "./resources/data/20news/20news-bydate-test/comp.windows.x/"}}})

(defn- count-map [xs]
  "Count the number of occurences of each element of seq, return as a map
   from element to count."
  (reduce (fn [m x]
            (update-in m [(keyword x)] (fnil inc 0)))
          {}
          xs))

(defn stem-count
  "Return a hashmap stem -> num-docs-appearing-in of all the stems that appear in a train/test set"
  [dir files]
  (->> files
       (map (partial str dir))           ;; Their full filename
       (map slurp)                       ;; Read em
       (mapcat stem/stems)               ;; Turn each to stems and concat
       (take 1e6)                        ;; Prevent overflow
       count-map))                       ;; Count each stem occurrance


(def stats-graph
  (graph/eager-compile
   {:files      (fnk [dir]
                     (fs/list-dir dir))
    :stem-count (fnk [dir files]
                     (stem-count dir files))}))

(def g
  (stats-graph (-> data-files :class-0 :train)))
