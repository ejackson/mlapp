(ns mlapp.naive-text
  "The text classification using Naive Bayes and beta-binomial model"
  (:use [plumbing.core])
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

(defn stem-seq
  "Return a seq of stems for each doc is dir"
  [dir files]
  (->> files
       (map (partial str dir))           ;; Their full filename
       (map slurp)                       ;; Read em
       (mapcat stem/stems)               ;; Turn each to stems and concat
       (take 1e6)                        ;; Prevent overflow
       ))

(def set-stats
  (graph/eager-compile
   {:files       (fnk [dir]
                     (fs/list-dir dir))
    :stems       (fnk [dir files]
                     (stem-seq dir files))
    :stem-counts (fnk [stems]
                     (frequencies-fast stems))
    :num-stems   (fnk [stem-counts]
                      (reduce + (vals stem-counts)))
    :stem-freqs  (fnk [stem-counts num-stems]
                      (map-vals #(/ % num-stems) stem-counts))
    :num-files   (fnk [files]
                     (count files))}))

(comment
  (def data2
    (-> data
        (update-in [:class-0 :train] set-stats)
        (update-in [:class-1 :train] set-stats))))
