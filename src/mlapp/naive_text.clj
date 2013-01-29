(ns mlapp.naive-text
  "The text classification using Naive Bayes and beta-binomial model"
  (:require [fs.core :as fs]
            [stemmers.core :as stem]))

;; -----------------------------------------------------------------------------
;; Read the files
(def data-files
  {:class-0 {:train "./resources/data/20news/20news-bydate-train/comp.os.ms-windows.misc/"
             :test  "./resources/data/20news/20news-bydate-test/comp.os.ms-windows.misc/"}
   :class-1 {:train "./resources/data/20news/20news-bydate-train/comp.windows.x/"
             :test  "./resources/data/20news/20news-bydate-test/comp.windows.x/"}})

(defn stem-set
  "Return a set of all the stems that appear in a train/test set"
  [class set]
  (let [dir (-> data-files class set)]
    (->> (fs/list-dir dir)
         (map (partial str dir))
         (map slurp)
         (mapcat stem/stems)
         distinct
         (into #{}))))

;; Have a look
(let [class-0-train (stem-set :class-0 :train)
      class-0-train (stem-set :class-1 :train)
      class-0-test  (stem-set :class-0 :test)
      class-1-test  (stem-set :class-1 :test)]
  (take 200 class-0-train))
