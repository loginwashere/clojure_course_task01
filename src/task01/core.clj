(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

;; Подход правильный, есть только небольшие замечания (ниже по тексту)

(defn- is-link-container? [item]
  (and (> (count (children item)) 0)
       (map? (attributes item))
       (= "r" (:class (attributes item)))))

(defn- get-link-from-container [node]
;; если функции без аргументов, то можно их записывать и без скобок (включая и keyword). т.е. это будет выглядеть
;; вот так (-> (children node) first attributes :href)
  (-> (children node) (first) (attributes) (get :href)))

(defn- process-tree [node elements]
;; в данном случае cond используется как if, так что лучше либо использовать if, либо изменить cond чтобы он обрабатывал
;; больше условий. Например,
;; (cond
;;    (not (vector? ....) ...
;;    (is-link-container? ...)
;;    :else ...

  (cond
    (not (vector? node)) elements
    :else
      (cond
        ;; тут есть небольшая потенциальная ошибка - предполагается что у "детей" этой ноды не может быть ссылок
        (is-link-container? node) (conj elements (get-link-from-container node))
        :else
          (loop [items node
                 result elements]
            ;; тут if будет смотреться лучше - за счет необходимости явно писать :else
            (cond
              (empty? items) result
              :else
                (recur (next items) (process-tree (first items) result)))))))

(defn get-links []
" 1) Find all elements containing {:class \"r\"}.

Example:
[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

The link from the example above is 'https://github.com/clojure/clojure'.

  3) Return vector of all 10 links.

Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
"
  (let [data (parse "clojure_google.html")]
    (process-tree data [])))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))
