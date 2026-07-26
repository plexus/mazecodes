(ns mazecodes-ui
  (:require [mazecodes :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM helpers taken from Thicc

(defn query
  "Find a matching DOM element as per `querySelector`, either on the full
  document, or starting from the given element"
  ([selector]
   (js/document.querySelector selector))
  ([el selector]
   (.querySelector el selector)))

(defn query-all
  "Find all matching DOM elements as per `querySelector`, either on the full
  document, or starting from the given element"
  ([selector]
   (js/document.querySelectorAll selector))
  ([el selector]
   (.querySelectorAll el selector)))

(defn el-by-id
  "Find a DOM element in the document based on its `id` attribute"
  [id]
  (js/document.getElementById id))

;;;;;;;;;;


(defn on-click [e f]
  (.addEventListener e "click" f))

(defn write-code! []
  (set!
   (.-value
    (query "textarea"))

   (edn->code
    {:arrows (parse-long (.-value (el-by-id "arrows")))
     :coins  (parse-long (.-value (el-by-id "coins")))
     :keys   (parse-long (.-value (el-by-id "keys")))
     :items
     (into #{}
           (comp
            (filter #(query % "input:checked"))
            (map #(-> % .-dataset .-item))
            (map keyword))
           (query-all "label"))})))

(on-click (el-by-id "write-btn") write-code!)
