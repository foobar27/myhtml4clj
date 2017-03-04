(ns myhtml4clj.core
  (:import [com.github.foobar27.myhtml4j Myhtml4j Sink]
           [java.util.function Supplier]))

(def sink
  (reify Sink
    (createText [this text]
      {:type :text
       :text text})
    (createComment [this text]
      {:type :comment
       :text text})
    (createElement [this ns tag attributes children]
      {:type :element
       :tag (keyword (.toString tag))
       :attributes (into {}
                         (for [a attributes]
                           [(-> a .getKey .toString keyword)
                            (-> a .getValue)]))
       :children (vec children)})))

(def sink-supplier
  (reify Supplier
    (get [this]
      sink)))

(defn parse [^java.lang.String html]
  (.getRoot (.parseUTF8 (Myhtml4j/getInstance) ^String html ^Supplier sink-supplier)))

