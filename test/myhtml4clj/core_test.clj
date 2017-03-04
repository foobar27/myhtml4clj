(ns myhtml4clj.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [myhtml4clj.core :refer :all])
  (:import [com.github.foobar27.myhtml4j.atoms Tags]))

(comment
  (import '[com.github.foobar27.myhtml4j Myhtml4j])
  (Myhtml4j/setNativeLoader (.fileLoader (Myhtml4j/getNativeLoaderFactory)
                                         (java.nio.file.Paths/get "/home/sebastien/dev/myhtml4j/cpp/build/release/src/libmyhtml4jnative.so.0.1.0" (make-array String 0)))))

(defn safe-parse [html]
  (try
    (parse html)
    (catch Exception e
      {:type :element
       :tag ::exception})))

;; TODO stub, lots of missing features
(defn ->html [root]
  (condp = (:type root)
    :element (let [tag (-> root :tag name)]
               (str "<"
                    tag
                    ">"
                    (apply str
                           (for [child (:children root)]
                             (->html child)))
                    "</"
                    tag
                    ">"))))

(defn node->structure [{:keys [tag children]}]
  [tag (vec (map node->structure children))])

(defn structure->node [[tag children]]
  {:type :element
   :tag tag
   :attributes {}
   :children (vec (map structure->node children))})

(deftest parsing
      (is (= (safe-parse "<div id=\"1\">")
             {:type :element
              :tag :html
              :attributes {}
              :children [{:type :element
                          :tag :head
                          :attributes {}
                          :children []}
                         {:type :element
                          :tag :body
                          :attributes {}
                          :children [{:type :element
                                      :tag :div
                                      :attributes {:id "1"}
                                      :children []}]}]})))

(deftest html-conversion
      (is (= (->html (safe-parse "<div>"))
             "<html><head></head><body><div></div></body></html>")))

(deftest structure-conversion
  (is (= (node->structure (safe-parse "<ul><li></li><li></li></ul>"))
         [:html [[:head []] [:body [[:ul [[:li []] [:li []]]]]]]]))
  (is (= (structure->node [:html [[:head []] [:body [[:ul [[:li []] [:li []]]]]]]])
         (parse "<ul><li></li><li></li></ul>"))))


(def all-tags
  (into #{} (filter #(not (= % (keyword ""))) (map #(-> % str keyword) (Tags/ALL_ATOMS)))))


(def tag-generator
  (gen/one-of [(gen/elements #{:div})
               (gen/fmap #(keyword (str "x" %)) gen/pos-int)]))

(def structure-compound
  (fn [inner-gen]
    (gen/fmap (fn [[tag children]]
                [tag (vec (remove #(= % ::terminal) children))])
              (gen/tuple tag-generator (gen/list inner-gen)))))

(def structure-generator
  (gen/recursive-gen structure-compound (gen/elements [::terminal])))

(defn verify-cycle [s]
  (= (-> s structure->node ->html safe-parse node->structure)
     [:html [[:head []] [:body [s]]]]))

(defspec parse-cycle
  100000
  (prop/for-all [s structure-generator]
                (verify-cycle s)))
