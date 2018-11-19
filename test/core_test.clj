(ns core-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hodur-contentful-schema.core :as contentful]
            [hodur-engine.core :as engine]))

(defn ^:private schema->contentful [path]
  (-> path
      engine/init-path
      (contentful/schema {:space-id "oewsurrg31ok"})))

(deftest test-all
  (let [s-mine   (-> "schema.edn" io/resource schema->contentful)
        s-target (-> "sample.json" io/resource slurp)]
    (is (= s-target s-mine))))
