(ns core-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hodur-contentful-schema.core :as contentful]
            [hodur-engine.core :as engine]
            [lambdaisland.deep-diff :as diff]))

(defn ^:private schema->contentful [path]
  (-> path
      engine/init-path
      (contentful/schema {:space-id "oewsurrg31ok"})))

(deftest test-all
  (let [s-target (-> "sample.json" io/resource slurp json/parse-string)
        s-mine   (-> "schema.edn" io/resource schema->contentful json/parse-string)]

    (diff/pretty-print (diff/diff s-target s-mine))

    (is (= s-target s-mine))))
