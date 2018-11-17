(ns hodur-contentful-schema.core
  (:require [camel-snake-kebab.core :refer [->Camel_Snake_Case_String]]
            [cheshire.core :as json]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.query-v3 :as q]))

(defn ^:private get-types [conn]
  (d/q '[:find [(pull ?t [* {:type/implements [*]
                             :field/_parent
                             [* {:field/type [*]}]}]) ...]
         :where
         [?t :type/name]
         [?t :contentful/tag true]
         [?t :type/nature :user]]
       @conn))

(defn ^:private display-name [{:keys [contentful/display-name] :as type-or-field}]
  (or display-name
      (let [type-name (:type/name type-or-field)
            field-name (:field/name type-or-field)]
        (-> (or type-name field-name)
            ->Camel_Snake_Case_String
            (string/replace #"_" " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private field-type-nature [field]
  (-> field :field/type :type/nature))

(defn ^:private field-card-type [{:keys [field/cardinality] :as field}]
  (if (and (= 1 (first cardinality))
           (= 1 (second cardinality)))
    :ref-one
    :ref-many))

(defn ^:private field-type-logic [field]
  (let [contentful-type (-> field :contentful/type)
        ref-type (-> field :field/type :type/name)
        card-type (field-card-type field)
        ref-type-nature (field-type-nature field)]
    (if contentful-type
      (if (and (= "Symbol" contentful-type)
               (= :ref-many card-type))
        card-type
        contentful-type)
      (if (or (= :user ref-type-nature)
              (and (= "String" ref-type)
                   (= :ref-many card-type)))
        card-type
        ref-type))))

(defmulti ^:private field-type field-type-logic)

(defmethod field-type "String" [_] "Symbol")

(defmethod field-type "Float" [_] "Number")

(defmethod field-type "Integer" [_] "Integer")

(defmethod field-type "Boolean" [_] "Boolean")

(defmethod field-type "DateTime" [_] "Date")

(defmethod field-type "ID" [_] "Symbol")

(defmethod field-type :ref-one [_] "Link")

(defmethod field-type :ref-many [_] "Array")

(defmethod field-type :default [{:keys [contentful/type]}] type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private build-ref-many []
  {:type })

(defn ^:private parse-content-type-field [{:keys [field/camelCaseName
                                                  field/optional] :as field}]
  (cond-> {:id (str camelCaseName)
           :name (display-name field)
           :type (field-type field)
           :localized false
           :required (not optional)
           :validations []
           :disabled false
           :omitted false}

    (= :ref-many (field-type-logic field))
    (assoc :items (build-ref-many field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-type-fields [fields]
  (reduce (fn [c field]
            (conj c (parse-content-type-field field)))
          [] fields))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-type [{:keys [type/camelCaseName type/doc field/_parent] :as t}
                                    {:keys [space-id]}]
  {:sys {:space {:sys {:type "Link"
                       :link-type "Space"
                       :id space-id}}
         :id (name camelCaseName)
         :type "ContentType"}
   :name (display-name t)
   :description doc
   :fields (parse-content-type-fields _parent)})

(defn ^:private parse-editor-type [t]
  {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-types [types opts]
  (reduce (fn [c t]
            (conj c (parse-content-type t opts)))
          [] types))

(defn ^:private parse-editor-interfaces [types opts]
  (reduce (fn [c t]
            (conj c (parse-editor-type t)))
          [] types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn schema
  ([conn]
   (schema conn nil))
  ([conn opts]
   (let [types (get-types conn)]
     {:content-types     (parse-content-types types opts)
      :editor-interfaces (parse-editor-interfaces types opts)})))




(require '[hodur-engine.core :as engine])
(require '[clojure.java.io :as io])

(let [meta-db (engine/init-path (io/resource "schema.edn"))]
  (get-types meta-db))

(let [meta-db (engine/init-path (io/resource "schema.edn"))]
  (clojure.pprint/pprint (schema meta-db {:space-id "oewsurrg31ok"})))
