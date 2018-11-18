(ns hodur-contentful-schema.core
  (:require [camel-snake-kebab.core :refer [->Camel_Snake_Case_String]]
            [cheshire.core :as json]
            [clojure.string :as string]
            [datascript.core :as d]
            [datascript.query-v3 :as q]))

(defn ^:private get-types [conn]
  (let [selector '[* {:type/implements [*]
                      :field/_parent
                      [* {:field/type
                          [* {:field/_parent [*]}]}]}]
        eids (-> (q/q '[:find ?t
                        :where
                        [?t :type/name]
                        [?t :contentful/tag true]
                        [?t :type/nature :user]
                        (not [?t :type/interface true])
                        (not [?t :type/union true])]
                      @conn)
                 vec flatten)]
    (->> eids
         (d/pull-many @conn selector)
         (sort-by :type/name))))

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

(defn ^:private is-field-user-entity? [field]
  (= :user (field-type-nature field)))

(defn ^:private field-card-type [{:keys [field/cardinality] :as field}]
  (if (and (= 1 (first cardinality))
           (= 1 (second cardinality)))
    :one
    :many))

(defn ^:private base-spec-field-type [field]
  (let [contentful-type (-> field :contentful/type)
        ref-type (-> field :field/type :type/camelCaseName)]
    (or contentful-type ref-type)))

(defmulti ^:private spec-field-type base-spec-field-type)

(defmethod spec-field-type "String" [_] "Symbol")

(defmethod spec-field-type "Float" [_] "Number")

(defmethod spec-field-type "Integer" [_] "Integer")

(defmethod spec-field-type "Boolean" [_] "Boolean")

(defmethod spec-field-type "DateTime" [_] "Date")

(defmethod spec-field-type "ID" [_] "Symbol")

(defmethod spec-field-type :default [field]
  (base-spec-field-type field))

(defn ^:private get-type-definition
  [field]
  (let [spec-type (spec-field-type field)
        card-type (field-card-type field)]
    (if (= :many card-type)
      "Array"
      (if (is-field-user-entity? field)
        "Link"
        spec-type))))

(defn ^:private get-link-content-types [field]
  (if (-> field :field/type :type/union)
    (map #(name (:field/camelCaseName %)) (-> field :field/type :field/_parent))
    [(-> field :field/type :type/camelCaseName name)]))

(defn ^:private get-type-many [field]
  (if (is-field-user-entity? field)
    {:type "Link"
     :link-type "Entry"
     :validations [{:link-content-type (get-link-content-types field)}]}
    {:type (spec-field-type field)
     :validations []}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private get-field-validations [field]
  (if (and (is-field-user-entity? field)
           (= :one (field-card-type field)))
    [{:link-content-type (get-link-content-types field)}]
    []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-type-field [{:keys [field/camelCaseName
                                                  field/optional] :as field}]
  (cond-> {:id (name camelCaseName)
           :name (display-name field)
           :type (get-type-definition field)
           :localized false
           :required (not optional)
           :validations (get-field-validations field)
           :disabled false
           :omitted false}

    (= :one (field-card-type field))
    (assoc :link-type "Entry")
    
    (= :many (field-card-type field))
    (assoc :items (get-type-many field))))

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
  (clojure.pprint/pprint (schema meta-db {:space-id "oewsurrg31ok"})))


#_(let [meta-db (engine/init-path (io/resource "schema.edn"))]
    (schema meta-db {:space-id "oewsurrg31ok"}))


#_(get-type-many '{:field/snake_case_name :many_unions, :field/parent #:db{:id 7}, :field/kebab-case-name :many-unions, :field/cardinality [0 n], :contentful/tag true, :db/id 15, :field/type {:type/snake_case_name :a_union, :type/nature :user, :type/kebab-case-name :a-union, :type/union true, :contentful/tag true, :db/id 14, :type/name AUnion, :type/camelCaseName :aUnion, :type/PascalCaseName :AUnion}, :field/name many-unions, :field/camelCaseName :manyUnions, :field/PascalCaseName :ManyUnions})
