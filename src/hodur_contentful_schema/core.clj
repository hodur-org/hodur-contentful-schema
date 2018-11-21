(ns hodur-contentful-schema.core
  (:require [camel-snake-kebab.core :refer [->Camel_Snake_Case_String
                                            ->camelCaseString]]
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
        ref-type (some-> field :field/type :type/PascalCaseName name)]
    (or contentful-type ref-type)))

(defmulti ^:private spec-field-type base-spec-field-type)

(defmethod spec-field-type "String" [_] "Symbol")

(defmethod spec-field-type "Float" [_] "Number")

(defmethod spec-field-type "Integer" [_] "Integer")

(defmethod spec-field-type "Boolean" [_] "Boolean")

(defmethod spec-field-type "DateTime" [_] "Date")

(defmethod spec-field-type "Id" [_] "Symbol")

(defmethod spec-field-type "Asset" [_] "Link")

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
    (map #(name (:field/PascalCaseName %)) (-> field :field/type :field/_parent))
    [(-> field :field/type :type/PascalCaseName name)]))

(defn ^:private get-base-validations [field]
  (cond
    (and (is-field-user-entity? field)
         (= :one (field-card-type field)))
    [{:link-content-type (get-link-content-types field)}]

    (and (= "ID" (-> field :field/type :type/name))
         (= :one (field-card-type field)))
    [{:unique true}]

    :otherwise
    []))

(defn ^:private get-field-validations [{:keys [contentful/validations] :as field}]
  (concat (get-base-validations field) validations))

(defn ^:private get-type-many [field]
  (cond
    (is-field-user-entity? field)
    {:type "Link"
     :link-type "Entry"
     :validations [{:link-content-type (get-link-content-types field)}]}

    (= "Asset" (base-spec-field-type field))
    {:type "Link"
     :link-type "Asset"
     :validations (get-field-validations field)}

    :otherwise
    {:type (spec-field-type field)
     :validations []}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private get-field-widget-logic [field]
  (let [contentful-widget-id (-> field :contentful/widget-id)
        spec-type (spec-field-type field)
        base-type (base-spec-field-type field)
        is-user? (is-field-user-entity? field)]
    (cond
      is-user?
      "Entry"

      (and (= "Asset" base-type)
           (not contentful-widget-id))
      base-type

      :otherwise
      (or contentful-widget-id spec-type))))

(defmulti ^:private get-field-widget get-field-widget-logic)

(defmethod get-field-widget "Symbol" [_] "singleLine")

(defmethod get-field-widget "Text" [_] "multipleLine")

(defmethod get-field-widget "Location" [_] "locationEditor")

(defmethod get-field-widget "Object" [_] "objectEditor")

(defmethod get-field-widget "Number" [_] "numberEditor")

(defmethod get-field-widget "Integer" [_] "numberEditor")

(defmethod get-field-widget "Boolean" [_] "boolean")

(defmethod get-field-widget "Date" [_] "datePicker")

(defmethod get-field-widget "RichText" [_] "richTextEditor")

(defmethod get-field-widget "Asset" [field]
  (if (= :one (field-card-type field))
    "assetLinkEditor"
    "assetLinksEditor"))

(defmethod get-field-widget "Entry" [field]
  (if (= :one (field-card-type field))
    "entryLinkEditor"
    "entryLinksEditor"))

(defmethod get-field-widget :default [{:keys [contentful/widget-id]}] widget-id)

(defn ^:private has-editor-settings? [{:keys [field/doc contentful/true-label
                                              contentful/false-label contentful/stars
                                              contentful/format contentful/ampm]}]
  (boolean (or doc true-label false-label stars format ampm)))

(defn ^:private get-editor-settings [{:keys [field/doc contentful/true-label
                                             contentful/false-label contentful/stars
                                             contentful/format contentful/ampm]}]
  (cond-> {}
    doc         (assoc :help-text doc)
    true-label  (assoc :true-label true-label)
    false-label (assoc :false-label false-label)
    stars       (assoc :stars stars)
    format      (assoc :format format)
    ampm        (assoc :ampm ampm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private omitted-field? [{:keys [contentful/omitted]}]
  (boolean omitted))

(defn ^:private disabled-field? [{:keys [contentful/disabled]}]
  (boolean disabled))

(defn ^:private parse-content-type-field [{:keys [field/camelCaseName
                                                  field/optional] :as field}]
  (cond-> {:id (name camelCaseName)
           :name (display-name field)
           :type (get-type-definition field)
           :localized false
           :required (not optional)
           :validations (get-field-validations field)
           :omitted (omitted-field? field)
           :disabled (disabled-field? field)}

    (and (is-field-user-entity? field)
         (= :one (field-card-type field)))
    (assoc :link-type "Entry")

    (and (= "Asset" (base-spec-field-type field))
         (= :one (field-card-type field)))
    (assoc :link-type "Asset")
    
    (= :many (field-card-type field))
    (assoc :items (get-type-many field))))

(defn ^:private parse-editor-field [{:keys [field/camelCaseName] :as field}]
  (cond-> {:field-id (name camelCaseName)
           :widget-id (get-field-widget field)}

    (has-editor-settings? field)
    (assoc :settings (get-editor-settings field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-type-fields [fields]
  (reduce (fn [c field]
            (conj c (parse-content-type-field field)))
          [] fields))

(defn ^:private parse-editor-fields [fields]
  (reduce (fn [c field]
            (conj c (parse-editor-field field)))
          [] fields))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private get-display-field [t]
  (some->> t
           :field/_parent
           (some (fn [{:keys [contentful/display-field
                              field/camelCaseName]}]
                   (when display-field (name camelCaseName))))))

(defn ^:private parse-content-type [{:keys [type/PascalCaseName type/doc field/_parent] :as t}
                                    {:keys [space-id]}]
  (let [display-field (get-display-field t)]
    (cond-> {:sys {:space {:sys {:type "Link"
                                 :link-type "Space"
                                 :id space-id}}
                   :id (name PascalCaseName)
                   :type "ContentType"
                   ;; FIXME: published-version here is a hack
                   ;; Contentful's server doesn't seem to use/need this node at all
                   ;; however, Contentful's CLI will skip importing editor interfaces
                   ;; if this node is not present at the endity definition
                   :published-version 1}
             :name (display-name t)
             :description doc
             :fields (parse-content-type-fields _parent)}

      display-field
      (assoc :display-field (->camelCaseString display-field)))))

(defn ^:private parse-editor-type [{:keys [type/PascalCaseName field/_parent] :as t}
                                   {:keys [space-id]}]
  {:sys {:id "default"
         :type "EditorInterface"
         :space {:sys {:type "Link"
                       :link-type "Space"
                       :id space-id}}
         :content-type {:sys {:id (name PascalCaseName)
                              :type "Link"
                              :link-type "ContentType"}}}
   :controls (parse-editor-fields _parent)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private parse-content-types [types opts]
  (reduce (fn [c t]
            (conj c (parse-content-type t opts)))
          [] types))

(defn ^:private parse-editor-interfaces [types opts]
  (reduce (fn [c t]
            (conj c (parse-editor-type t opts)))
          [] types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn schema
  ([conn]
   (schema conn nil))
  ([conn opts]
   (let [types (get-types conn)]
     (-> {:content-types     (parse-content-types types opts)
          :editor-interfaces (parse-editor-interfaces types opts)}
         (json/generate-string {:pretty true
                                :key-fn ->camelCaseString})))))


(comment
  (require '[hodur-engine.core :as engine])
  (require '[clojure.java.io :as io])

  (let [meta-db (engine/init-path (io/resource "schema.edn"))]
    (println (schema meta-db {:space-id "oewsurrg31ok"}))
    (spit "my-lovely-model.json" (schema meta-db {:space-id "oewsurrg31ok"}))))
