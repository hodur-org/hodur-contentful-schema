[circleci-badge]: https://circleci.com/gh/hodur-org/hodur-contentful-schema.svg?style=shield&circle-token=146b7ab96c4056373f64b69de3200c2485266c1f
[circleci]: https://circleci.com/gh/hodur-org/hodur-contentful-schema
[clojars-badge]: https://img.shields.io/clojars/v/hodur/contentful-schema.svg
[clojars]: http://clojars.org/hodur/contentful-schema
[contentful]: https://contentful.com/
[contentful-cli]: https://www.contentful.com/developers/docs/tutorials/general/import-and-export/
[contentful-config]: https://github.com/contentful/contentful-import/blob/master/example-config.json
[contentful-types]: https://www.contentful.com/developers/docs/references/content-management-api/#/reference/content-types/content-type
[contentful-editor]: https://www.contentful.com/developers/docs/concepts/editor-interfaces/
[github-issues]: https://github.com/hodur-org/hodur-contentful-schema/issues
[hodur-engine]: https://github.com/hodur-org/hodur-engine
[hodur-engine-clojars-badge]: https://img.shields.io/clojars/v/hodur/engine.svg
[hodur-engine-clojars]: http://clojars.org/hodur/engine
[hodur-engine-definition]: https://github.com/hodur-org/hodur-engine#model-definition
[hodur-engine-started]: https://github.com/hodur-org/hodur-engine#getting-started
[license-badge]: https://img.shields.io/badge/license-MIT-blue.svg
[license]: ./LICENSE
[logo]: ./docs/logo-tag-line.png
[motivation]: https://github.com/hodur-org/hodur-engine/blob/master/docs/MOTIVATION.org
[plugins]: https://github.com/hodur-org/hodur-engine#hodur-plugins
[status-badge]: https://img.shields.io/badge/project%20status-beta-brightgreen.svg
[nodejs]: https://nodejs.org/en/

# Hodur Contentful Schema

[![CircleCI][circleci-badge]][circleci]
[![Clojars][hodur-engine-clojars-badge]][hodur-engine-clojars]
[![Clojars][clojars-badge]][clojars]
[![License][license-badge]][license]
![Status][status-badge]

![Logo][logo]

Hodur is a descriptive domain modeling approach and related collection
of libraries for Clojure.

By using Hodur you can define your domain model as data, parse and
validate it, and then either consume your model via an API making your
apps respond to the defined model or use one of the many plugins to
help you achieve mechanical, repetitive results faster and in a purely
functional manner.

> This Hodur plugin provides the ability to generate
> [Contentful][contentful] schemas out of your Hodur model. You can
> then apply your schema to your Contentful environments.

## Motivation

For a deeper insight into the motivations behind Hodur, check the
[motivation doc][motivation].

## Getting Started

Hodur has a highly modular architecture. [Hodur Engine][hodur-engine]
is always required as it provides the meta-database functions and APIs
consumed by plugins.

Therefore, refer the [Hodur Engine's Getting
Started][hodur-engine-started] first and then return here for
Datomic-specific setup.

After having set up `hodur-engine` as described above, we also need to
add `hodur/contentful-schema`, the plugin that creates Contentful
Schemas out of your model to the `deps.edn` file:

``` clojure
  {:deps {hodur/engine            {:mvn/version "0.1.6"}
          hodur/contentful-schema {:mvn/version "0.1.1"}}}
```

You should `require` it any way you see fit:

``` clojure
  (require '[hodur-engine.core :as hodur])
  (require '[hodur-contentful-schema.core :as hodur-contentful])
```

Let's expand our `Person` model from the original getting started by
"tagging" the `Person` entity for Contentful. You can read more about
the concept of tagging for plugins in the sessions below but, in
short, this is the way we, model designers, use to specify which
entities we want to be exposed to which plugins.

``` clojure
  (def meta-db (hodur/init-schema
                '[^{:contentful/tag-recursive true}
                  Person
                  [^String first-name
                   ^String last-name]]))
```

The `hodur-contentful-schema` plugin exposes a function called
`schema` that generates your model as a Contentful schema payload:

``` clojure
  (def contentful-schema (hodur-contentful/schema meta-db {:space-id "<YOUR_SPACE_ID>"))
```

You should replace `<YOUR_SPACE_ID>` with the space id of your
Contentful instance.

When you inspect `contentful-schema`, this is what you have:

``` json
  {
    "contentTypes" : [ {
      "sys" : {
        "space" : {
          "sys" : {
            "type" : "Link",
            "linkType" : "Space",
            "id" : "<YOUR_SPACE_ID>"
          }
        },
        "id" : "person",
        "type" : "ContentType",
        "publishedVersion" : 1
      },
      "name" : "Person",
      "description" : null,
      "fields" : [ {
        "id" : "firstName",
        "name" : "First Name",
        "type" : "Symbol",
        "localized" : false,
        "required" : true,
        "validations" : [ ],
        "omitted" : false,
        "disabled" : false
      }, {
        "id" : "lastName",
        "name" : "Last Name",
        "type" : "Symbol",
        "localized" : false,
        "required" : true,
        "validations" : [ ],
        "omitted" : false,
        "disabled" : false
      } ]
    } ],
    "editorInterfaces" : [ {
      "sys" : {
        "id" : "default",
        "type" : "EditorInterface",
        "space" : {
          "sys" : {
            "type" : "Link",
            "linkType" : "Space",
            "id" : "<YOUR_SPACE_ID>"
          }
        },
        "contentType" : {
          "sys" : {
            "id" : "person",
            "type" : "Link",
            "linkType" : "ContentType"
          }
        }
      },
      "controls" : [ {
        "fieldId" : "firstName",
        "widgetId" : "singleLine"
      }, {
        "fieldId" : "lastName",
        "widgetId" : "singleLine"
      } ]
    } ]
  }
```

In order to import the model above to your Contentful space, first
make sure you have [NodeJS installed][nodejs], then save the JSON
returned from `schema` to a file (i.e. `my-model.json`).

You will also need your Contentful settings to either on a
`contentful-config.json` file to run the [Contentful
CLI][contentful-cli]. More info on the [config file
here][contentful-config]. For the purposes of this getting started,
I'm using something along the lines of:

``` json
{
  "spaceId": "<YOUR_SPACE_ID>",
  "managementToken": "<YOUR_MANAGEMENT_TOKEN>"
}
```

Then you can run the importer with the following command:

``` bash
$ npx contentful-cli space import --config contentful-config.json --content-file my-model.json
```

You can also specify the environment you are importing the model to
with the parameter `--environment-id`.

## Model Definition

All Hodur plugins follow the [Model
Definition][hodur-engine-definition] as described on Hodur [Engine's
documentation][hodur-engine].

## Setting Display Name

The display name of entities and fields can be controlled by using the
marker `:contentful/display-name`:

``` clojure
  [^{:contentful/display-name "My Dream List"}
   Dream
   [^{:contentful/display-name "The Dream Title"}
    title]]
```

If no `:contentful/display-name` is provided, the plugin will default
to a capitalized version of the entity or field name.

## Making a Field the Display of an Entity

Contentful uses one of the fields of each entity as a visual
identifier for editors on its admin interface.

In order to specify which field is used for it, mark it with
`:contentful/display-field true`.

## Documenting Entities and Fields

The marker `:doc` is fully supported. Both entity and field
documentations will show on the admin for editors.

## Specifying Contentful Types

Contentful-specific types can be specified by using the marker
`:contentful/type`.

The supported basic types are:

- `Symbol` (short text - default for `String`)
- `Text` (long text)
- `Integer` (default for `Integer`)
- `Number` (default for `Float`)
- `Date` (default for `DateTime`)
- `Boolean` (default for `Boolean`)
- `Object`
- `Location`
- `RichText`

In general you don't need to specify the following ones because they
are managed internally by the plugin but, for reference:

- `Array` (default for any multiple `cardinality`)
- `Link` (default for linking to one asset and other user-specified entities)

Last but not least, you can also specify `Asset` as a special type
that will point to an asset (or more if cardinality is many) on the
digital asset manager:

- `Asset`

## Asset Fields

By specifying `:contentful/type "Asset"` you are letting Contentful
know that a certain field should be associated with and asset from the
digital asset manager.

This field can also have cardinality of many (`[0 n]`) and it should
let editors choose several assets for it.

Please refer to the section describing [further validations down
below](#field-validations) for examples on how to limit to certain
kind of assets (images for instance) and certain image features.

Also, refer to the [widget configuration
below](#choosing-widget-for-fields) as there are different widgets
that can be used for asset selection.

## ID Fields

`ID` fields are sent to Contentful as `Symbol` by default. Please do
provide a different `:contentful/type` if you need something else.

In addition, `ID` fields are automatically marked as unique by
default. If you prefer to control this more granularly, use a more
basic data type (`String` i.e.) and detail your validations manually
as documented in the section below.

## Enum Fields

Fields that point to `:enum` entities will be sent to Contentful as
`Symbol` by default. If you need a different type, please provide it
via `:contentful/type`.

By default the values of the enum are used as an `:in` validation for
the field. Therefore, the editor will be constrained to select one of
the options.

A `dropdown` widget is chosen by default in order to help editors
understand the selection. If you prefer a different rendering (such as
a `radio` or a `singleLine`) you can specify it with the
`:contentful/widget-id` marker as documented in the respective section
below.

## Field Validations

This plugin acts as a pass-through to the validations specified on
marker `:contentful/validations`. This marker, when specified, must be
an array of at least one entry. The full documentation of all the
[field validations available on Contentful can be found
here][contentful-types].

Here's an example showing some of these combined. They are pretty
self-explanatory:

``` clojure
  [ValidationEntity
   [;; will validate that `platform-field` is either `iOS` or `Android`
    ^{:type String
      :contentful/validations [{:in ["iOS" "Android"]}]}
    platform-field

    ;; will validate that `range-field` is between 5 and 15 with a custom message
    ^{:type Integer
      :contentful/validations [{:range {:min 5
                                        :max 15}
                                :message "Must be between 5 and 15"}]}
    range-field

    ;; will validate that `regexp-field` follows regexp `/^such/im`
    ^{:type String
      :contentful/validations [{:regexp {:pattern "^such"
                                         :flags "im"}}]}
    regexp-field

    ;; will validate that `unique-field` is unique
    ^{:type String
      :contentful/validations [{:unique true}]}
    unique-field

    ;; will validate that `date-range-field` is between the min and max date
    ^{:type DateTime
      :contentful/validations [{:date-range {:min "2017-05-01"
                                             :max "2020-05-01"}}]}
    date-range-field

    ;; will validate that `enabled-node-types-field` has only the specified node types active
    ^{:type String
      :contentful/type "RichText"
      :contentful/validations [{:enabled-node-types ["heading-1"
                                                     "quote"
                                                     "embedded-entry-block"]}]}
    enabled-node-types-field

    ;; will validate that `enabled-marks-field` has only the specified marks enabled
    ^{:type String
      :contentful/type "RichText"
      :contentful/validations [{:enabled-marks ["bold" "italics"]}]}
    enabled-marks-field

    ;; will validate that `multiple-validations-field` is both foo or bar, and between
    ;; 2 and 5 characters with custom messages
    ^{:type String
      :contentful/validations [{:in ["foo" "bar"]
                                :message "Should be foo or bar"}
                               {:size {:min 2
                                       :max 5}
                                :message "Should have 2 to 5 characters"}]}
    multiple-validations-field

    ;; will validate that `multiple-asset-validations-field` is an image, within certain
    ;; dimensions foo or bar, and certain byte size between with custom messages
    ^{:contentful/type "Asset"
      :contentful/validations [{:link-mimetype-group ["image"]
                                :message "Must be of MIME-Type image"}
                               {:asset-image-dimensions
                                {:width {:min 100
                                         :max 1000}
                                 :height {:min 200
                                          :max 2300}}
                                :message "Width must be 100-1000 and height 200-2300"}
                               {:asset-file-size {:min 1048576
                                                  :max 8388608}
                                :message "File must be between 1048576B and 8388608B"}]}
    multiple-asset-validations-field]]
```

## Choosing Widget for Fields

In order to make the experience more interesting for editors,
Contentful supports several dedicated widgets. A widget for a field
can be specified with the marker `:contentful/widget-id`. If a widget
is not specified a reasonable default one will be selected.

A full list of the [available widgets can be found
here][contentful-editor]. As of this writing, the options are:

| Widget ID            | Applicable field types        | Description                                                                                                     |
|----------------------|-------------------------------|-----------------------------------------------------------------------------------------------------------------|
| `assetLinkEditor`    | Asset                         | Search, attach, and preview an asset.                                                                           |
| `assetLinksEditor`   | Asset (array)                 | Search, attach, reorder, and preview multiple assets.                                                           |
| `assetGalleryEditor` | Asset (array)                 | Search, attach, reorder, and preview multiple assets in a gallery layout                                        |
| `boolean`            | Boolean                       | Radio buttons with customizable labels.                                                                         |
| `datePicker`         | Date                          | Select date, time, and timezone.                                                                                |
| `entryLinkEditor`    | Entry                         | Search and attach another entry.                                                                                |
| `entryLinksEditor`   | Entry (array)                 | Search and attach multiple entries.                                                                             |
| `entryCardEditor`    | Entry                         | Search, attach, and preview another entry.                                                                      |
| `entryCardsEditor`   | Entry (array)                 | Search, attach and preview multiple entries.                                                                    |
| `numberEditor`       | Integer, Number               | A simple input for numbers.                                                                                     |
| `rating`             | Integer, Number               | Uses stars to select a number.                                                                                  |
| `locationEditor`     | Location                      | A map to select or find coordinates from an address.                                                            |
| `objectEditor`       | Object                        | A code editor for JSON                                                                                          |
| `urlEditor`          | Symbol                        | A text input that also shows a preview of the given URL.                                                        |
| `slugEditor`         | Symbol                        | Automatically generates a slug and validates its uniqueness across entries.                                     |
| `listInput`          | Symbol (array)                | Text input that splits values on , and stores them as an array.                                                 |
| `checkbox`           | Symbol (array)                | A group of checkboxes. One for each value from the in validation on the content type field                      |
| `tagEditor`          | Symbol (array)                | A text input to add a string to the list. Shows the items as tags and allows to remove them.                    |
| `multipleLine`       | Text                          | A simple <textarea> input                                                                                       |
| `markdown`           | Text                          | A full-fledged markdown editor                                                                                  |
| `singleLine`         | Text, Symbol                  | A simple text input field                                                                                       |
| `dropdown`           | Text, Symbol, Integer, Number | A <input type="select"> element. It uses the values from an in validation on the content type field as options. |
| `radio`              | Text, Symbol, Integer, Number | A group of radio buttons. One for each value from the in validation on the content type field                   |


Here's a simple example:

``` clojure
  [MarketingEntry
   [^{:type String
      :contentful/widget-id "urlEditor"} url
    ^{:type Integer
      :contentful/widget-id "rating"}    stars]]
```

## Widget Settings

With the exception of the help text, all other widget settings are
available via Hodur with specific markers.

For `boolean` widget:

- `:contentful/true-label` : Shows this text next to the radio button
  that sets this value to `true`. Defaults to "Yes".
- `:contentful/false-label` : Shows this text next to the radio button
  that sets this value to `false`. Defaults to "No".

For `rating` widget:

- `:contentful/stars` : Number of stars to select from. Defaults to 5.

For `datePicker` widget:

- `:contentful/format` : One of "dateonly", "time", "timeZ"
  (default). Specifies whether to show the clock and/or timezone
  inputs.
- `:contentful/ampm` : Specifies which type of clock to use. Must be
  one of the strings "12" or "24" (default).

Example:

``` clojure
  [Entity
   [^{:type Integer
      :contentful/widget-id "rating"
      :contentful/stars 10}
    stars-field

    ^{:type Boolean
      :contentful/true-label "Si!"
      :contentful/false-label "No!"}
    si-o-no-field

    ^{:type DateTime
      :contentful/format "dateonly"}
    date-only-field

    ^{:type DateTime
      :contentful/format "time"}
    time-field

    ^{:type DateTime
      :contentful/format "timeZ"}
    full-date-time-field

    ^{:type DateTime
      :contentful/ampm "12"}
    american-style-time-field]]
```

## Limitations & Assumptions

- This plugin ignores `interfaces` and field parameters.
- `PascalCasing` is used on naming entities and `camelCasing` is used
  on all fields
- There are no validations on the widgets, validations, or other
  Contentful-specific markers. They are simply passed over to
  Contentful.
- Unions are supported by creating entry relationships that support
  multiple content types.

## Bugs

If you find a bug, submit a [GitHub issue][github-issues].

## Help!

This project is looking for team members who can help this project
succeed! If you are interested in becoming a team member please open
an issue.

## License

Copyright © 2019 Tiago Luchini

Distributed under the MIT License (see [LICENSE][license]).
