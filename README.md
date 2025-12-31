<div align="center">
  <h1>malt</h1>

  <p>
    Typed Clojure protocols using Malli schemas.
  </p>

[![Clojars Project][clojars-badge]][clojars-link]

</div>

---

## What is this?

Malt is a small layer on top of Clojure protocols and records that lets you attach Malli schemas to protocol methods and
record constructors. The output is a native Clojure protocol or record, so tooling and language features keep working,
but you also gain the ability to perform runtime validation of inputs and outputs. The result is a protocol interface
that is concrete, self-documenting, and usable as a real boundary between parts of a system.

The intent is to make it easy to express contracts between components without introducing a heavy type system. If you
already use Malli, you can reuse its schemas for validation, documentation, and generation. If you just want runtime
checks for tricky boundaries, you can use the macros only in those places.

Malt fully integrates with clj-kondo enabling deep editor integration into the macro syntax. All built-in language
features for protocols and records such as `find-references`, `find-definitions`, and `find-implementations` continue to
work the way you would expect. Your editor will understand the syntax out-of-the-box with no need for finiky
configurations or tweaks.

## Quick example

```clojure
(ns example
  (:require
    [io.julienvincent.malt :as malt]
    [malli.core :as m]))

(def ?Plumburg
  [:map
   [:name :string]
   [:edges :int]])

;; Define a protocol using parameter/schema pairs.
(malt/defprotocol Api
  (create-plumburg [name :string edges :int]
    ?Plumburg))

;; Using `malt/defrecord` works identically to `clojure.core/defrecord` but
;; overrides the generated `->Type` and `map->Type` constructors to add
;; schema validation.
(malt/defrecord Service
  [db ?DataSource]

  Api
  (create-plumburg [_service name edges]
    (write-to-db db name edges)))

(create-plumburg (->Service db) "fred" "2")
;; => throws input validation exception

(defn create-service [db]
  ;; Use `malt/reify` instead of `clojure.core/reify` to implement a protocol
  ;; with schema validation
  (malt/reify Api
    (create-plumburg [_ name edges]
      (write-to-db db name edges))))

(defn create-service-with-reify [db]
  ;; You can still use native `clojure.core/reify`, just without the schema
  ;; validation
  (reify Api
    (create-plumburg [_ name edges]
      (write-to-db db name edges))))

;; Some top-level malli schemas are also exported as vars which can be used to
;; validate record types.
(m/validate ?Service (->Service db))
```

## How it works

`malt/defprotocol` stores Malli schemas in the protocol var metadata. Later, when an implementation is created with
`malt/extend-type` or `malt/reify`, those implementations wrap the method bodies with validation. A method call
validates the inputs against the argument schema and validates the return value against the return schema. Validation
failures are raised as `ExceptionInfo` with structured error data.

`malt/defrecord` behaves like `clojure.core/defrecord` but also validates record constructors. Additionally, if you
inline protocol implementations inside a `malt/defrecord`, those implementations are also wrapped when the protocol
being implemented was defined with `malt/defprotocol`.

## Reference API

### `malt/defprotocol`

Used to define a typed Clojure protocol. The protocol remains native, but the schema metadata lets malt attach
validators and makes the contract visible to tools and humans.

- Accepts param/schema pairs in the method vector, followed by the return schema.
- Produces a normal Clojure protocol plus a `?ProtocolName` Malli schema var.

```clojure
(malt/defprotocol UserStore
  (create-user [name :string age :int] :string)
  (delete-user [id :string] :nil))
```

Method definitions differ slightly from `clojure.core/defprotocol` in that the docstring and metadata needs to be placed
before the params vector, instead of after. This makes the definition read more like `defn` and makes the return schema
clearer.

```clojure
(malt/defprotocol UserStore
  (create-user
    "Create a new user and return the id."
    {:audit/event :user.created}
    [name :string age :int]
    :string))
```

Additionally, the `this` parameter from `clojure.core/defprotocol` is completely omitted as we consider it unnecessary
due to being required by every method.

Exports:

- `UserStore`: the protocol var.
- `?UserStore`: Malli schema that checks `satisfies?` for the protocol.

##### The Protocol Var

The resulting Clojure protocol has additional information associated with it. This information is what is used by
`malt/reify`, `malt/extend-type`, and `malt/defrecord` to augment implementations with schema validations.

The data is considered part of the public API and it is fully the expectation that other tools, and you, can use the
protocol data to build on top of.

The protocol var (accessed by `#'ProtocolVar`) stores a `:sigs` map containing the underlying Clojure method
definitions, and malt additionally stores namespaced data there as well.

You can verify if a protocol is a malt protocol by checking the `:malt/protocol` field on the var:
`(:malt/protocol #'UserStore)`

Evaluating the protocol var shows the stored sigs:

```clojure
#'UserStore
{:malt/protocol true
 :sigs
 {:create-user
  {:malt/params [name age]
   ;; A map of :param-name -> Malli schema, useful for tools
   :malt/param-schemas {:name :string
                        :age :int}
   ;; A prepared schema for validating a function call arguments
   :malt/arguments-schema [:cat :string :int]
   :malt/return-schema :string
   ;; These are precompiled malli validators via `(m/validator ?schema)`
   :malt/arguments-validator #object[...]
   :malt/return-validator #object[...]}
  :delete-user
  {:malt/params [id]
   :malt/param-schemas {:id :string}
   :malt/arguments-schema [:cat :string]
   :malt/return-schema :nil
   :malt/arguments-validator #object[...]
   :malt/return-validator #object[...]}}}
```

### `malt/defrecord`

Inline protocol implementations are validated when the protocol was defined with `malt/defprotocol`. This lets records
serve as concrete, validated implementations while still validating their own construction.

```clojure
(malt/defrecord UserStoreImpl
  [db ?DataSource]

  UserStore
  (create-user [_ name age]
    (persist-user db name age))
  (delete-user [_ id]
    (delete-user! db id)))
```

- Accepts a vector of field/schema pairs, then optional protocol implementations.
- Overrides `->Record` and `map->Record` to validate constructor inputs.
- Produces `?RecordSchema` (map shape) and `?Record` (instance check) schemas.

Exports:

- `UserStoreImpl`: the record type.
- `->UserStoreImpl`: validated positional constructor.
- `map->UserStoreImpl`: validated map constructor.
- `?UserStoreImplSchema`: Malli `:map` schema for the record fields.
- `?UserStoreImpl`: Malli schema that checks `instance?` for the record.

### `malt/extend-type`

This is the main way to attach validation to concrete types without changing how you structure code. You can continue to
extend classes and records, but get consistent validation and error data at the protocol boundary.

`malt/extend-type` supports passing a var for a record in another namespace, which is a deviation from
`clojure.core/extend-type`. If you pass a qualified record var (for example `other.ns/SomeRecord`), malt resolves it to
the underlying class so you can extend external records without manually constructing the class name.

```clojure
(malt/extend-type UserStoreImpl
  UserStore
  (create-user [store name age]
    (persist-user (:db store) name age))
  (delete-user [store id]
    (delete-user! (:db store) id)))
```

- Accepts the same syntax as `clojure.core/extend-type`.
- Wraps method bodies to validate inputs and outputs.
- Produces a normal `extend-type` result with runtime validation on calls.

### `malt/reify`

This is useful for tests, adapters, and small inline implementations where you still want the protocol contract enforced
at runtime.

```clojure
(def in-memory-store
  (malt/reify UserStore
    (create-user [_ name age]
      (swap! users conj {:name name :age age})
      (java.util.UUID/randomUUID))
    (delete-user [_ id]
      (swap! users (partial remove #(= (:id %) id)))
      nil)))
```

- Accepts the same syntax as `clojure.core/reify`.
- Validates inputs and outputs for each protocol method.
- Produces an anonymous instance that satisfies the protocol.

### Validation errors

Validation failures throw `ExceptionInfo` with a `:type` in `ex-data` that you can reliably switch on. The intent is
that you can log or surface these errors without additional translation. The error data is structured enough to be
inspected and rendered usefully in tests and runtime logs.

- Errors are thrown as `ExceptionInfo`.
- `ex-data` includes `:type` and contextual keys like `:protocol`, `:method`, `:record`, `:constructor`, `:input`,
  `:output`, and `:errors`.
- `:errors` is produced by `malli.error/humanize`.

Error types (constructor-form examples):

#### `:malt/input-validation-failed`

Protocol input validation failed.

```clojure
(ex-info
 "Invalid parameter 'name' passed to 'create-user' of example/UserStore"
 {:type :malt/input-validation-failed
  :protocol 'example/UserStore
  :method 'create-user
  :input [123 '_]
  :errors [["should be a string"]]})
```

```clojure
(ex-info
 "Invalid parameter 'age' passed to 'create-user' of example/UserStore"
 {:type :malt/input-validation-failed
  :protocol 'example/UserStore
  :method 'create-user
  :input ['_ "not-an-int"]
  :errors [nil ["should be an integer"]]})
```

#### `:malt/output-validation-failed`

Protocol output validation failed.

```clojure
(ex-info
 "Invalid return value from 'delete-user' of example/UserStore"
 {:type :malt/output-validation-failed
  :protocol 'example/UserStore
  :method 'delete-user
  :output 1
  :errors ["should be nil"]})
```

#### `:malt/record-validation-failed`

Record constructor validation failed.

```clojure
(ex-info
 "Invalid parameter passed to constructor '->UserStoreImpl' of example/UserStoreImpl"
 {:type :malt/record-validation-failed
  :record 'example/UserStoreImpl
  :constructor '->UserStoreImpl
  :input [1]
  :errors [["should satisfy ?DataSource"]]})
```

## Formatting

Unfortunately cljfmt does not currently support loading config from the classpath like clj-kondo does, so you may want
to add custom indent rules:

```clojure
;; .cljfmt.edn
{:extra-indents {io.julienvincent.malt/extend-type [[:inner 0] [:inner 1]]
                 io.julienvincent.malt/reify [[:inner 0] [:inner 1]]
                 io.julienvincent.malt/defprotocol [[:inner 0] [:inner 1]]
                 io.julienvincent.malt/defrecord [[:inner 0] [:inner 1]]}}
```

See cljfmt issue [weavejester/cljfmt#336](https://github.com/weavejester/cljfmt/issues/336).

## Gotchas

If you are using the autogenerated `?Type` schemas from `malt/defprotocol` and `malt/defrecord`, note that clojure-lsp
does not always index those vars well. Go-to-references and rename may not include those schema vars.

See clojure-lsp issues:

- [x] [clojure-lsp/clojure-lsp#2176](https://github.com/clojure-lsp/clojure-lsp/issues/2176) (solved)
- [ ] [clojure-lsp/clojure-lsp#2183](https://github.com/clojure-lsp/clojure-lsp/issues/2183).

## Prior art

Prismatic Schema implemented a `defprotocol` for their schema language. They jumped through many hoops to get native
clojure reify, extend-type, etc. working with schema validation without custom macros. That comes at the cost of
performance because it disables method short-circuiting using var `:inline` metadata. malt uses a less native approach
in exchange for better runtime performance.

See implementation here:
[schema/macros.cljL417](https://github.com/plumatic/schema/blob/master/src/clj/schema/macros.clj).

[clojars-badge]: https://img.shields.io/clojars/v/io.julienvincent/malt.svg
[clojars-link]: https://clojars.org/io.julienvincent/malt
