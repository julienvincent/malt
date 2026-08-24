(ns io.julienvincent.kondo-test
  (:require
   [io.julienvincent.malt :as malt]
   [malli.core :as m]))

(def ?SchemaReference :int)

(malt/defrecord Point
  [x :int
   y ?SchemaReference])

(malt/defrecord UnusedSchemaVars
  [x :int])
(->UnusedSchemaVars 1)

(map->Point {:x 1 :y 1})
(map->Point {:x "asd" :y 1})
(->Point 1 1)
(->Point "1" "1")

(malt/defprotocol Example
  (a
    "Some docs about this API"
    {:this-is "some-metadata"}
    [a ?SchemaReference b :int] ?SchemaReference)
  (b
    {:some-thing "meta"}
    [a :int] :int)

  (c [a :int b :int] ?SchemaReference))

(def example
  (reify Example
    (a [_ _ _] 1)
    (b [_ a] a)
    (c [_ _ _] 1)))

(a example 2 3)
(b example 2)
(c example 2 2)
(c example "asd" "asd")

(m/validate ?Example example)

(def example2
  (malt/reify Example
    (a [_ _ _] 1)
    (b [_ a] a)
    (c [_ a _b] a)))

(a example2 2 3)
(b example2 2)
(c example2 1 "asd")

;; Checking formatting
(malt/reify
  Example
  (a [_ _ _] 1)
  (b [_ a] a)
  (c [_ a _b] a))

(malt/extend-type String
  Example
  (a [_ a _b] a)
  (b [_ a] a)
  (c [_ a _b] a))

(malt/extend-type Long
  Example
  (a [_ _ _]
    "lol")
  (b [_ _] nil)
  (c [_ a _b] a))

(b "" 1)
(b "" "asd")
(a 1 1 1)
(c "asd" "asd" "asd")

(m/validate ?Point (->Point 1 1))
(m/validate ?PointSchema {:x 1
                          :y 1})

(malt/defprotocol ExampleWithDocs
  "This is what the protocol does"
  (get-docs [a :int] :string))

(get-docs
 (reify ExampleWithDocs
   (get-docs [_ _] ""))
 1)

(malt/defprotocol NoArgs
  (no-args [] [:vector :string]))

(def with-no-args
  (reify NoArgs
    (no-args [_] "")))

(no-args with-no-args)

;; Checked exceptions
(def not_found
  {:code :not_found
   :schema [:map [:id :string]]
   :message "Resource not found"
   :metadata {:http/status-code 404}})

(def conflict
  {:code :conflict
   :message "Resource conflict"
   :schema [:map [:resource :string]]})

(def timeout
  {:class java.util.concurrent.TimeoutException
   :metadata {:http/status-code 504}})

(malt/defprotocol CheckedExample
  (find-resource! [id :string]
    :nil
    (throws [not_found java.io.IOException]))

  (create-resource! [data :any]
    :nil
    (throws [not_found conflict timeout Exception])))

(find-resource!
 (reify CheckedExample
   (find-resource! [_ _] nil)
   (create-resource! [_ _] nil))
 "abc")

(create-resource!
 (reify CheckedExample
   (find-resource! [_ _] nil)
   (create-resource! [_ _] nil))
 {})

(malt/defprotocol MultiArityExample
  (resolve-resource!
    "Resolve a resource using one of the supported call forms."
    {:operation :resolve-resource}
    ([]
     :nil)
    ([resource ?SchemaReference]
     ?SchemaReference)
    ([resource ?SchemaReference fallback :string]
     [:vector ?SchemaReference]
     (throws [not_found]))))

(def multi-arity-example
  (malt/reify MultiArityExample
    (resolve-resource! [_] nil)
    (resolve-resource! [_ resource] resource)
    (resolve-resource! [_ resource _fallback] [resource])))

(resolve-resource! multi-arity-example)
(resolve-resource! multi-arity-example 1)
(resolve-resource! multi-arity-example 1 "fallback")

(malt/defrecord MultiArityRecord
  []
  MultiArityExample
  (resolve-resource! [_] nil)
  (resolve-resource! [_ resource] resource)
  (resolve-resource! [_ resource _fallback] [resource]))

(def multi-arity-record (->MultiArityRecord))
(resolve-resource! multi-arity-record)
(resolve-resource! multi-arity-record 1)
(resolve-resource! multi-arity-record 1 "fallback")

(malt/extend-type String
  MultiArityExample
  (resolve-resource! [_] nil)
  (resolve-resource! [_ resource] resource)
  (resolve-resource! [_ resource _fallback] [resource]))

(resolve-resource! "resource")
(resolve-resource! "resource" 1)
(resolve-resource! "resource" 1 "fallback")
