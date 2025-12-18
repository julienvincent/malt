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

(def example2
  (malt/implement Example
    (a [_ _ _] 1)
    (b [_ a] a)
    (c [_ a _b] a)))

(a example2 2 3)
(b example2 2)
(c example2 1 "asd")

(malt/extend String
  Example
  (a [_ a _b] a)
  (b [_ a] a)
  (c [_ a _b] a))

(malt/extend Long
  Example
  (a [_ _ _] "lol")
  (b [_ _] nil)
  (c [_ a _b] a))

(b "" 1)
(b "" "asd")
(a 1 1 1)
(c "asd" "asd" "asd")

(m/validate ?Point (->Point 1 1))
(m/validate ?PointSchema {:x 1
                          :y 1})
