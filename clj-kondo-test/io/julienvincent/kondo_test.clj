(ns io.julienvincent.kondo-test
  (:require [io.julienvincent.malt :as malt]))

(def some-malli-input-schema :int)
(def some-malli-output-schema :int)

(malt/defprotocol Example
  (a "Some docs about this API"
    {:this-is "some-metadata"}
    [some-malli-input-schema some-malli-input-schema] some-malli-output-schema)
  (b
    {:some-thing "meta"}
    [some-malli-input-schema] some-malli-output-schema)

  (c [some-malli-input-schema some-malli-input-schema] some-malli-output-schema))

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
