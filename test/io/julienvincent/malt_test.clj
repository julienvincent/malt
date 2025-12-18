(ns io.julienvincent.malt-test
  (:require
   [clojure.test :refer [deftest is]]
   [io.julienvincent.fixture.external-record :as external-record]
   [io.julienvincent.malt :as malt]
   [io.julienvincent.test.extensions]
   [malli.core :as m]
   [malli.error :as me]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test]))

(malt/defprotocol Example0
  (with-docstrings
    "Docstring"
    {:test/meta 1}
    [a :int b :int] :int)
  (with-nothing [] :int))

(deftest protocol-metadata
  (let [proto-data (into {} Example0)]

    (is (match?
         {:sigs {:with-docstrings {:malt/params '[a b]
                                   :malt/param-schemas {:a :int
                                                        :b :int}
                                   :malt/arguments-schema [:cat :int :int]
                                   :malt/return-schema :int
                                   :malt/arguments-validator (matchers/pred #(not (nil? %)))
                                   :malt/return-validator (matchers/pred #(not (nil? %)))
                                   :arglists '([this a b])
                                   :doc "Docstring"
                                   :test/meta 1}

                 :with-nothing {:malt/params '[]
                                :malt/arguments-schema nil
                                :malt/return-schema :int
                                :malt/arguments-validator matchers/absent
                                :malt/return-validator (matchers/pred #(not (nil? %)))
                                :arglists '([this])
                                :doc nil}}}
         proto-data))))

(malt/defprotocol Example1
  (method-1 [a :int b :int] :int)
  (method-2 [a :int] :int)
  (method-3 [] :int)
  (method-4 [] :nil)
  (method-5 [] :nil))

(deftest implement-methods-are-validated
  (let [impl (malt/implement Example1
               (method-1 [_ a b] (+ a b))
               (method-2 [_ a] a)
               (method-3 [_] "123")
               (method-4 [_])
               (method-5 [_] 1))]

    (is (= 3 (method-1 impl 1 2)))
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid parameter 'a' passed to 'method-1' of io.julienvincent.malt-test/Example1"
                    (matchers/equals
                     {:type :malt/input-validation-failed
                      :protocol 'io.julienvincent.malt-test/Example1
                      :method 'method-1
                      :input ["abc" '_]
                      :errors [["should be an integer"]]})
                    (method-1 impl "abc" 2)))
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid parameter 'b' passed to 'method-1' of io.julienvincent.malt-test/Example1"
                    (matchers/equals
                     {:type :malt/input-validation-failed
                      :protocol 'io.julienvincent.malt-test/Example1
                      :method 'method-1
                      :input ['_ "abc"]
                      :errors [nil ["should be an integer"]]})
                    (method-1 impl 2 "abc")))

    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid parameter 'a' passed to 'method-2' of io.julienvincent.malt-test/Example1"
                    (matchers/equals
                     {:type :malt/input-validation-failed
                      :protocol 'io.julienvincent.malt-test/Example1
                      :method 'method-2
                      :input ["123"]
                      :errors [["should be an integer"]]})
                    (method-2 impl "123")))

    (is (exception? Exception
                    "Invalid return value from 'method-3' of io.julienvincent.malt-test/Example1"
                    (matchers/equals
                     {:type :malt/output-validation-failed
                      :protocol 'io.julienvincent.malt-test/Example1
                      :method 'method-3
                      :output "123"
                      :errors ["should be an integer"]})
                    (method-3 impl)))

    (is (nil? (method-4 impl)))
    (is (exception? Exception
                    "Invalid return value from 'method-5' of io.julienvincent.malt-test/Example1"
                    {:output 1
                     :errors ["should be nil"]}
                    (method-5 impl)))))

(malt/defprotocol Example2
  (concat-str [suffix :string] :string))

(malt/extend String
  Example2
  (concat-str
   [original suffix]
   (str original suffix)))

(deftest extend-methods-are-validated
  (is (= "abc123" (concat-str "abc" "123")))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter 'suffix' passed to 'concat-str' of io.julienvincent.malt-test/Example2"
                  (matchers/equals
                   {:type :malt/input-validation-failed
                    :protocol 'io.julienvincent.malt-test/Example2
                    :method 'concat-str
                    :input [1]
                    :errors [["should be a string"]]})
                  (concat-str "abc" 1))))

(malt/defrecord Person
  [name :string
   age :int])

(deftest record-constructors-are-validated
  (is (= {:name "bob" :age 1}
         (into {} (->Person "bob" 1))))

  (is (= {:name "bob" :age 1}
         (into {} (map->Person {:name "bob" :age 1}))))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter 'name' passed to constructor '->Person' of io.julienvincent.malt-test/Person"
                  (matchers/equals
                   {:type :malt/record-validation-failed
                    :record 'io.julienvincent.malt-test/Person
                    :constructor '->Person
                    :input [1 '_]
                    :errors [["should be a string"]]})
                  (->Person 1 2)))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter passed to constructor 'map->Person' of io.julienvincent.malt-test/Person"
                  (matchers/equals
                   {:type :malt/record-validation-failed
                    :record 'io.julienvincent.malt-test/Person
                    :constructor 'map->Person
                    :input {:name "bob" :age "1"}
                    :errors {:age ["should be an integer"]}})
                  (map->Person {:name "bob" :age "1"}))))

(deftest record-schemas-are-defined
  (is (= [:map {:closed true}
          [:name :string]
          [:age :int]]
         ?PersonSchema))
  (is (not (nil? ?Person)))

  (is (m/validate ?Person (->Person "bob" 1)))
  (is (= ["should be an instance of io.julienvincent.malt-test/Person"]
         (me/humanize
          (m/explain ?Person "not-person")))))

(def ?PersonDef
  [:map
   [:name :string]])

(defn make-api-schema [?type]
  [:vector ?type])

(malt/defprotocol Example3
  (count-people [people [:vector ?PersonDef]
                 belongings (make-api-schema :string)]
    :int))

(deftest unresolved-protocol-schema-forms
  (let [impl (malt/implement Example3
               (count-people [_ people _] (count people)))]

    (is (= 2 (count-people impl [{:name "bob"} {:name "alice"}] ["desk"])))
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid parameter 'people' passed to 'count-people' of io.julienvincent.malt-test/Example3"
                    (matchers/embeds
                     {:type :malt/input-validation-failed
                      :protocol 'io.julienvincent.malt-test/Example3
                      :method 'count-people
                      :input [[{:name 1}] '_]
                      :errors (matchers/pred some?)})
                    (count-people impl [{:name 1}] [])))))

(malt/defrecord Person2
  [def [:vector ?PersonDef]
   belongings (make-api-schema :string)])

(deftest unresolved-schema-forms
  (is (= {:def [{:name "bob"}]
          :belongings ["desk"]}
         (into {} (->Person2 [{:name "bob"}]
                             ["desk"])))))

;; Implicit test.
(malt/extend external-record/External
  Example3
  (count-people [_ _ _] 1))
