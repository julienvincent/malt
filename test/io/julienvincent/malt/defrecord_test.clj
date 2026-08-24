(ns io.julienvincent.malt.defrecord-test
  (:require
   [clojure.test :refer [deftest is]]
   [io.julienvincent.malt :as malt]
   [io.julienvincent.test.extensions]
   [malli.core :as m]
   [malli.error :as me]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test]))

(malt/defrecord Person
  [name :string
   age :int])

(deftest record-constructors-are-validated
  (is (= {:name "bob" :age 1}
         (into {} (->Person "bob" 1))))

  (is (= {:name "bob" :age 1}
         (into {} (map->Person {:name "bob" :age 1}))))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter 'name' passed to constructor '->Person' of io.julienvincent.malt.defrecord-test/Person"
                  (matchers/equals
                   {:type :malt/record-validation-failed
                    :record 'io.julienvincent.malt.defrecord-test/Person
                    :constructor '->Person
                    :input [1 '_]
                    :errors [["should be a string"]]})
                  (->Person 1 2)))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter passed to constructor 'map->Person' of io.julienvincent.malt.defrecord-test/Person"
                  (matchers/equals
                   {:type :malt/record-validation-failed
                    :record 'io.julienvincent.malt.defrecord-test/Person
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
  (is (= ["should be an instance of io.julienvincent.malt.defrecord-test/Person"]
         (me/humanize
          (m/explain ?Person "not-person")))))

(def ?PersonDef
  [:map
   [:name :string]])

(defn make-api-schema [?type]
  [:vector ?type])

(malt/defrecord Person2
  [def [:vector ?PersonDef]
   belongings (make-api-schema :string)])

(deftest unresolved-schema-forms
  (is (= {:def [{:name "bob"}]
          :belongings ["desk"]}
         (into {} (->Person2 [{:name "bob"}]
                             ["desk"])))))

(malt/defprotocol Api
  (create-user [name :string] :string))

(malt/defrecord Service
  [name-prefix :string]

  Api
  (create-user [{:keys [name-prefix]} name]
    (str name-prefix name)))

(deftest inline-implementations-validated-test
  (let [impl (->Service "some-prefix-")]
    (is (= "some-prefix-john" (create-user impl "john")))

    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid parameter 'name' passed to 'create-user' of io.julienvincent.malt.defrecord-test/Api"
                    (matchers/equals
                     {:type :malt/input-validation-failed
                      :protocol 'io.julienvincent.malt.defrecord-test/Api
                      :method 'create-user
                      :input [1]
                      :errors [["should be a string"]]})
                    (create-user impl 1)))))

(defprotocol NativeApi
  (foobar [this input]))

(malt/defrecord Service2
  [name-prefix :string]

  Api
  (create-user [{:keys [name-prefix]} name]
    (str name-prefix name))

  NativeApi
  (foobar [_ input] input))

(deftest native-protocol-extensions-test
  (let [impl (->Service2 "some-prefix-")]
    (is (= "some-prefix-john" (create-user impl "john")))
    (is (= 1 (foobar impl 1)))))

(malt/defprotocol MultiArityApi
  (lookup-value
    ([value :string]
     :string)
    ([left :int right :int]
     :int)))

(malt/defrecord MultiArityService
  [prefix :string]

  MultiArityApi
  (lookup-value [{:keys [prefix]} value]
    (if (= "bad-output" value)
      1
      (str prefix value)))
  (lookup-value [_ left right]
    (if (neg? left)
      "bad-output"
      (+ left right))))

(deftest multi-arity-inline-implementations-test
  (let [impl (->MultiArityService "prefix-")]
    (is (= "prefix-value" (lookup-value impl "value")))
    (is (= 3 (lookup-value impl 1 2)))

    (is (exception? clojure.lang.ExceptionInfo
                    #"Invalid parameter 'value' passed to 'lookup-value'"
                    {:type :malt/input-validation-failed
                     :input [1]}
                    (lookup-value impl 1)))

    (is (exception? clojure.lang.ExceptionInfo
                    #"Invalid parameter 'right' passed to 'lookup-value'"
                    {:type :malt/input-validation-failed
                     :input ['_ "2"]}
                    (lookup-value impl 1 "2")))

    (is (exception? clojure.lang.ExceptionInfo
                    #"Invalid return value from 'lookup-value'"
                    {:type :malt/output-validation-failed
                     :output 1}
                    (lookup-value impl "bad-output")))

    (is (exception? clojure.lang.ExceptionInfo
                    #"Invalid return value from 'lookup-value'"
                    {:type :malt/output-validation-failed
                     :output "bad-output"}
                    (lookup-value impl -1 2)))))
