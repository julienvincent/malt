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
