(ns io.julienvincent.malt.extend-type-test
  (:require
   [clojure.test :refer [deftest is]]
   [io.julienvincent.fixture.external-record :as external-record]
   [io.julienvincent.malt :as malt]
   [io.julienvincent.test.extensions]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test]))

(malt/defprotocol Example
  (concat-str [suffix :string] :string))

(malt/extend-type String
  Example
  (concat-str [original suffix]
    (str original suffix)))

(deftest extend-methods-are-validated
  (is (= "abc123" (concat-str "abc" "123")))

  (is (exception? clojure.lang.ExceptionInfo
                  "Invalid parameter 'suffix' passed to 'concat-str' of io.julienvincent.malt.extend-type-test/Example"
                  (matchers/equals
                   {:type :malt/input-validation-failed
                    :protocol 'io.julienvincent.malt.extend-type-test/Example
                    :method 'concat-str
                    :input [1]
                    :errors [["should be a string"]]})
                  (concat-str "abc" 1))))

(malt/defprotocol Adder
  (add [value :int] :int))

;; Implicit test. This should not fail because it is an external reference.
;; Extend-type should be able to resolve external references
(malt/extend-type external-record/External
  Adder
  (add [self value]
    (+ (:value self) value)))

(deftest external-extension-test
  (is (= 4 (add (external-record/->External 3) 1))))

(defprotocol NativeApi
  (foobar [this input]))

(malt/extend-type external-record/External
  NativeApi
  (foobar [_ input] input))

(deftest native-protocol-extensions-test
  (let [impl (external-record/->External 3)]
    (is (= 1 (foobar impl 1)))))

(malt/defprotocol MultiArityExample
  (combine-values
    ([suffix :string]
     :string)
    ([left :int right :int]
     :int)))

(malt/extend-type String
  MultiArityExample
  (combine-values [original suffix]
    (if (= "bad-output" suffix)
      1
      (str original suffix)))
  (combine-values [_ left right]
    (if (neg? left)
      "bad-output"
      (+ left right))))

(malt/extend-type Long
  MultiArityExample
  (combine-values
    ([original suffix]
     (str original suffix))
    ([_ left right]
     (+ left right))))

(deftest multi-arity-extend-methods-test
  (is (= "abc123" (combine-values "abc" "123")))
  (is (= 3 (combine-values "abc" 1 2)))
  (is (= "1123" (combine-values 1 "123")))
  (is (= 3 (combine-values 1 1 2)))

  (is (exception? clojure.lang.ExceptionInfo
                  #"Invalid parameter 'suffix' passed to 'combine-values'"
                  {:type :malt/input-validation-failed
                   :input [1]}
                  (combine-values "abc" 1)))

  (is (exception? clojure.lang.ExceptionInfo
                  #"Invalid parameter 'right' passed to 'combine-values'"
                  {:type :malt/input-validation-failed
                   :input ['_ "2"]}
                  (combine-values "abc" 1 "2")))

  (is (exception? clojure.lang.ExceptionInfo
                  #"Invalid return value from 'combine-values'"
                  {:type :malt/output-validation-failed
                   :output 1}
                  (combine-values "abc" "bad-output")))

  (is (exception? clojure.lang.ExceptionInfo
                  #"Invalid return value from 'combine-values'"
                  {:type :malt/output-validation-failed
                   :output "bad-output"}
                  (combine-values "abc" -1 2))))
