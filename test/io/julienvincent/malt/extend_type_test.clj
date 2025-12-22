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
  (concat-str
    [original suffix]
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
