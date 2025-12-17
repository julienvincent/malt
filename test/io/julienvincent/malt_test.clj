(ns io.julienvincent.malt-test
  (:require
   [matcher-combinators.test]
   [matcher-combinators.matchers :as matchers]
   [clojure.test :refer [deftest is]]
   [io.julienvincent.malt :as malt]))

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
    (is (thrown-match? clojure.lang.ExceptionInfo
                       (matchers/equals
                        {:type :malt/input-validation-failed
                         :protocol 'io.julienvincent.malt-test/Example1
                         :method 'method-1
                         :input ["abc" '_]
                         :errors ["should be an integer"]})
                       (method-1 impl "abc" 2)))
    (is (thrown-match? clojure.lang.ExceptionInfo
                       (matchers/equals
                        {:type :malt/input-validation-failed
                         :protocol 'io.julienvincent.malt-test/Example1
                         :method 'method-1
                         :input ['_ "abc"]
                         :errors ["should be an integer"]})
                       (method-1 impl 2 "abc")))

    (is (thrown-match? Exception
                       (matchers/equals
                        {:type :malt/input-validation-failed
                         :protocol 'io.julienvincent.malt-test/Example1
                         :method 'method-2
                         :input ["123"]
                         :errors ["should be an integer"]})
                       (method-2 impl "123")))

    (is (thrown-match? Exception
                       (matchers/equals
                        {:type :malt/output-validation-failed
                         :protocol 'io.julienvincent.malt-test/Example1
                         :method 'method-3
                         :output "123"
                         :errors ["should be an integer"]})
                       (method-3 impl)))

    (is (nil? (method-4 impl)))
    (is (thrown-match? Exception
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

  (is (thrown-match? Exception
                     (matchers/equals
                      {:type :malt/input-validation-failed
                       :protocol 'io.julienvincent.malt-test/Example2
                       :method 'concat-str
                       :input [1]
                       :errors ["should be a string"]})
                     (concat-str "abc" 1))))
