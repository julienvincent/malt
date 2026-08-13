(ns io.julienvincent.malt.error-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [io.julienvincent.malt.error :as error]
   [io.julienvincent.test.extensions]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test]))

(deftest ex-with-definition-test
  (testing "creates exception from a full definition"
    (let [definition {:code :not_found
                      :message "Resource not found"
                      :schema [:map [:id :string]]
                      :metadata {:http/status-code 404}}
          exception (error/ex definition {:id "123"})]
      (is (instance? clojure.lang.ExceptionInfo exception))
      (is (= "Resource not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {:id "123"}}
             (ex-data exception)))))

  (testing "creates exception from definition with no explicit data"
    (let [definition {:code :conflict
                      :message "Resource conflict"}
          exception (error/ex definition)]
      (is (= "Resource conflict" (ex-message exception)))
      (is (= {:type :malt/error
              :code :conflict
              :data {}}
             (ex-data exception)))))

  (testing "creates exception from definition with string message override"
    (let [definition {:code :not_found
                      :message "Resource not found"}
          exception (error/ex definition "Custom message")]
      (is (= "Custom message" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {}}
             (ex-data exception))))))

(deftest ex-with-keyword-test
  (testing "creates exception from keyword code with message"
    (let [exception (error/ex :not_found "Not found")]
      (is (= "Not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {}}
             (ex-data exception)))))

  (testing "throws when keyword code provided without message"
    (is (thrown? IllegalArgumentException
                 (error/ex :not_found)))))

(deftest ex-with-data-test
  (testing "creates exception with data map as second arg"
    (let [definition {:code :not_found
                      :message "Not found"}
          exception (error/ex definition {:id "abc"})]
      (is (= {:type :malt/error
              :code :not_found
              :data {:id "abc"}}
             (ex-data exception)))))

  (testing "creates exception with all four args"
    (let [cause (Exception. "root cause")
          exception (error/ex :not_found "Not found" {:id "abc"} cause)]
      (is (= "Not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {:id "abc"}}
             (ex-data exception)))
      (is (identical? cause (ex-cause exception))))))

(deftest ex-three-arity-test
  (testing "creates exception with message and data"
    (let [exception (error/ex :not_found "Not found" {:id "abc"})]
      (is (= "Not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {:id "abc"}}
             (ex-data exception)))
      (is (nil? (ex-cause exception)))))

  (testing "creates exception with message and cause"
    (let [cause (Exception. "root cause")
          exception (error/ex :not_found "Not found" cause)]
      (is (= "Not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {}}
             (ex-data exception)))
      (is (identical? cause (ex-cause exception)))))

  (testing "creates exception with data and cause, message from definition"
    (let [cause (Exception. "root cause")
          definition {:code :not_found
                      :message "Resource not found"}
          exception (error/ex definition {:id "abc"} cause)]
      (is (= "Resource not found" (ex-message exception)))
      (is (= {:type :malt/error
              :code :not_found
              :data {:id "abc"}}
             (ex-data exception)))
      (is (identical? cause (ex-cause exception)))))

  (testing "throws when second arg is neither string nor map"
    (is (thrown? IllegalArgumentException
                 (error/ex :not_found 123 {}))))

  (testing "throws when third arg is neither map nor Throwable"
    (is (thrown? IllegalArgumentException
                 (error/ex :not_found "Not found" 123))))

  (testing "throws when data is provided twice"
    (is (thrown? IllegalArgumentException
                 (error/ex :not_found {:id "abc"} {:id "def"}))))

  (testing "throws when data and cause provided but no message available"
    (is (thrown? IllegalArgumentException
                 (error/ex :not_found {:id "abc"} (Exception. "cause"))))
    (is (thrown? IllegalArgumentException
                 (error/ex {:code :not_found} {:id "abc"} (Exception. "cause"))))))

(deftest ex-error-cases-test
  (testing "throws when definition has no :message and none provided"
    (is (thrown? IllegalArgumentException
                 (error/ex {:code :foo}))))

  (testing "throws when definition has no :code"
    (is (thrown? IllegalArgumentException
                 (error/ex {:message "foo"} "msg"))))

  (testing "throws when second arg is neither string nor map"
    (is (thrown? IllegalArgumentException
                 (error/ex {:code :foo :message "msg"} 123)))))

(deftest throw!-test
  (testing "throws ExceptionInfo from definition"
    (let [definition {:code :not_found
                      :message "Resource not found"}]
      (is (exception? clojure.lang.ExceptionInfo
                      "Resource not found"
                      (matchers/equals
                       {:type :malt/error
                        :code :not_found
                        :data {}})
                      (error/throw! definition)))))

  (testing "throws ExceptionInfo from definition with data"
    (let [definition {:code :not_found
                      :message "Resource not found"}]
      (is (exception? clojure.lang.ExceptionInfo
                      "Resource not found"
                      (matchers/equals
                       {:type :malt/error
                        :code :not_found
                        :data {:id "abc"}})
                      (error/throw! definition {:id "abc"})))))

  (testing "throws ExceptionInfo from keyword code with message"
    (is (exception? clojure.lang.ExceptionInfo
                    "Something broke"
                    (matchers/equals
                     {:type :malt/error
                      :code :fault
                      :data {}})
                    (error/throw! :fault "Something broke")))))

(deftest validate-definition!-test
  (testing "valid definitions pass through"
    (let [definition {:code :not_found
                      :message "Not found"}]
      (is (= definition (error/validate-definition! definition))))

    (let [definition {:code :conflict
                      :message "Conflict"
                      :schema [:map [:id :string]]
                      :metadata {:http/status-code 409}}]
      (is (= definition (error/validate-definition! definition)))))

  (testing "throws on missing :code"
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid error definition"
                    (matchers/embeds
                     {:type :malt/invalid-definition
                      :definition {:message "Foo"}
                      :errors (matchers/pred some?)})
                    (error/validate-definition! {:message "Foo"}))))

  (testing "throws on unknown keys"
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid error definition"
                    (matchers/embeds
                     {:type :malt/invalid-definition
                      :errors (matchers/pred some?)})
                    (error/validate-definition!
                     {:code :foo
                      :message "Foo"
                      :unknown-key "bar"}))))

  (testing "throws when :code is not a keyword"
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid error definition"
                    (matchers/embeds
                     {:type :malt/invalid-definition
                      :errors (matchers/pred some?)})
                    (error/validate-definition!
                     {:code "not_a_keyword"
                      :message "Foo"}))))

  (testing "throws when :message is not a string"
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid error definition"
                    (matchers/embeds
                     {:type :malt/invalid-definition
                      :errors (matchers/pred some?)})
                    (error/validate-definition!
                     {:code :foo
                      :message 123}))))

  (testing "throws when :metadata is not a map"
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid error definition"
                    (matchers/embeds
                     {:type :malt/invalid-definition
                      :errors (matchers/pred some?)})
                    (error/validate-definition!
                     {:code :foo
                      :message "Foo"
                      :metadata "not a map"})))))
