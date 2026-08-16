(ns io.julienvincent.malt.checked-protocol-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [io.julienvincent.malt :as malt]
   [io.julienvincent.malt.error :as malt.error]
   [io.julienvincent.test.extensions]
   [matcher-combinators.matchers :as matchers]
   [matcher-combinators.test]))

(def not_found
  {:code :not_found
   :schema [:map
            [:id :string]]
   :message "Resource not found"
   :metadata {:http/status-code 404}})

(def conflict
  {:code :conflict
   :message "Resource conflict"
   :schema [:map
            [:resource :string]]})

(def io_error java.io.IOException)

(def strict_error
  {:class java.lang.Exception
   :schema [:map
            [:reason :string]]
   :metadata {:http/status-code 500}})

(malt/defprotocol CheckedExceptions
  (find! [id :string]
    :nil
    (throws [not_found]))

  (do-foo! [data :any]
    :nil
    (throws [conflict]))

  (do-bar! [action :keyword]
    :nil
    (throws [Exception]))

  (do-baz! [action :keyword]
    :nil
    (throws [conflict io_error]))

  (do-qux! [action :keyword]
    :nil
    (throws [strict_error]))

  (do-strict! [action :keyword]
    :nil))

(malt/defrecord Api
  []
  CheckedExceptions
  (find! [_ id]
    (cond
      (= id "")
      (throw (java.lang.IllegalArgumentException. "String too short"))

      (= "checked" id)
      (malt.error/throw! not_found {:id id})

      (= "bad-output" id)
      123

      :else
      (malt.error/throw! :fault "Fault")))

  (do-foo! [_ data]
    (malt.error/throw! conflict data))

  (do-bar! [_ action]
    (case action
      :plain (throw (ex-info "Bad" {:some "data"}))
      :malt (malt.error/throw! :fault "Fault")))

  (do-baz! [_ action]
    (case action
      :conflict (malt.error/throw! conflict {:resource "abc"})
      :io (throw (java.io.FileNotFoundException. "missing.txt"))
      :other (throw (IllegalStateException. "Illegal state"))
      :malt (malt.error/throw! :fault "Fault")))

  (do-qux! [_ action]
    (case action
      :valid (throw (ex-info "Custom failure" {:reason "broken"}))
      :invalid (throw (ex-info "Custom failure" {:reason 123}))
      :plain (throw (RuntimeException. "Plain failure"))))

  (do-strict! [_ action]
    (case action
      :plain (throw (ex-info "Boom" {:some "data"}))
      :malt (malt.error/throw! :fault "Fault")
      :ok nil)))

(deftest checked-protocol-metadata-test
  (let [proto-data (into {} CheckedExceptions)]
    (is (match?
         {:malt/protocol true
          :sigs {:find! {:malt/throws [not_found]
                         :malt/exception-validators
                         {:not_found (matchers/pred #(not (nil? %)))}}

                 :do-foo! {:malt/throws [conflict]
                           :malt/exception-validators
                           {:conflict (matchers/pred #(not (nil? %)))}}

                 :do-bar! {:malt/throws [{:class java.lang.Exception}]
                           :malt/exception-validators (matchers/equals {})}

                 :do-baz! {:malt/throws [conflict {:class java.io.IOException}]
                           :malt/exception-validators
                           {:conflict (matchers/pred #(not (nil? %)))}}

                 :do-qux! {:malt/throws [strict_error]
                           :malt/exception-validators
                           {java.lang.Exception (matchers/pred #(not (nil? %)))}}}}
         proto-data))))

(deftest checked-error-test
  (let [api (->Api)]
    (is (exception? clojure.lang.ExceptionInfo
                    "Resource not found"
                    (matchers/equals
                     {:type :malt/error
                      :code :not_found
                      :data {:id "checked"}})
                    (find! api "checked")))))

(deftest unchecked-validation-errors-test
  (testing "Malt validation errors should not be checked"
    (let [api (->Api)]
      (is (exception? clojure.lang.ExceptionInfo
                      #"Invalid parameter 'id' passed to 'find!'"
                      {:type :malt/input-validation-failed
                       :errors [["should be a string"]]
                       :input [1]}
                      (find! api 1)))

      (is (exception? clojure.lang.ExceptionInfo
                      #"Invalid return value from 'find!'"
                      {:type :malt/output-validation-failed
                       :output 123
                       :errors ["should be nil"]}
                      (find! api "bad-output"))))))

(deftest unchecked-error-test
  (let [api (->Api)]
    (is (exception? clojure.lang.ExceptionInfo
                    "Unspecified exception thrown from method 'find!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                    (matchers/equals
                     {:type :malt/unspecified-exception-error
                      :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                      :method 'find!})
                    (find! api "unchecked")))
    (is (exception? clojure.lang.ExceptionInfo
                    "Unspecified exception thrown from method 'find!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                    (matchers/equals
                     {:type :malt/unspecified-exception-error
                      :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                      :method 'find!})
                    (find! api "")))))

(deftest invalid-error-data-test
  (let [api (->Api)]
    (is (exception? clojure.lang.ExceptionInfo
                    "Invalid exception thrown from method 'do-foo!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                    (matchers/equals
                     {:type :malt/invalid-exception-error
                      :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                      :method 'do-foo!
                      :data {:resource 1}
                      :errors {:resource ["should be a string"]}})
                    (do-foo! api {:resource 1})))))

(deftest exception-cause-test
  (let [api (->Api)]
    (try
      (find! api "unchecked")
      (is false "Expected exception to be thrown")
      (catch clojure.lang.ExceptionInfo ex
        (let [cause (ex-cause ex)]
          (is (instance? clojure.lang.ExceptionInfo cause))
          (is (= "Fault" (ex-message cause)))
          (is (= {:type :malt/error
                  :code :fault
                  :data {}}
                 (ex-data cause))))))))

(deftest no-throws-clause-test
  (let [api (->Api)]
    (testing "methods without a throws clause are expected not to throw"
      (is (exception? clojure.lang.ExceptionInfo
                      "Unspecified exception thrown from method 'do-strict!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/unspecified-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-strict!})
                      (do-strict! api :plain)))

      (is (exception? clojure.lang.ExceptionInfo
                      "Unspecified exception thrown from method 'do-strict!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/unspecified-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-strict!})
                      (do-strict! api :malt))))

    (testing "the original exception is preserved as the cause"
      (try
        (do-strict! api :plain)
        (is false "Expected exception to be thrown")
        (catch clojure.lang.ExceptionInfo ex
          (let [cause (ex-cause ex)]
            (is (instance? clojure.lang.ExceptionInfo cause))
            (is (= "Boom" (ex-message cause)))
            (is (= {:some "data"} (ex-data cause)))))))

    (testing "returning normally passes"
      (is (nil? (do-strict! api :ok))))))

(deftest class-throws-test
  (let [api (->Api)]
    (testing "non-malt exceptions matching a declared class are rethrown unchanged"
      (is (exception? clojure.lang.ExceptionInfo
                      "Bad"
                      (matchers/equals {:some "data"})
                      (do-bar! api :plain))))

    (testing "malt errors never match class declarations"
      (is (exception? clojure.lang.ExceptionInfo
                      "Unspecified exception thrown from method 'do-bar!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/unspecified-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-bar!})
                      (do-bar! api :malt))))))

(deftest mixed-throws-test
  (let [api (->Api)]
    (testing "malt errors match declared error definitions by :code"
      (is (exception? clojure.lang.ExceptionInfo
                      "Resource conflict"
                      (matchers/equals
                       {:type :malt/error
                        :code :conflict
                        :data {:resource "abc"}})
                      (do-baz! api :conflict))))

    (testing "subclasses of a declared class are matched"
      (is (exception? java.io.FileNotFoundException
                      "missing.txt"
                      nil
                      (do-baz! api :io))))

    (testing "exceptions not matching any declared class are unspecified"
      (is (exception? clojure.lang.ExceptionInfo
                      "Unspecified exception thrown from method 'do-baz!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/unspecified-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-baz!})
                      (do-baz! api :other))))

    (testing "malt errors with undeclared codes are unspecified even when classes are declared"
      (is (exception? clojure.lang.ExceptionInfo
                      "Unspecified exception thrown from method 'do-baz!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/unspecified-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-baz!})
                      (do-baz! api :malt))))))

(deftest exception-definition-schema-test
  (let [api (->Api)]
    (testing "valid ex-data is rethrown unchanged"
      (is (exception? clojure.lang.ExceptionInfo
                      "Custom failure"
                      (matchers/equals {:reason "broken"})
                      (do-qux! api :valid))))

    (testing "invalid ex-data is wrapped in an invalid-exception-error"
      (is (exception? clojure.lang.ExceptionInfo
                      "Invalid exception thrown from method 'do-qux!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/invalid-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-qux!
                        :data {:reason 123}
                        :errors {:reason ["should be a string"]}})
                      (do-qux! api :invalid))))

    (testing "exceptions without ex-data fail the schema"
      (is (exception? clojure.lang.ExceptionInfo
                      "Invalid exception thrown from method 'do-qux!' of io.julienvincent.malt.checked-protocol-test/CheckedExceptions"
                      (matchers/equals
                       {:type :malt/invalid-exception-error
                        :protocol 'io.julienvincent.malt.checked-protocol-test/CheckedExceptions
                        :method 'do-qux!
                        :data nil
                        :errors ["invalid type"]})
                      (do-qux! api :plain))))))

(deftest invalid-throws-entry-test
  (testing "class entries must extend Throwable"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:type :malt/invalid-definition
                        :definition {:class java.lang.String}}
                       (binding [*ns* (the-ns 'io.julienvincent.malt.checked-protocol-test)]
                         (eval '(io.julienvincent.malt/defprotocol InvalidClassThrows
                                  (invalid-class-op! []
                                    :nil
                                    (throws [String]))))))))

  (testing "definitions may not contain both :code and :class"
    (is (thrown-match? clojure.lang.ExceptionInfo
                       {:type :malt/invalid-definition
                        :definition {:code :foo
                                     :class java.lang.Exception}}
                       (binding [*ns* (the-ns 'io.julienvincent.malt.checked-protocol-test)]
                         (eval '(do
                                  (def bad_throws_definition
                                    {:code :foo
                                     :class java.lang.Exception})
                                  (io.julienvincent.malt/defprotocol BadDefinitionThrows
                                    (bad-definition-op! []
                                      :nil
                                      (throws [bad_throws_definition])))))))))

  (testing "throws symbols must resolve to a var or a class"
    (is (thrown? IllegalArgumentException
                 (binding [*ns* (the-ns 'io.julienvincent.malt.checked-protocol-test)]
                   (eval '(io.julienvincent.malt/defprotocol UnresolvableThrows
                            (unresolvable-op! []
                              :nil
                              (throws [does-not-exist])))))))))
