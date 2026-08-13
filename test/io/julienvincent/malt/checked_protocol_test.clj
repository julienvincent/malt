(ns io.julienvincent.malt.checked-protocol-test
  (:require
   [clojure.test :refer [deftest is]]
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

(malt/defprotocol CheckedExceptions
  (find! [id :string]
    :nil
    (throws [not_found]))

  (do-foo! [data :any]
    :nil
    (throws [conflict])))

(malt/defrecord Api
  []
  CheckedExceptions
  (find! [_ id]
    (when (= "" id)
      (throw (java.lang.IllegalArgumentException. "String too short")))
    (when (= "checked" id)
      (malt.error/throw! not_found {:id id}))
    (malt.error/throw! :fault "Fault"))

  (do-foo! [_ data]
    (malt.error/throw! conflict data)))

(deftest checked-protocol-metadata-test
  (let [proto-data (into {} CheckedExceptions)]
    (is (match?
         {:malt/protocol true
          :sigs {:find! {:malt/throws [not_found]
                         :malt/exception-validators
                         {:not_found (matchers/pred #(not (nil? %)))}}

                 :do-foo! {:malt/throws [conflict]
                           :malt/exception-validators
                           {:conflict (matchers/pred #(not (nil? %)))}}}}
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
