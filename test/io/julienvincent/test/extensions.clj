(ns io.julienvincent.test.extensions
  (:require
   [clojure.test :as test]
   [matcher-combinators.clj-test :refer [tagged-for-pretty-printing]]
   [matcher-combinators.core :as match]
   [matcher-combinators.matchers :as matchers]))

(defmethod test/assert-expr 'exception? [msg form]
  (let [[_ ex-class ex-msg ex-data] form
        body (nthnext form 4)]
    `(try ~@body
          (test/do-report {:type :fail
                           :message ~msg
                           :expected (list '~ex-class '~ex-msg '~ex-data)
                           :actual nil})
          (catch Exception ex#
            (let [data# [ex# (ex-message ex#) (ex-data ex#)]
                  test# [(matchers/pred (fn [value#]
                                          (instance? ~ex-class value#))
                                        (str "Must be an instance of " ~ex-class))
                         ~ex-msg
                         ~ex-data]
                  match-result# (match/match test# data#)]
              (if (match/indicates-match? match-result#)
                (clojure.test/do-report {:type :pass
                                         :message ~msg
                                         :expected '~form
                                         :actual ex#})
                (clojure.test/do-report {:type :fail
                                         :message ~msg
                                         :expected '~form
                                         :actual (tagged-for-pretty-printing
                                                  (list '~'not (list 'exception?
                                                                     ~ex-class
                                                                     ~ex-msg
                                                                     ~ex-data
                                                                     '~body))
                                                  match-result#)
                                         :ex-class ~ex-class})))))))
