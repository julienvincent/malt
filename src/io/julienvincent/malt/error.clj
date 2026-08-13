(ns io.julienvincent.malt.error
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(def ?ErrorDefinition
  [:map {:closed true}
   [:code :keyword]
   [:message {:optional true} :string]
   [:schema {:optional true} :any]
   [:metadata {:optional true} :map]])

(def ^:private definition-validator
  (delay (m/validator ?ErrorDefinition)))

(defn validate-definition!
  "Validate that a given malt error `definition` conforms to the malt error
   definition schema format."
  [definition]
  (when-not (@definition-validator definition)
    (let [explain (m/explain ?ErrorDefinition definition)]
      (throw (ex-info "Invalid error definition"
                      {:type :malt/invalid-definition
                       :definition definition
                       :errors (me/humanize explain)}))))
  definition)

(defn ex
  "Constructs a `clojure.lang.ExceptionInfo` representing a malt error.

   The first argument is either a keyword error `code` or an error `definition`
   map (see [?ErrorDefinition]) containing at minimum a `:code`. Remaining
   arguments are interpreted by type:

   - A string is treated as the exception message
   - A map is treated as the exception data
   - A Throwable is treated as the exception cause

   A message must always be resolvable - either passed explicitly as a string,
   or present as `:message` on the definition. When both are provided, the
   explicit string takes precedence.

   The resulting exception carries `{:type :malt/error :code code :data data}`
   as its `ex-data`.

   Usage:

   ```clojure
   (def not-found
     {:code :not-found
      :message \"Resource not found\"})

   ;; From a definition containing a :message
   (ex not-found)

   ;; Code or definition with an explicit message
   (ex :not-found \"Resource not found\")

   ;; Definition with data, message taken from the definition
   (ex not-found {:id \"123\"})

   ;; Message and data
   (ex :not-found \"Resource not found\" {:id \"123\"})

   ;; Message and cause
   (ex :not-found \"Resource not found\" cause)

   ;; Definition with data and cause, message taken from the definition
   (ex not-found {:id \"123\"} cause)

   ;; All arguments explicitly
   (ex :not-found \"Resource not found\" {:id \"123\"} cause)
   ```

   Throws an `IllegalArgumentException` when no message can be resolved, when
   the definition contains no `:code`, or when an argument has an unexpected
   type."
  ([code|definition]
   (when (keyword? code|definition)
     (throw (IllegalArgumentException.
             (str "A message must be provided when constructing "
                  "an exception from only a code"))))
   (when-not (:message code|definition)
     (throw (IllegalArgumentException.
             (str "A message must be provided when constructing "
                  "an exception from a definition that does not contain "
                  "a :message"))))
   (ex code|definition (:message code|definition) {} nil))

  ([code|definition message|data]
   (let [message (cond
                   (string? message|data)
                   message|data

                   (and (map? code|definition)
                        (not (nil? (:message code|definition))))
                   (:message code|definition))
         data (cond
                (string? message|data)
                {}

                (map? message|data)
                message|data

                :else
                (throw (IllegalArgumentException.
                        (str "Invalid second argument provided. Must either be "
                             "a string or a map, received " (type message|data)))))]

     (when-not message
       (if (keyword? code|definition)
         (throw (IllegalArgumentException.
                 (str "A message must be provided when constructing "
                      "an exception from only a code")))
         (throw (IllegalArgumentException.
                 (str "A message must be provided when constructing "
                      "an exception from a definition that does not contain "
                      "a :message")))))

     (ex code|definition message data nil)))
  ([code|definition message|data data|cause]
   (let [message (cond
                   (string? message|data)
                   message|data

                   (and (map? code|definition)
                        (not (nil? (:message code|definition))))
                   (:message code|definition))
         data (cond
                (map? message|data)
                message|data

                (map? data|cause)
                data|cause

                :else
                {})]

     (when-not (or (string? message|data)
                   (map? message|data))
       (throw (IllegalArgumentException.
               (str "Invalid second argument provided. Must either be "
                    "a string or a map, received " (type message|data)))))

     (when-not (or (map? data|cause)
                   (instance? Throwable data|cause))
       (throw (IllegalArgumentException.
               (str "Invalid third argument provided. Must either be "
                    "a map or a Throwable, received " (type data|cause)))))

     (when (and (map? message|data)
                (map? data|cause))
       (throw (IllegalArgumentException.
               (str "Invalid arguments provided. Data was provided as both "
                    "the second and third argument"))))

     (when-not message
       (if (keyword? code|definition)
         (throw (IllegalArgumentException.
                 (str "A message must be provided when constructing "
                      "an exception from only a code")))
         (throw (IllegalArgumentException.
                 (str "A message must be provided when constructing "
                      "an exception from a definition that does not contain "
                      "a :message")))))

     (ex code|definition message data
         (when (instance? Throwable data|cause)
           data|cause))))
  ([code|definition message data cause]
   (let [code (cond
                (keyword? code|definition)
                code|definition

                (and (map? code|definition)
                     (:code code|definition))
                (:code code|definition)

                (map? code|definition)
                (throw (IllegalArgumentException.
                        (str "Invalid first argument. A malt error definition "
                             "must contain, at minimum, " "a code")))

                :else
                (throw (IllegalArgumentException.
                        (str "Invalid first argument. Expected a keyword or a map, "
                             "received a " (type code|definition)))))]
     (ex-info message {:type :malt/error
                       :code code
                       :data data}
              cause))))

(defn throw!
  "Constructs and immediately throws a malt error.

   Accepts the same arguments as [ex] - see its docstring for the full set of
   supported call shapes.

   ```clojure
   (def not-found
     {:code :not-found
      :message \"Resource not found\"})

   (throw! not-found)
   (throw! :not-found \"Resource not found\")
   (throw! :not-found \"Resource not found\" {:id \"123\"} cause)
   ```"
  ([code|definition]
   (throw (ex code|definition)))
  ([code|definition message|data]
   (throw (ex code|definition message|data)))
  ([code|definition message|data data|cause]
   (throw (ex code|definition message|data data|cause)))
  ([code|definition message data cause]
   (throw (ex code|definition message data cause))))
