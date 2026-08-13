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
  [definition]
  (when-not (@definition-validator definition)
    (let [explain (m/explain ?ErrorDefinition definition)]
      (throw (ex-info "Invalid error definition"
                      {:type :malt/invalid-definition
                       :definition definition
                       :errors (me/humanize explain)}))))
  definition)

(defn ex
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
  ([code|definition message|data data|exception]
   (let [message (cond
                   (string? message|data)
                   message|data

                   (and (map? code|definition)
                        (not (nil? (:message code|definition))))
                   (:message code|definition))
         data (cond
                (map? message|data)
                message|data

                (map? data|exception)
                data|exception

                :else
                {})]

     (when-not (or (string? message|data)
                   (map? message|data))
       (throw (IllegalArgumentException.
               (str "Invalid second argument provided. Must either be "
                    "a string or a map, received " (type message|data)))))

     (when-not (or (map? data|exception)
                   (instance? Throwable data|exception))
       (throw (IllegalArgumentException.
               (str "Invalid third argument provided. Must either be "
                    "a map or a Throwable, received " (type data|exception)))))

     (when (and (map? message|data)
                (map? data|exception))
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
         (when (instance? Throwable data|exception)
           data|exception))))
  ([code|definition message data exception]
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
              exception))))

(defn throw!
  ([code|definition]
   (throw (ex code|definition)))
  ([code|definition message|data]
   (throw (ex code|definition message|data)))
  ([code|definition message|data data|exception]
   (throw (ex code|definition message|data data|exception)))
  ([code|definition message data exception]
   (throw (ex code|definition message data exception))))
