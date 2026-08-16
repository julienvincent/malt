(ns ^:no-doc io.julienvincent.malt.runtime
  "Runtime support invoked by the code emitted from the io.julienvincent.malt
   macros. Nothing in this namespace is intended to be called directly by users."
  (:refer-clojure :exclude [method-sig])
  (:require
   [io.julienvincent.malt.error :as malt.error]
   [io.julienvincent.malt.schema :as malt.schema]
   [malli.core :as m]
   [malli.error :as me]))

(def ^:private ?ThrowsDefinition
  [:or
   malt.error/?ErrorDefinition
   malt.error/?ExceptionDefinition])

(def ^:private throws-definition-validator
  (delay (m/validator ?ThrowsDefinition)))

(defn- validate-throws-definition!
  [definition]
  (when-not (@throws-definition-validator definition)
    (let [explain (m/explain ?ThrowsDefinition definition)]
      (throw (ex-info "Invalid throws definition"
                      {:type :malt/invalid-definition
                       :definition definition
                       :errors (me/humanize explain)}))))
  definition)

(defn- resolve-throws-definitions
  [protocol-ns throws-syms]
  (mapv (fn [throws-sym]
          (let [resolved (ns-resolve protocol-ns throws-sym)
                value (cond
                        (var? resolved) @resolved
                        (class? resolved) resolved
                        :else
                        (throw (IllegalArgumentException.
                                (str "throws symbol '" throws-sym
                                     "' must resolve to a var or a class"))))]
            (validate-throws-definition!
             (if (class? value)
               {:class value}
               value))))
        throws-syms))

(defn- enrich-sig
  "Resolves the authored schema forms stored on a protocol method signature and
   extends the signature with the resolved schemas and their precompiled
   validators."
  [protocol-ns sig]
  (let [resolve-schema #(malt.schema/resolve-schema-spec protocol-ns %)
        arg-schemas (when (seq (:malt/arguments-schema sig))
                      (mapv resolve-schema (rest (:malt/arguments-schema sig))))
        return-schema (when (:malt/return-schema sig)
                        (resolve-schema (:malt/return-schema sig)))
        throws (when (seq (:malt/throws sig))
                 (resolve-throws-definitions protocol-ns (:malt/throws sig)))]
    (cond-> sig
      arg-schemas
      (assoc :malt/arguments-schema (into [:cat] arg-schemas)
             :malt/arguments-validator (m/validator (into [:cat] arg-schemas)))

      (and arg-schemas (:malt/param-schemas sig))
      (assoc :malt/param-schemas
             (zipmap (mapv (comp keyword name) (:malt/params sig))
                     arg-schemas))

      return-schema
      (assoc :malt/return-schema return-schema
             :malt/return-validator (m/validator return-schema))

      throws
      (assoc :malt/throws throws
             :malt/exception-validators
             (into {}
                   (keep (fn [{:keys [code schema] klass :class}]
                           (when schema
                             [(or code klass) (m/validator schema)])))
                   throws)))))

(defn enrich-protocol-var!
  "Marks `protocol-var` as a malt protocol and enriches every method signature
   with resolved schemas and precompiled validators. Called once as part of a
   `malt/defprotocol` definition."
  [protocol-var]
  (let [protocol-ns (:ns (meta protocol-var))]
    (alter-var-root
     protocol-var
     (fn [protocol]
       (-> protocol
           (assoc :malt/protocol true)
           (update :sigs update-vals #(enrich-sig protocol-ns %))))))
  protocol-var)

(defn method-sig
  "Returns the enriched signature map for a protocol method."
  [protocol-var method-kw]
  (get-in @protocol-var [:sigs method-kw]))

(defn- call-target-description
  [{:keys [protocol method constructor record]}]
  (if protocol
    (str "'" (name method) "' of " protocol)
    (str "constructor '" (name constructor) "' of " record)))

(defn validate-inputs!
  "Validates positional arguments against an `[:cat ...]` schema. On failure
   throws an ExceptionInfo identifying the first failing parameter.

   `context` identifies the call site being validated - either
   `{:protocol sym :method sym}` or `{:record sym :constructor sym}` - and is
   merged into the thrown ex-data."
  [args-schema args-validator param-syms values context]
  (when-not (= (count param-syms) (count values))
    (throw (ex-info (if (:protocol context)
                      (str "Invalid arguments passed to " (call-target-description context))
                      (str "Invalid parameter passed to " (call-target-description context)))
                    (assoc context
                           :type :malt/arity-mismatch
                           :expected (count param-syms)
                           :actual (count values)
                           :input (mapv (constantly '_) values)
                           :value values))))
  (when-not (args-validator values)
    (let [errors (me/humanize (m/explain args-schema values))
          failing-idx (or (->> errors
                               (map-indexed vector)
                               (filter (fn [[_idx idx-errors]]
                                         (some? idx-errors)))
                               (ffirst))
                          0)
          param-name (some-> (nth param-syms failing-idx nil) name)
          message (if param-name
                    (str "Invalid parameter '" param-name "' passed to "
                         (call-target-description context))
                    (if (:protocol context)
                      (str "Invalid arguments passed to " (call-target-description context))
                      (str "Invalid parameter passed to " (call-target-description context))))]
      (throw (ex-info message
                      (assoc context
                             :type (or (:type context) :malt/input-validation-failed)
                             :errors errors
                             :input (mapv (fn [idx value]
                                            (if (= idx failing-idx) value '_))
                                          (range)
                                          values)))))))

(defn validate-value!
  "Validates a single value against a schema. On failure throws an ExceptionInfo
   with `message`, merging `data` with the humanized errors and the value keyed
   under the failing `phase` (`:input` or `:output`)."
  [schema validator value {:keys [message data phase type]}]
  (when-not (validator value)
    (let [errors (me/humanize (m/explain schema value))]
      (throw (ex-info message
                      (cond-> (assoc data
                                     :type type
                                     :errors errors)
                        (= :input phase) (assoc :input value)
                        (= :output phase) (assoc :output value)))))))

(defn check-throws!
  "Handles an exception escaping the implementation body of a protocol method.

   Malt errors are matched exclusively against the declared error definitions by
   `:code`; any other exception is matched exclusively against the declared
   exception classes with `instance?`. Rethrows the exception unchanged when it
   matches a declaration (and its data validates against the definition's
   `:schema`, when present), otherwise wraps it in an ExceptionInfo describing
   the contract violation.

   Methods without a `(throws [...])` clause are expected not to throw at all -
   `throws-defs` is empty and every exception is wrapped as unspecified."
  [caught throws-defs exception-validators protocol-sym method-sym]
  (let [data (when (instance? clojure.lang.IExceptionInfo caught)
               (ex-data caught))
        malt-error? (= :malt/error (:type data))
        matching-def (if malt-error?
                       (some (fn [definition]
                               (when (and (:code definition)
                                          (= (:code data) (:code definition)))
                                 definition))
                             throws-defs)
                       (some (fn [definition]
                               (when-let [klass (:class definition)]
                                 (when (instance? klass caught)
                                   definition)))
                             throws-defs))
        error-data (if malt-error?
                     (:data data)
                     data)
        validator (when matching-def
                    (get exception-validators
                         (or (:code matching-def)
                             (:class matching-def))))]
    (cond
      (nil? matching-def)
      (throw (ex-info (str "Unspecified exception thrown from method '"
                           (name method-sym) "' of " protocol-sym)
                      {:type :malt/unspecified-exception-error
                       :protocol protocol-sym
                       :method method-sym}
                      caught))

      (or (nil? validator)
          (validator error-data))
      (throw caught)

      :else
      (let [errors (me/humanize (m/explain (:schema matching-def) error-data))]
        (throw (ex-info (str "Invalid exception thrown from method '"
                             (name method-sym) "' of " protocol-sym)
                        {:type :malt/invalid-exception-error
                         :protocol protocol-sym
                         :method method-sym
                         :data error-data
                         :errors errors}
                        caught))))))
