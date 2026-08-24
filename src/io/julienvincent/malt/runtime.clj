(ns ^:no-doc io.julienvincent.malt.runtime
  "Runtime support invoked by the code emitted from the io.julienvincent.malt
   macros. Nothing in this namespace is intended to be called directly by users."
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

(defn- enrich-spec
  "Resolves an authored method spec and precompiles its validators."
  [protocol-ns spec]
  (let [resolve-schema #(malt.schema/resolve-schema-spec protocol-ns %)
        arg-schemas (when (seq (:arguments-schema spec))
                      (mapv resolve-schema (rest (:arguments-schema spec))))
        return-schema (resolve-schema (:return-schema spec))
        throws (when (seq (:throws spec))
                 (resolve-throws-definitions protocol-ns (:throws spec)))]
    (cond-> spec
      arg-schemas
      (assoc :arguments-schema (into [:cat] arg-schemas)
             :arguments-validator (m/validator (into [:cat] arg-schemas)))

      (and arg-schemas (:param-schemas spec))
      (assoc :param-schemas
             (zipmap (mapv (comp keyword name) (:params spec))
                     arg-schemas))

      return-schema
      (assoc :return-schema return-schema
             :return-validator (m/validator return-schema))

      throws
      (assoc :throws throws
             :exception-validators
             (into {}
                   (keep (fn [{:keys [code schema] klass :class}]
                           (when schema
                             [(or code klass) (m/validator schema)])))
                   throws)))))

(def ^:private spec->legacy-sig-keys
  {:params :malt/params
   :param-schemas :malt/param-schemas
   :arguments-schema :malt/arguments-schema
   :return-schema :malt/return-schema
   :throws :malt/throws
   :arguments-validator :malt/arguments-validator
   :return-validator :malt/return-validator
   :exception-validators :malt/exception-validators})

(defn- spec->legacy-sig-data
  [spec]
  (reduce-kv (fn [sig-data spec-k value]
               (if-let [sig-k (get spec->legacy-sig-keys spec-k)]
                 (assoc sig-data sig-k value)
                 sig-data))
             {}
             spec))

(defn- enrich-sig
  [protocol-ns sig authored-specs]
  (let [specs (mapv #(enrich-spec protocol-ns %) authored-specs)
        sig (assoc sig :malt/specs specs)]
    (if (= 1 (count specs))
      (merge sig (spec->legacy-sig-data (first specs)))
      sig)))

(def ^:private var-meta-keys
  [:malt/params
   :malt/param-schemas
   :malt/arguments-schema
   :malt/return-schema
   :malt/throws
   :malt/arguments-validator
   :malt/return-validator
   :malt/exception-validators])

(defn- method-var-meta
  [sig]
  (assoc (select-keys sig var-meta-keys)
         :malt/specs
         (:malt/specs sig)))

(defn enrich-protocol-var!
  "Marks `protocol-var` as a malt protocol and installs the authored method
   specs onto the protocol's method signatures, resolving schemas and compiling
   validators. The complete enriched specs are also attached to the generated
   method vars. Called once as part of a `malt/defprotocol` definition.

   `method-specs` maps each method keyword to its authored malt specs."
  [protocol-var method-specs]
  (let [protocol-ns (:ns (meta protocol-var))]
    (alter-var-root
     protocol-var
     (fn [protocol]
       (-> protocol
           (assoc :malt/protocol true)
           (update :sigs
                   (fn [sigs]
                     (reduce-kv
                      (fn [sigs method-kw specs]
                        (update sigs method-kw
                                #(enrich-sig protocol-ns % specs)))
                      sigs
                      method-specs))))))
    (doseq [method-kw (keys method-specs)]
      (let [sig (get-in @protocol-var [:sigs method-kw])]
        (when-let [method-var (ns-resolve protocol-ns (:name sig))]
          (alter-meta! method-var merge (method-var-meta sig))))))
  protocol-var)

(defn method-spec
  "Returns the enriched spec for a protocol method arity."
  [protocol-var method-kw arity]
  (or (some (fn [spec]
              (when (= arity (count (:params spec)))
                spec))
            (get-in @protocol-var [:sigs method-kw :malt/specs]))
      (throw (IllegalArgumentException.
              (str "No arity " arity " found for protocol method " method-kw)))))

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
