(ns ^:no-doc io.julienvincent.malt.syntax
  "Macro-time parsing and normalization of the io.julienvincent.malt macro
   syntax. Everything here operates on unevaluated forms during macro expansion."
  (:require
   [io.julienvincent.malt.runtime :as malt.runtime]))

(defn take-doc+attrs
  "Splits an optional leading docstring and attribute map from `forms`. Returns
   `{:doc <string?> :attrs <map?> :forms <remaining>}`."
  [forms]
  (let [[doc forms] (if (string? (first forms))
                      [(first forms) (rest forms)]
                      [nil forms])
        [attrs forms] (if (map? (first forms))
                        [(first forms) (rest forms)]
                        [nil forms])]
    {:doc doc
     :attrs attrs
     :forms forms}))

(defn parse-schema-pairs
  "Parses a vector of name/schema pairs into `{:params [...] :schemas [...]}`.

   `pairs-error` and `symbols-error` are the error message prefixes used when
   the input is malformed."
  [pairs {:keys [pairs-error symbols-error]}]
  (let [elems (vec pairs)]
    (when (odd? (count elems))
      (throw (IllegalArgumentException.
              (str pairs-error "; got " (pr-str pairs)))))
    (let [params (vec (take-nth 2 elems))
          schemas (vec (take-nth 2 (rest elems)))]
      (when-not (every? symbol? params)
        (throw (IllegalArgumentException.
                (str symbols-error "; got " (pr-str pairs)))))
      {:params params
       :schemas schemas})))

(defn normalize-protocol-method
  "Normalizes a malt method spec into a clojure.core/defprotocol method form,
   attaching the authored schemas as :malt/\\* metadata on the method name."
  [protocol-sym method-spec]
  (let [[method-sym & method-forms] method-spec
        {:keys [doc attrs forms]} (take-doc+attrs method-forms)]
    (when-not (<= 2 (count forms) 3)
      (throw (IllegalArgumentException.
              (str "Method spec must be of the form "
                   "(" method-sym " <optional docstring> "
                   "<optional metadata> [input-schema-1 ...] "
                   "output-schema <optional (throws [...])>) for "
                   protocol-sym "; got " (pr-str method-spec)))))
    (let [[input-schemas output-schema throws-form] forms]
      (when throws-form
        (when-not (and (seq? throws-form)
                       (= 'throws (first throws-form))
                       (vector? (second throws-form)))
          (throw (IllegalArgumentException.
                  (str "throws clause must be of the form "
                       "(throws [def1 def2 ...]) for "
                       protocol-sym "/" method-sym "; got "
                       (pr-str throws-form))))))
      (when-not (vector? input-schemas)
        (throw (IllegalArgumentException.
                (str "Input schemas must be a vector for "
                     protocol-sym "/" method-sym "; got "
                     (pr-str input-schemas)))))
      (let [{:keys [params schemas]}
            (parse-schema-pairs
             input-schemas
             {:pairs-error (str "Input schemas must be param/schema pairs for "
                                protocol-sym "/" method-sym)
              :symbols-error (str "Parameter names must be symbols for "
                                  protocol-sym "/" method-sym)})]
        (when (some #{'this} params)
          (throw (IllegalArgumentException.
                  (str "Parameter name must not be `this` for "
                       protocol-sym "/" method-sym "; got "
                       (pr-str input-schemas)))))
        (let [throws-syms (when throws-form
                            (second throws-form))
              schema-meta (cond-> {:malt/params params
                                   :malt/arguments-schema (when (seq params)
                                                            (into [:cat] schemas))
                                   :malt/return-schema output-schema}
                            (seq params)
                            (assoc :malt/param-schemas
                                   (zipmap (mapv (comp keyword name) params)
                                           schemas))

                            throws-syms
                            (assoc :malt/throws (vec throws-syms)))
              method-meta (cond-> (merge (meta method-sym) schema-meta)
                            doc (assoc :doc doc)
                            attrs (merge attrs))]
          (list* (with-meta method-sym method-meta)
                 (into ['this] params)
                 (cond-> []
                   doc (conj doc)
                   attrs (conj attrs))))))))

(defn group-implementations
  "Groups a flat seq of protocol symbols and method forms - as accepted by
   reify/extend-type/defrecord - into [protocol-sym [method-form ...]] pairs,
   preserving order."
  [forms]
  (loop [remaining forms
         current-protocol nil
         current-methods []
         grouped []]
    (cond
      (empty? remaining)
      (cond-> grouped
        current-protocol (conj [current-protocol current-methods]))

      (symbol? (first remaining))
      (recur (rest remaining)
             (first remaining)
             []
             (cond-> grouped
               current-protocol (conj [current-protocol current-methods])))

      :else
      (recur (rest remaining)
             current-protocol
             (conj current-methods (first remaining))
             grouped))))

(defn- normalize-method-impl
  "Wraps a protocol method implementation with input/output validation and
   checked exception handling.

   The generated body obtains the enriched method signature from the protocol
   var - referenced directly, resolved at compile time - so a call only pays for
   a deref, the signature lookup and the precompiled validators."
  [protocol-sym method-form]
  (when-not (seq? method-form)
    (throw (IllegalArgumentException.
            (str "Method implementation must be a list; got " (pr-str method-form)))))
  (let [protocol-var (resolve protocol-sym)]
    (when-not (var? protocol-var)
      (throw (IllegalArgumentException.
              (str "Protocol must resolve to a var; got " (pr-str protocol-sym)))))
    (let [qualified-protocol-sym (symbol (str (ns-name (:ns (meta protocol-var))))
                                         (str (:name (meta protocol-var))))
          [method-sym arglist & body] method-form
          method-kw (keyword (name method-sym))]
      (when-not (contains? (:sigs @protocol-var) method-kw)
        (throw (IllegalArgumentException.
                (str "No method '" method-sym "' found on protocol "
                     qualified-protocol-sym))))
      (when-not (vector? arglist)
        (throw (IllegalArgumentException.
                (str "Argument list must be a vector for " qualified-protocol-sym "/" method-sym
                     "; got " (pr-str arglist)))))
      (let [[this-binding & param-bindings] arglist]
        (when-not this-binding
          (throw (IllegalArgumentException.
                  (str "Argument list must include a `this` binding for "
                       qualified-protocol-sym "/" method-sym
                       "; got " (pr-str arglist)))))
        (let [this-sym (gensym "this-")
              params-syms (mapv (fn [_binding] (gensym "param-")) param-bindings)
              destructure-bindings (->> (cons [this-binding this-sym]
                                              (map vector param-bindings params-syms))
                                        (mapcat identity)
                                        (vec))]
          (list method-sym
                (into [this-sym] params-syms)
                `(let [sig# (malt.runtime/method-sig (var ~qualified-protocol-sym) ~method-kw)]
                   (when-let [args-validator# (:malt/arguments-validator sig#)]
                     (malt.runtime/validate-inputs! (:malt/arguments-schema sig#)
                                                    args-validator#
                                                    (:malt/params sig#)
                                                    [~@params-syms]
                                                    {:protocol '~qualified-protocol-sym
                                                     :method '~method-sym}))
                   (let [~@destructure-bindings]
                     (try
                       (let [result# (do ~@body)]
                         (malt.runtime/validate-value!
                          (:malt/return-schema sig#)
                          (:malt/return-validator sig#)
                          result#
                          {:type :malt/output-validation-failed
                           :phase :output
                           :message ~(str "Invalid return value from '"
                                          (name method-sym)
                                          "' of "
                                          qualified-protocol-sym)
                           :data {:protocol '~qualified-protocol-sym
                                  :method '~method-sym}})
                         result#)
                       (catch Exception ex#
                         (if-let [throws-defs# (:malt/throws sig#)]
                           (malt.runtime/check-throws! ex#
                                                       throws-defs#
                                                       (:malt/exception-validators sig#)
                                                       '~qualified-protocol-sym
                                                       '~method-sym)
                           (throw ex#))))))))))))

(defn wrap-method-impls
  "Given grouped [protocol-sym methods] pairs, returns the flat seq of protocol
   symbols and method forms with malt-protocol methods wrapped in validation.
   Methods of non-malt protocols are passed through untouched."
  [grouped missing-protocol-error]
  (mapcat (fn [[protocol-sym methods]]
            (when-not protocol-sym
              (throw (IllegalArgumentException. missing-protocol-error)))
            (let [protocol-var (resolve protocol-sym)
                  malt-protocol? (and (var? protocol-var)
                                      (:malt/protocol @protocol-var))]
              (cons protocol-sym
                    (if malt-protocol?
                      (mapv #(normalize-method-impl protocol-sym %) methods)
                      methods))))
          grouped))

(defn normalize-extend-type-sym
  "Resolves a namespace-qualified record symbol - such as `other.ns/Record` or
   `alias/Record` - to the munged class name symbol expected by
   clojure.core/extend-type. Unqualified symbols pass through untouched."
  [type-sym]
  (if (and (symbol? type-sym) (namespace type-sym))
    (let [ns-part (symbol (namespace type-sym))
          alias-ns (get (ns-aliases *ns*) ns-part)
          ns-name-str (clojure.lang.Compiler/munge
                       (str (or (some-> alias-ns ns-name)
                                ns-part)))
          record-name-str (clojure.lang.Compiler/munge (name type-sym))]
      (symbol (str ns-name-str "." record-name-str)))
    type-sym))
