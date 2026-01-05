(ns io.julienvincent.malt
  (:refer-clojure :exclude [defprotocol extend-type defrecord reify])
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(defn validate!
  [schema value ex-data]
  (let [validator (:validator ex-data)
        msg (:message ex-data)]
    (when-not (fn? validator)
      (throw (IllegalStateException.
              (str "validate! requires a :validator fn; got "
                   (pr-str validator)
                   " with ex-data "
                   (pr-str (dissoc ex-data :validator))))))
    (when-not (string? msg)
      (throw (IllegalStateException.
              (str "validate! requires a :message string; got "
                   (pr-str msg)
                   " with ex-data "
                   (pr-str (dissoc ex-data :message))))))
    (when-not (validator value)
      (let [explain (m/explain schema value)
            phase (:phase ex-data)
            failure-type (or (:type ex-data)
                             (case phase
                               :input :malt/input-validation-failed
                               :output :malt/output-validation-failed
                               nil))
            data (-> ex-data
                     (dissoc :phase :type :index :schema :value :explain :errors :output :input :message :validator)
                     (assoc :type failure-type
                            :errors (me/humanize explain))
                     (cond-> (= :output phase)
                       (assoc :output value))
                     (cond-> (= :input phase)
                       (assoc :input (:input ex-data))))]
        (throw (ex-info msg data))))))

(defmacro defprotocol
  [name & specs]
  (let [[doc-string specs] (if (string? (first specs))
                             [(first specs) (rest specs)]
                             [nil specs])
        [attr-map specs] (if (map? (first specs))
                           [(first specs) (rest specs)]
                           [nil specs])
        name-meta (merge (meta name)
                         attr-map
                         (when doc-string {:doc doc-string}))
        name-sym (with-meta name name-meta)
        protocol-schema-sym (symbol (str "?" name))
        parse-input-specs (fn [protocol-sym method-name input-schemas]
                            (let [elems (vec input-schemas)]
                              (when (odd? (count elems))
                                (throw (IllegalArgumentException.
                                        (str "Input schemas must be param/schema pairs for "
                                             protocol-sym "/" method-name "; got "
                                             (pr-str input-schemas)))))
                              (let [params (vec (take-nth 2 elems))
                                    schemas (vec (take-nth 2 (rest elems)))]
                                (when-not (every? symbol? params)
                                  (throw (IllegalArgumentException.
                                          (str "Parameter names must be symbols for "
                                               protocol-sym "/" method-name "; got "
                                               (pr-str input-schemas)))))
                                (when (some #{'this} params)
                                  (throw (IllegalArgumentException.
                                          (str "Parameter name must not be `this` for "
                                               protocol-sym "/" method-name "; got "
                                               (pr-str input-schemas)))))
                                {:params params
                                 :schemas schemas})))
        normalize-method (fn [method]
                           (let [[method-name & method-forms] method
                                 [method-doc method-forms] (if (string? (first method-forms))
                                                             [(first method-forms)
                                                              (clojure.core/rest method-forms)]
                                                             [nil method-forms])
                                 [method-attr method-forms] (if (map? (first method-forms))
                                                              [(first method-forms)
                                                               (clojure.core/rest method-forms)]
                                                              [nil method-forms])]
                             (when-not (= 2 (count method-forms))
                               (throw (IllegalArgumentException.
                                       (str "Method spec must be of the form "
                                            "(" method-name " <optional docstring> "
                                            "<optional metadata> [input-schema-1 ...] "
                                            "output-schema) for "
                                            name "; got " (pr-str method)))))
                             (let [[input-schemas output-schema] method-forms]
                               (when-not (vector? input-schemas)
                                 (throw (IllegalArgumentException.
                                         (str "Input schemas must be a vector for "
                                              name "/" method-name "; got "
                                              (pr-str input-schemas)))))
                               (let [{:keys [params schemas]} (parse-input-specs name method-name input-schemas)
                                     schema-map (zipmap (mapv (comp keyword clojure.core/name) params) schemas)
                                     schema-meta (cond-> {:malt/params (vec params)
                                                          :malt/arguments-schema (when (seq params)
                                                                                   (into [:cat] schemas))
                                                          :malt/return-schema output-schema}
                                                   (seq params)
                                                   (assoc :malt/param-schemas schema-map))
                                     method-meta (cond-> (merge (meta method-name) schema-meta)
                                                   method-doc (assoc :doc method-doc)
                                                   method-attr (merge method-attr))
                                     method-name (with-meta method-name method-meta)
                                     arglist (into ['this] params)
                                     doc+attr (cond-> []
                                                method-doc (conj method-doc)
                                                method-attr (conj method-attr))]
                                 (list* method-name arglist doc+attr)))))]
    `(do
       (clojure.core/defprotocol ~name-sym
         ~@(cond-> []
             doc-string (conj doc-string)
             attr-map (conj attr-map))
         ~@(mapv normalize-method specs))
       (let [protocol-var# (var ~name-sym)
             protocol-ns# (:ns (meta protocol-var#))
             resolve-schema# (fn [schema-spec#]
                               (resolve-schema-spec protocol-ns# schema-spec#))]
         (alter-var-root
          protocol-var#
          (fn [proto#]
            (update
             (assoc proto# :malt/protocol true)
             :sigs
             (fn [sigs#]
               (into {}
                     (map (fn [[method-kw# sig#]]
                            (let [args-schema-spec# (:malt/arguments-schema sig#)
                                  return-schema-spec# (:malt/return-schema sig#)
                                  sig# (cond-> sig#
                                         (seq args-schema-spec#)
                                         (assoc :malt/arguments-validator
                                                (m/validator
                                                 (into [:cat]
                                                       (mapv resolve-schema#
                                                             (rest args-schema-spec#)))))

                                         return-schema-spec#
                                         (assoc :malt/return-validator
                                                (m/validator (resolve-schema# return-schema-spec#))))]
                              [method-kw# sig#]))
                          sigs#)))))))
       (def ~protocol-schema-sym
         [:fn
          {:error/message ~(str "should satisfy " (symbol (str (ns-name *ns*)) (str name)))}
          (fn [value#]
            (satisfies? ~name-sym value#))])
       (var ~name-sym))))

(defn ^:no-doc resolve-schema-spec
  [schema-ns schema-spec]
  (when (nil? schema-spec)
    (throw (IllegalArgumentException. "Schema must not be nil")))
  (letfn [(resolve-leaf [form strict?]
            (cond
              (var? form) (resolve-leaf (deref form) false)
              (symbol? form) (let [schema-var (ns-resolve schema-ns form)]
                               (cond
                                 (var? schema-var) (resolve-leaf (deref schema-var) false)
                                 strict? (throw (IllegalArgumentException.
                                                 (str "Schema symbol must resolve to a var; got "
                                                      (pr-str form))))
                                 :else form))
              :else form))
          (resolve-walk [form strict?]
            (let [form (resolve-leaf form strict?)]
              (when (nil? form)
                (throw (IllegalArgumentException. "Schema must not be nil")))
              (cond
                (seq? form) (let [evaluated (binding [*ns* schema-ns]
                                              (eval form))]
                              (resolve-walk evaluated false))
                (vector? form) (mapv #(resolve-walk % false) form)
                (map? form) (into (empty form)
                                  (map (fn [[k v]]
                                         [(resolve-walk k false)
                                          (resolve-walk v false)]))
                                  form)
                (set? form) (set (map #(resolve-walk % false) form))
                :else form)))]
    (resolve-walk schema-spec true)))

(defn schema-vars-for-method
  [protocol-sym method-sym]
  (let [protocol-var (resolve protocol-sym)]
    (when-not (var? protocol-var)
      (throw (IllegalArgumentException.
              (str "Protocol must resolve to a var; got " (pr-str protocol-sym)))))
    (let [sigs (:sigs @protocol-var)
          method-kw (keyword (name method-sym))
          method-sig (get sigs method-kw)
          args-schema-spec (:malt/arguments-schema method-sig)
          args-validator (:malt/arguments-validator method-sig)
          return-schema-spec (:malt/return-schema method-sig)
          return-validator (:malt/return-validator method-sig)
          input-param-syms (vec (:malt/params method-sig))
          protocol-ns (:ns (meta protocol-var))
          resolve-schema (fn [schema-spec]
                           (resolve-schema-spec protocol-ns schema-spec))]
      (when (nil? return-schema-spec)
        (throw (IllegalArgumentException.
                (str "Missing return schema spec for " protocol-sym "/" method-kw))))
      [(when (seq args-schema-spec)
         (into [:cat] (mapv resolve-schema (rest args-schema-spec))))
       args-validator
       (resolve-schema return-schema-spec)
       return-validator
       input-param-syms])))

(declare parse-implementations normalize-method-impl)

(defmacro defrecord
  [name & specs]
  (let [[doc-string specs] (if (string? (first specs))
                             [(first specs) (rest specs)]
                             [nil specs])
        [attr-map specs] (if (map? (first specs))
                           [(first specs) (rest specs)]
                           [nil specs])
        [fields & impls] specs]
    (when-not (vector? fields)
      (throw (IllegalArgumentException.
              (str "Fields must be a vector for " name "; got " (pr-str fields)))))
    (let [elems (vec fields)]
      (when (odd? (count elems))
        (throw (IllegalArgumentException.
                (str "Fields must be param/schema pairs for " name "; got " (pr-str fields)))))
      (let [params (vec (take-nth 2 elems))
            schema-specs (vec (take-nth 2 (rest elems)))]
        (when-not (every? symbol? params)
          (throw (IllegalArgumentException.
                  (str "Field names must be symbols for " name "; got " (pr-str fields)))))
        (let [name-meta (merge (meta name)
                               attr-map
                               (when doc-string {:doc doc-string}))
              name-sym (with-meta name name-meta)
              ctor-sym (symbol (str "->" name))
              map-ctor-sym (symbol (str "map->" name))
              schema-sym (symbol (str "?" name "Schema"))
              instance-schema-sym (symbol (str "?" name))
              record-ns-sym (ns-name *ns*)
              qualified-record-sym (symbol (str record-ns-sym) (str name))
              field-ks (mapv (comp keyword clojure.core/name) params)
              grouped-impls (parse-implementations impls)
              normalized-impls (mapcat (fn [[protocol-sym methods]]
                                         (when-not protocol-sym
                                           (throw (IllegalArgumentException.
                                                   (str "Missing protocol in defrecord for "
                                                        (pr-str name-sym)))))
                                         (let [protocol-var (resolve protocol-sym)
                                               malt-protocol? (and (var? protocol-var)
                                                                   (:malt/protocol @protocol-var))]
                                           (cons protocol-sym
                                                 (if malt-protocol?
                                                   (mapv (partial normalize-method-impl protocol-sym) methods)
                                                   methods))))
                                       grouped-impls)]
          `(do
             (clojure.core/defrecord ~name-sym [~@params] ~@normalized-impls)
             (let [orig-ctor# ~ctor-sym
                   orig-map-ctor# ~map-ctor-sym
                   schema-ns# (the-ns '~record-ns-sym)
                   field-schemas# (mapv (fn [schema-spec#]
                                          (resolve-schema-spec schema-ns# schema-spec#))
                                        '~schema-specs)
                   args-schema# (into [:cat] field-schemas#)
                   args-validator# (m/validator args-schema#)
                   map-schema# (into [:map {:closed true}]
                                     (mapv (fn [field-k# field-schema#]
                                             [field-k# field-schema#])
                                           '~field-ks
                                           field-schemas#))
                   map-validator# (m/validator map-schema#)
                   ex-data-base# {:record (quote ~qualified-record-sym)}]
               (def ~schema-sym map-schema#)
               (def ~instance-schema-sym
                 [:fn
                  {:error/message ~(str "should be an instance of " qualified-record-sym)}
                  (fn [value#]
                    (instance? ~name-sym value#))])
               (defn ~ctor-sym
                 ~(into [] params)
                 (validate-inputs! args-schema#
                                   args-validator#
                                   '~params
                                   [~@params]
                                   (assoc ex-data-base#
                                          :constructor (quote ~ctor-sym)
                                          :type :malt/record-validation-failed))
                 (orig-ctor# ~@params))
               (defn ~map-ctor-sym
                 [m#]
                 (validate! map-schema#
                            m#
                            (assoc ex-data-base#
                                   :constructor (quote ~map-ctor-sym)
                                   :phase :input
                                   :type :malt/record-validation-failed
                                   :input m#
                                   :validator map-validator#
                                   :message ~(str "Invalid parameter passed to constructor '"
                                                  (clojure.core/name map-ctor-sym)
                                                  "' of "
                                                  qualified-record-sym)))
                 (orig-map-ctor# m#)))))))))

(defn validate-inputs!
  [args-schema args-validator input-param-syms params ex-data]
  (let [protocol-sym (:protocol ex-data)
        method-sym (:method ex-data)
        record-sym (:record ex-data)
        ctor-sym (:constructor ex-data)
        base-msg (cond
                   (and protocol-sym method-sym) (str "Invalid arguments passed to '"
                                                      (name method-sym)
                                                      "' of "
                                                      protocol-sym)
                   (and record-sym ctor-sym) (str "Invalid parameter passed to constructor '"
                                                  (name ctor-sym)
                                                  "' of "
                                                  record-sym)
                   :else (throw (IllegalStateException.
                                 (str "validate-inputs! requires either :protocol/:method or "
                                      ":record/:constructor; got "
                                      (pr-str (select-keys ex-data
                                                           [:protocol :method :record :constructor]))))))]
    (when-not (fn? args-validator)
      (throw (IllegalStateException.
              (str "validate-inputs! requires an args-validator fn; got "
                   (pr-str args-validator)))))
    (when-not (= (count input-param-syms) (count params))
      (throw (ex-info base-msg
                      (assoc (dissoc ex-data :phase)
                             :type :malt/arity-mismatch
                             :expected (count input-param-syms)
                             :actual (count params)
                             :input (mapv (constantly '_) params)
                             :value params))))
    (when-not (args-validator params)
      (let [explain (m/explain args-schema params)
            errors-by-idx (me/humanize explain)
            failing-idx (or (->> errors-by-idx
                                 (map-indexed vector)
                                 (filter (fn [[_idx idx-errors]]
                                           (some? idx-errors)))
                                 (ffirst))
                            0)
            param-name (some-> (nth input-param-syms failing-idx nil) name)
            msg (cond
                  (and protocol-sym method-sym param-name) (str "Invalid parameter '"
                                                                param-name
                                                                "' passed to '"
                                                                (name method-sym)
                                                                "' of "
                                                                protocol-sym)
                  (and record-sym ctor-sym param-name) (str "Invalid parameter '"
                                                            param-name
                                                            "' passed to constructor '"
                                                            (name ctor-sym)
                                                            "' of "
                                                            record-sym)
                  :else base-msg)
            data (assoc (dissoc ex-data :phase)
                        :type (or (:type ex-data) :malt/input-validation-failed)
                        :errors errors-by-idx
                        :input (mapv (fn [current-idx current-value]
                                       (if (= current-idx failing-idx) current-value '_))
                                     (range)
                                     params))]
        (throw (ex-info msg data))))))

(defn- parse-implementations
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
          [method-sym arglist & body] method-form]
      (when-not (vector? arglist)
        (throw (IllegalArgumentException.
                (str "Argument list must be a vector for " qualified-protocol-sym "/" method-sym
                     "; got " (pr-str arglist)))))
      (let [[this-binding & param-bindings] arglist
            _ (when-not this-binding
                (throw (IllegalArgumentException.
                        (str "Argument list must include a `this` binding for "
                             qualified-protocol-sym "/" method-sym
                             "; got " (pr-str arglist)))))
            this-sym (gensym "this-")
            params-syms (mapv (fn [_binding] (gensym "param-")) param-bindings)
            destructure-bindings (->> (cons [this-binding this-sym]
                                            (map vector param-bindings params-syms))
                                      (mapcat identity)
                                      (vec))
            args-schema-sym (gensym "args-schema-")
            args-validator-sym (gensym "args-validator-")
            return-schema-sym (gensym "return-schema-")
            return-validator-sym (gensym "return-validator-")
            input-param-syms-sym (gensym "input-param-syms-")
            ex-data-base-sym (gensym "ex-data-base-")
            result-sym (gensym "result-")
            ex-data-base {:protocol (list 'quote qualified-protocol-sym)
                          :method (list 'quote method-sym)}]
        (list method-sym
              (into [this-sym] params-syms)
              `(let [[~args-schema-sym ~args-validator-sym ~return-schema-sym ~return-validator-sym ~input-param-syms-sym]
                     (schema-vars-for-method '~qualified-protocol-sym '~method-sym)
                     ~ex-data-base-sym ~ex-data-base]
                 (when ~args-validator-sym
                   (validate-inputs! ~args-schema-sym
                                     ~args-validator-sym
                                     ~input-param-syms-sym
                                     [~@params-syms]
                                     ~ex-data-base-sym))
                 (let [~@destructure-bindings]
                   (let [~result-sym (do ~@body)]
                     (validate! ~return-schema-sym
                                ~result-sym
                                (assoc ~ex-data-base-sym
                                       :phase :output
                                       :type :malt/output-validation-failed
                                       :validator ~return-validator-sym
                                       :message ~(str "Invalid return value from '"
                                                      (name method-sym)
                                                      "' of "
                                                      qualified-protocol-sym)))
                     ~result-sym))))))))

(defn- normalize-extend-type-sym
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

(defmacro extend-type
  [type-sym & protocol+method-forms]
  (let [type-sym (normalize-extend-type-sym type-sym)
        grouped (parse-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "extend-type requires at least one protocol; got " (pr-str type-sym)))))
    `(clojure.core/extend-type ~type-sym
       ~@(mapcat (fn [[protocol-sym methods]]
                   (when-not protocol-sym
                     (throw (IllegalArgumentException.
                             (str "Missing protocol in extend-type for " (pr-str type-sym)))))
                   (let [protocol-var (resolve protocol-sym)
                         malt-protocol? (and (var? protocol-var)
                                             (:malt/protocol @protocol-var))]
                     (cons protocol-sym
                           (if malt-protocol?
                             (mapv (partial normalize-method-impl protocol-sym) methods)
                             methods))))
                 grouped))))

(defmacro reify
  [& protocol+method-forms]
  (let [grouped (parse-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "reify requires at least one protocol; got "
                   (pr-str protocol+method-forms)))))
    `(clojure.core/reify
       ~@(mapcat (fn [[protocol-sym methods]]
                   (when-not protocol-sym
                     (throw (IllegalArgumentException.
                             (str "Missing protocol in reify; got "
                                  (pr-str protocol+method-forms)))))
                   (let [protocol-var (resolve protocol-sym)
                         malt-protocol? (and (var? protocol-var)
                                             (:malt/protocol @protocol-var))]
                     (cons protocol-sym
                           (if malt-protocol?
                             (mapv (partial normalize-method-impl protocol-sym) methods)
                             methods))))
                 grouped))))
