(ns io.julienvincent.malt
  (:refer-clojure :exclude [defprotocol extend])
  (:require
   [malli.core :as m]
   [malli.error :as me]))

(defn validate!
  [schema value ex-data]
  (when-not (m/validate schema value)
    (let [explain (m/explain schema value)
          phase (:phase ex-data)
          failure-type (or (:type ex-data)
                           (case phase
                             :input :malt/input-validation-failed
                             :output :malt/output-validation-failed
                             nil))
          data (-> ex-data
                   (dissoc :phase :type :index :schema :value :explain :errors :output :input)
                   (assoc :type failure-type
                          :errors (me/humanize explain))
                   (cond-> (= :output phase)
                     (assoc :output value))
                   (cond-> (= :input phase)
                     (assoc :input (:input ex-data))))]
      (throw (ex-info "Malli validation failed"
                      data)))))

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
                                     schema-meta {::input-schemas (vec schemas)
                                                  ::output-schema output-schema}
                                     method-meta (cond-> (merge (meta method-name) schema-meta)
                                                   method-doc (assoc :doc method-doc)
                                                   method-attr (merge method-attr))
                                     method-name (with-meta method-name method-meta)
                                     arglist (into ['this] params)]
                                 (list method-name arglist)))))]
    `(clojure.core/defprotocol ~name-sym
       ~@(mapv normalize-method specs))))

(defn schema-vars-for-method
  [protocol-sym method-sym]
  (let [protocol-var (resolve protocol-sym)]
    (when-not (var? protocol-var)
      (throw (IllegalArgumentException.
              (str "Protocol must resolve to a var; got " (pr-str protocol-sym)))))
    (let [sigs (:sigs @protocol-var)
          method-kw (keyword (name method-sym))
          method-sig (get sigs method-kw)
          input-schema-specs (or (::input-schemas method-sig)
                                 (some-> (::input-schema method-sig) vector))
          output-schema-spec (::output-schema method-sig)
          protocol-ns (:ns (meta protocol-var))
          resolve-schema (fn [schema-spec]
                           (cond
                             (nil? schema-spec) (throw (IllegalArgumentException.
                                                        "Schema must not be nil"))
                             (var? schema-spec) (deref schema-spec)
                             (symbol? schema-spec) (let [schema-var (ns-resolve protocol-ns schema-spec)]
                                                     (when-not (var? schema-var)
                                                       (throw (IllegalArgumentException.
                                                               (str "Schema symbol must resolve to a var; got "
                                                                    (pr-str schema-spec)))))
                                                     (deref schema-var))
                             :else schema-spec))]
      (when-not (vector? input-schema-specs)
        (throw (IllegalArgumentException.
                (str "Missing input schema specs for " protocol-sym "/" method-kw))))
      (when (nil? output-schema-spec)
        (throw (IllegalArgumentException.
                (str "Missing output schema spec for " protocol-sym "/" method-kw))))
      [(mapv resolve-schema input-schema-specs)
       (resolve-schema output-schema-spec)])))

(defn validate-inputs!
  [input-schemas params ex-data]
  (when-not (= (count input-schemas) (count params))
    (throw (ex-info "Malli validation failed"
                    (assoc ex-data
                           :phase :input
                           :type :malt/arity-mismatch
                           :expected (count input-schemas)
                           :actual (count params)
                           :input (mapv (constantly '_) params)
                           :value params))))
  (doseq [[schema value idx] (map vector input-schemas params (range))]
    (validate! schema
               value
               (assoc ex-data
                      :phase :input
                      :type :malt/input-validation-failed
                      :input (mapv (fn [current-idx current-value]
                                     (if (= current-idx idx) current-value '_))
                                   (range)
                                   params)))))

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
            params-syms (mapv (fn [binding]
                                (if (symbol? binding) binding (gensym "param-")))
                              param-bindings)
            destructure-bindings (->> (map vector param-bindings params-syms)
                                      (remove (fn [[binding _sym]] (symbol? binding)))
                                      (mapcat identity)
                                      (vec))
            input-schema-sym (gensym "input-schema-")
            output-schema-sym (gensym "output-schema-")
            ex-data-base-sym (gensym "ex-data-base-")
            result-sym (gensym "result-")
            ex-data-base {:protocol (list 'quote qualified-protocol-sym)
                          :method (list 'quote method-sym)}]
        (list method-sym
              (into [this-binding] params-syms)
              `(let [[~input-schema-sym ~output-schema-sym]
                     (schema-vars-for-method '~qualified-protocol-sym '~method-sym)
                     ~ex-data-base-sym ~ex-data-base]
                 (validate-inputs! ~input-schema-sym [~@params-syms] ~ex-data-base-sym)
                 ~(if (seq destructure-bindings)
                    `(let [~@destructure-bindings]
                       (let [~result-sym (do ~@body)]
                         (validate! ~output-schema-sym
                                    ~result-sym
                                    (assoc ~ex-data-base-sym
                                           :phase :output
                                           :type :malt/output-validation-failed))
                         ~result-sym))
                    `(let [~result-sym (do ~@body)]
                       (validate! ~output-schema-sym
                                  ~result-sym
                                  (assoc ~ex-data-base-sym
                                         :phase :output
                                         :type :malt/output-validation-failed))
                       ~result-sym))))))))

(defmacro extend
  {:style/indent :defn}
  [type-sym & protocol+method-forms]
  (let [grouped (parse-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "extend requires at least one protocol; got " (pr-str type-sym)))))
    `(clojure.core/extend-type ~type-sym
       ~@(mapcat (fn [[protocol-sym methods]]
                   (when-not protocol-sym
                     (throw (IllegalArgumentException.
                             (str "Missing protocol in extend for " (pr-str type-sym)))))
                   (cons protocol-sym
                         (mapv (partial normalize-method-impl protocol-sym) methods)))
                 grouped))))

(defmacro implement
  {:style/indent :defn}
  [& protocol+method-forms]
  (let [grouped (parse-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "implement requires at least one protocol; got "
                   (pr-str protocol+method-forms)))))
    `(clojure.core/reify
       ~@(mapcat (fn [[protocol-sym methods]]
                   (when-not protocol-sym
                     (throw (IllegalArgumentException.
                             (str "Missing protocol in implement; got "
                                  (pr-str protocol+method-forms)))))
                   (cons protocol-sym
                         (mapv (partial normalize-method-impl protocol-sym) methods)))
                 grouped))))
