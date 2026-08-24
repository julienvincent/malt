(ns io.julienvincent.malt
  (:refer-clojure :exclude [defprotocol extend-type defrecord reify])
  (:require
   [io.julienvincent.malt.runtime :as malt.runtime]
   [io.julienvincent.malt.schema :as malt.schema]
   [io.julienvincent.malt.syntax :as malt.syntax]
   [malli.core :as m]))

(defmacro defprotocol
  "Defines a native Clojure protocol whose methods carry Malli schemas.

   This is the core malt API entrypoint.

   A method spec takes param/schema pairs followed by a return schema and an
   optional `(throws [...])` clause for checked exceptions. Methods with
   multiple fixed arities wrap each complete spec in a list.

   Methods without a `throws` clause are expected not to throw.

   ```clojure
   (defprotocol UserStore
     (create-user [name :string age :int] :string)
     (find-user
       ([id :string] ?User)
       ([id :string options ?Options] [:maybe ?User]))
     (suspend-user! [id :string]
       :nil
       (throws [not-found Exception])))
   ```

   All provided data is made available on the produced protocol root value:

   ```clojure
   (malt/defprotocol Foo
     (foo [name :string]
       :string))

   Foo
   ;; =>
   {:malt/protocol true
    :sigs {:foo
           {:malt/specs
            [{:params [name]
              :param-schemas {:name :string}
              :arguments-schema [:cat :string]
              :return-schema :string
              :arguments-validator #object[...]
              :return-validator #object[...]}]
            ...}}}
   ```

   The resolved schema data (excluding the precompiled validators) is also
   attached to the metadata of each generated method var, so a method's
   signature can be read from the method itself:

   ```clojure
   (meta #'foo)
   ;; =>
   {:malt/specs
    [{:params [name]
      :param-schemas {:name :string}
      :arguments-schema [:cat :string]
      :return-schema :string}]
    ...}
   ```

   Also defines a `?Name` var containing a Malli schema which can be used to
   assert a value implements the protocol. This is done through `satisfies?`."
  {:style/indent [1 :form [1]]}
  [protocol-sym & specs]
  (let [{:keys [doc attrs forms]} (malt.syntax/take-doc+attrs specs)
        name-sym (with-meta protocol-sym
                   (merge (meta protocol-sym)
                          attrs
                          (when doc {:doc doc})))
        protocol-schema-sym (symbol (str "?" protocol-sym))
        qualified-protocol-sym (symbol (str (ns-name *ns*)) (str protocol-sym))
        methods (mapv #(malt.syntax/normalize-protocol-method protocol-sym %) forms)
        method-specs (into {} (map (juxt :method-kw :specs)) methods)]
    `(do
       (clojure.core/defprotocol ~name-sym
         ~@(cond-> []
             doc (conj doc)
             attrs (conj attrs))
         ~@(mapv :form methods))
       (malt.runtime/enrich-protocol-var! (var ~name-sym) '~method-specs)
       (def ~protocol-schema-sym
         [:fn
          {:error/message ~(str "should satisfy " qualified-protocol-sym)}
          (fn [value#]
            (satisfies? ~name-sym value#))])
       (var ~name-sym))))

(defmacro defrecord
  ;; pruner-ignore
  "Defines a native Clojure record with schema-validated constructors.

   Fields are given as field/schema pairs. Inline implementations of malt
   protocols are wrapped with input/output validation:

   ```clojure
   (defrecord UserStoreImpl
    [db ?DataSource]

     UserStore
     (create-user [_ name age]
       (persist-user db name age)))
   ```

   Overrides the generated `->Name` and `map->Name` constructors to validate
   their inputs, and additionally defines `?NameSchema` (a Malli :map schema of
   the fields) and `?Name` (an `instance?` check)."
  {:style/indent [1 :form [1]]}
  [record-sym & specs]
  (let [{:keys [doc attrs forms]} (malt.syntax/take-doc+attrs specs)
        [fields & impls] forms]
    (when-not (vector? fields)
      (throw (IllegalArgumentException.
              (str "Fields must be a vector for " record-sym "; got " (pr-str fields)))))
    (let [{:keys [params schemas]}
          (malt.syntax/parse-schema-pairs
           fields
           {:pairs-error (str "Fields must be param/schema pairs for " record-sym)
            :symbols-error (str "Field names must be symbols for " record-sym)})
          name-sym (with-meta record-sym
                     (merge (meta record-sym)
                            attrs
                            (when doc {:doc doc})))
          ctor-sym (symbol (str "->" record-sym))
          map-ctor-sym (symbol (str "map->" record-sym))
          schema-sym (symbol (str "?" record-sym "Schema"))
          instance-schema-sym (symbol (str "?" record-sym))
          record-ns-sym (ns-name *ns*)
          qualified-record-sym (symbol (str record-ns-sym) (str record-sym))
          field-ks (mapv (comp keyword name) params)
          impl-forms (malt.syntax/wrap-method-impls
                      (malt.syntax/group-implementations impls)
                      (str "Missing protocol in defrecord for " (pr-str name-sym)))]
      `(do
         (clojure.core/defrecord ~name-sym [~@params] ~@impl-forms)
         (let [orig-ctor# ~ctor-sym
               orig-map-ctor# ~map-ctor-sym
               schema-ns# (the-ns '~record-ns-sym)
               field-schemas# (mapv (fn [schema-spec#]
                                      (malt.schema/resolve-schema-spec schema-ns# schema-spec#))
                                    '~schemas)
               args-schema# (into [:cat] field-schemas#)
               args-validator# (m/validator args-schema#)
               map-schema# (into [:map {:closed true}]
                                 (mapv (fn [field-k# field-schema#]
                                         [field-k# field-schema#])
                                       '~field-ks
                                       field-schemas#))
               map-validator# (m/validator map-schema#)
               context# {:record '~qualified-record-sym
                         :constructor '~ctor-sym
                         :type :malt/record-validation-failed}]
           (def ~schema-sym map-schema#)
           (def ~instance-schema-sym
             [:fn
              {:error/message ~(str "should be an instance of " qualified-record-sym)}
              (fn [value#]
                (instance? ~name-sym value#))])
           (defn ~ctor-sym
             ~params
             (malt.runtime/validate-inputs! args-schema#
                                            args-validator#
                                            '~params
                                            [~@params]
                                            context#)
             (orig-ctor# ~@params))
           (defn ~map-ctor-sym
             [m#]
             (malt.runtime/validate-value!
              map-schema#
              map-validator#
              m#
              {:type :malt/record-validation-failed
               :phase :input
               :message ~(str "Invalid parameter passed to constructor '"
                              map-ctor-sym
                              "' of "
                              qualified-record-sym)
               :data {:record '~qualified-record-sym
                      :constructor '~map-ctor-sym}})
             (orig-map-ctor# m#)))))))

(defmacro extend-type
  "Like [clojure.core/extend-type], but methods of malt protocols are wrapped
   with input/output validation.

   Additionally supports passing a namespace-qualified record symbol (for
   example `other.ns/SomeRecord`) which is resolved to the underlying class."
  {:style/indent [1 :form [1]]}
  [type-sym & protocol+method-forms]
  (let [type-sym (malt.syntax/normalize-extend-type-sym type-sym)
        grouped (malt.syntax/group-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "extend-type requires at least one protocol; got " (pr-str type-sym)))))
    `(clojure.core/extend-type ~type-sym
       ~@(malt.syntax/wrap-extend-method-impls
          grouped
          (str "Missing protocol in extend-type for " (pr-str type-sym))))))

(defmacro reify
  "Like [clojure.core/reify], but methods of malt protocols are wrapped with
   input/output validation."
  {:style/indent [:defn [1]]}
  [& protocol+method-forms]
  (let [grouped (malt.syntax/group-implementations protocol+method-forms)]
    (when (empty? grouped)
      (throw (IllegalArgumentException.
              (str "reify requires at least one protocol; got "
                   (pr-str protocol+method-forms)))))
    `(clojure.core/reify
       ~@(malt.syntax/wrap-method-impls
          grouped
          (str "Missing protocol in reify; got " (pr-str protocol+method-forms))))))
