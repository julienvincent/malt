(ns ^:no-doc io.julienvincent.malt.schema
  "Resolution of authored schema forms into malli schemas.

   Schemas in malt definitions can be written as literals, as symbols
   referencing vars, or as function-call forms producing a schema. This
   namespace turns those authored forms into fully resolved malli schema data.")

(defn resolve-schema-spec
  "Recursively resolves the authored schema form `schema-spec` in the context of
   `schema-ns`.

   - Symbols are resolved through the namespace and replaced by their var's
     value
   - Function-call (seq) forms are evaluated
   - Vectors, maps and sets are walked recursively

   The result contains no unresolved symbols or unevaluated forms and is safe to
   pass to malli."
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
