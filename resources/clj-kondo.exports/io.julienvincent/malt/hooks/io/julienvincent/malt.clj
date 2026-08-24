(ns hooks.io.julienvincent.malt
  (:refer-clojure :exclude [defprotocol])
  (:require
   [clj-kondo.hooks-api :as api]))

(defn- vector-node? [node]
  (= :vector (:tag node)))

(defn- parse-input-schemas-node
  [input-schemas-node]
  (let [children (:children input-schemas-node)]
    (if (even? (count children))
      {:pair-form? true
       :param-nodes (vec (take-nth 2 children))
       :schema-nodes (vec (take-nth 2 (rest children)))}
      {:pair-form? false
       :param-nodes []
       :schema-nodes []})))

(defn- list-node? [node]
  (= :list (:tag node)))

(defn- throws-node? [node]
  (and (list-node? node)
       (let [children (:children node)]
         (and (seq children)
              (= 'throws (api/sexpr (first children)))))))

(defn- parse-method-arity
  [arity-children]
  (let [[arity-children throws-node]
        (if (and (<= 3 (count arity-children))
                 (throws-node? (last arity-children)))
          [(butlast arity-children) (last arity-children)]
          [arity-children nil])
        throws-vec-node (when throws-node
                          (second (:children throws-node)))
        valid-throws? (or (nil? throws-node)
                          (and (= 2 (count (:children throws-node)))
                               (vector-node? throws-vec-node)))]
    (when (and valid-throws?
               (= 2 (count arity-children))
               (vector-node? (first arity-children)))
      (let [[input-schemas-node output-schema-node] arity-children
            {:keys [pair-form? param-nodes schema-nodes]}
            (parse-input-schemas-node input-schemas-node)
            params-sexpr (mapv api/sexpr param-nodes)]
        (when (and pair-form?
                   (every? symbol? params-sexpr)
                   (not-any? #{'this} params-sexpr))
          (let [input-sexpr (mapv api/sexpr schema-nodes)
                throws-ref-nodes (when throws-vec-node
                                   (:children throws-vec-node))
                spec (cond-> {:params params-sexpr
                              :arguments-schema (when (seq input-sexpr)
                                                  (into [:cat] input-sexpr))
                              :return-schema (api/sexpr output-schema-node)}
                       (seq params-sexpr)
                       (assoc :param-schemas
                              (zipmap (mapv (fn [sym]
                                              (keyword (name sym)))
                                            params-sexpr)
                                      input-sexpr))

                       throws-node
                       (assoc :throws (mapv api/sexpr throws-ref-nodes)))]
            {:arity (count params-sexpr)
             :arglist-node (api/vector-node
                            (into [(api/token-node 'this)] param-nodes))
             :reference-nodes (concat schema-nodes
                                      [output-schema-node]
                                      throws-ref-nodes)
             :spec spec}))))))

(defn- spec->legacy-meta
  [spec]
  (cond-> {:malt/params (:params spec)
           :malt/arguments-schema (:arguments-schema spec)
           :malt/return-schema (:return-schema spec)}
    (:param-schemas spec)
    (assoc :malt/param-schemas (:param-schemas spec))

    (contains? spec :throws)
    (assoc :malt/throws (:throws spec))))

(defn- normalize-method [method-node]
  (let [[method-name & method-children] (:children method-node)
        [doc-node method-children] (if (and (seq method-children)
                                            (string? (api/sexpr (first method-children))))
                                     [(first method-children) (rest method-children)]
                                     [nil method-children])
        [attr-node method-children] (if (and (seq method-children)
                                             (map? (api/sexpr (first method-children))))
                                      [(first method-children) (rest method-children)]
                                      [nil method-children])
        multi-arity? (and (seq method-children)
                          (every? list-node? method-children))
        arity-children (if multi-arity?
                         (mapv :children method-children)
                         [method-children])
        arities (mapv parse-method-arity arity-children)
        duplicate-arity (when (every? some? arities)
                          (some (fn [[arity n]]
                                  (when (< 1 n)
                                    arity))
                                (frequencies (mapv :arity arities))))]
    (when duplicate-arity
      (api/reg-finding!
       (assoc (meta method-name)
              :message (str "Duplicate arity " duplicate-arity " for "
                            (api/sexpr method-name))
              :type :malt/duplicate-arity)))
    (if (and (seq arities) (every? some? arities))
      (let [specs (mapv :spec arities)
            method-meta (cond-> (merge (meta (api/sexpr method-name))
                                       {:malt/specs specs})
                          (= 1 (count specs)) (merge (spec->legacy-meta (first specs)))
                          doc-node (assoc :doc (api/sexpr doc-node))
                          attr-node (merge (api/sexpr attr-node)))
            method-name (with-meta method-name method-meta)
            doc+attr (cond-> []
                       doc-node (conj doc-node)
                       attr-node (conj attr-node))]
        {:node (api/list-node (concat [method-name]
                                      (mapv :arglist-node arities)
                                      doc+attr))
         :reference-nodes (mapcat :reference-nodes arities)})
      {:node method-node
       :reference-nodes []})))

(defn defprotocol [{:keys [node]}]
  (let [[_ name-node & rest-children] (:children node)
        name-sym (api/sexpr name-node)
        protocol-name (name name-sym)
        schema-var-sym (symbol (str "?" protocol-name))
        schema-def-node (api/list-node [(api/token-node 'def)
                                        (api/token-node schema-var-sym)
                                        (api/token-node nil)])
        [doc-node rest-children] (if (and (seq rest-children)
                                          (string?
                                           (api/sexpr (first rest-children))))
                                   [(first rest-children)
                                    (rest rest-children)]
                                   [nil rest-children])
        [attr-node rest-children] (if (and (seq rest-children)
                                           (map?
                                            (api/sexpr (first rest-children))))
                                    [(first rest-children)
                                     (rest rest-children)]
                                    [nil rest-children])
        name-meta (cond-> (or (meta (api/sexpr name-node)) {})
                    doc-node (assoc :doc (api/sexpr doc-node))
                    attr-node (merge (api/sexpr attr-node)))
        name-node (with-meta name-node name-meta)
        normalized-methods (mapv normalize-method rest-children)
        method-schema-nodes (mapcat :reference-nodes normalized-methods)
        methods (mapv :node normalized-methods)
        defprotocol-node (api/list-node (concat (cond-> [(api/token-node 'defprotocol)
                                                         name-node]
                                                  doc-node (conj doc-node)
                                                  attr-node (conj attr-node))
                                                methods))
        new-node (if (seq method-schema-nodes)
                   (let [bindings (->> method-schema-nodes
                                       (mapcat (fn [schema-node]
                                                 [(api/token-node '_) schema-node]))
                                       (vec))]
                     (api/list-node [(api/token-node 'let)
                                     (api/vector-node bindings)
                                     defprotocol-node]))
                   defprotocol-node)]
    {:node (api/list-node [(api/token-node 'do)

                           ;; The protocol node is placed first as lsp
                           ;; references have a priority.
                           (with-meta new-node (meta name-node))
                           (with-meta schema-def-node (meta name-node))

                           ;; Generate fake usage to prevent clojure-lsp from
                           ;; reporting unused-var warnings
                           (with-meta (api/token-node schema-var-sym) (meta name-node))])}))

(defn extend-type [{:keys [node]}]
  (let [[_ & rest-children] (:children node)]
    {:node (api/list-node (cons (api/token-node 'extend-type) rest-children))}))

(defn reify [{:keys [node]}]
  (let [[_ & rest-children] (:children node)]
    {:node (api/list-node (cons (api/token-node 'reify) rest-children))}))

(defn defrecord [{:keys [node]}]
  (let [[_ name-node & rest-children] (:children node)
        name-sym (api/sexpr name-node)
        record-name (name name-sym)
        schema-var-sym (symbol (str "?" record-name "Schema"))
        instance-var-sym (symbol (str "?" record-name))
        schema-def-node (api/list-node [(api/token-node 'def)
                                        (api/token-node schema-var-sym)
                                        (api/token-node nil)])
        instance-def-node (api/list-node [(api/token-node 'def)
                                          (api/token-node instance-var-sym)
                                          (api/token-node nil)])
        [doc-node rest-children] (if (and (seq rest-children)
                                          (string? (api/sexpr (first rest-children))))
                                   [(first rest-children)
                                    (rest rest-children)]
                                   [nil rest-children])
        [attr-node rest-children] (if (and (seq rest-children)
                                           (map? (api/sexpr (first rest-children))))
                                    [(first rest-children)
                                     (rest rest-children)]
                                    [nil rest-children])
        [fields-node & impls] rest-children
        field-children (when (vector-node? fields-node) (:children fields-node))
        pair-form? (and (vector-node? fields-node)
                        (even? (count field-children)))
        params (when pair-form? (vec (take-nth 2 field-children)))
        schemas (when pair-form? (vec (take-nth 2 (rest field-children))))
        fields-node (if pair-form?
                      (api/vector-node params)
                      fields-node)
        defrecord-node (api/list-node (concat (cond-> [(api/token-node 'defrecord)
                                                       name-node]
                                                doc-node (conj doc-node)
                                                attr-node (conj attr-node))
                                              [fields-node]
                                              impls))
        new-node (if pair-form?
                   (let [bindings (->> schemas
                                       (mapcat (fn [schema-node]
                                                 [(api/token-node '_) schema-node]))
                                       (vec))]
                     (api/list-node [(api/token-node 'let)
                                     (api/vector-node bindings)
                                     defrecord-node]))
                   defrecord-node)]
    {:node (api/list-node [(api/token-node 'do)

                           ;; The protocol node is placed first as lsp
                           ;; references have a priority.
                           (with-meta new-node (meta name-node))
                           (with-meta schema-def-node (meta name-node))
                           (with-meta instance-def-node (meta name-node))

                           ;; Generate fake usages to prevent clojure-lsp from
                           ;; reporting unused-var lint warnings
                           (with-meta (api/token-node instance-var-sym) (meta name-node))
                           (with-meta (api/token-node schema-var-sym) (meta name-node))])}))
