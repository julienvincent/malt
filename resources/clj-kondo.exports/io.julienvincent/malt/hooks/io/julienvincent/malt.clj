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

(defn- normalize-method [method-node]
  (let [[method-name & rest-children] (:children method-node)
        [doc-node rest-children] (if (and (seq rest-children)
                                          (string? (api/sexpr (first rest-children))))
                                   [(first rest-children) (rest rest-children)]
                                   [nil rest-children])
        [attr-node rest-children] (if (and (seq rest-children)
                                           (map? (api/sexpr (first rest-children))))
                                    [(first rest-children) (rest rest-children)]
                                    [nil rest-children])
        schema-form? (and (= 2 (count rest-children))
                          (= 1 (count (filter vector-node? rest-children))))
        method-children (if schema-form?
                          (let [[input-schemas-node output-schema-node] rest-children
                                {:keys [pair-form? param-nodes schema-nodes]} (parse-input-schemas-node
                                                                               input-schemas-node)]
                            (if pair-form?
                              (let [params-sexpr (mapv api/sexpr param-nodes)
                                    input-sexpr (mapv api/sexpr schema-nodes)
                                    schema-map-sexpr (zipmap (mapv (fn [sym]
                                                                     (keyword (name sym)))
                                                                   params-sexpr)
                                                             input-sexpr)
                                    args-schema-sexpr (when (seq input-sexpr)
                                                        (into [:cat] input-sexpr))
                                    output-sexpr (api/sexpr output-schema-node)
                                    arity (count schema-nodes)
                                    method-meta (cond-> (merge (meta (api/sexpr method-name))
                                                               (cond-> {:malt/params params-sexpr
                                                                        :malt/arguments-schema args-schema-sexpr
                                                                        :malt/return-schema output-sexpr}
                                                                 (seq params-sexpr)
                                                                 (assoc :malt/param-schemas schema-map-sexpr)))
                                                  doc-node (assoc :doc (api/sexpr doc-node))
                                                  attr-node (merge (api/sexpr attr-node)))
                                    method-name (with-meta method-name method-meta)
                                    arg-vector (api/vector-node (into [(api/token-node 'this)]
                                                                      (map (fn [node]
                                                                             (api/token-node (api/sexpr node)))
                                                                           param-nodes)))
                                    doc+attr (cond-> []
                                               doc-node (conj doc-node)
                                               attr-node (conj attr-node))]
                                (into [method-name arg-vector] doc+attr))
                              (:children method-node)))
                          (:children method-node))]
    (api/list-node method-children)))

(defn defprotocol [{:keys [node]}]
  (let [[_ name-node & rest-children] (:children node)
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
        method-schema-nodes (->> rest-children
                                 (mapcat (fn [method-node]
                                           (let [[_method-name & method-rest] (:children method-node)
                                                 [_doc method-rest] (if (and (seq method-rest)
                                                                             (string? (api/sexpr (first method-rest))))
                                                                      [(first method-rest) (rest method-rest)]
                                                                      [nil method-rest])
                                                 [_attr method-rest] (if (and (seq method-rest)
                                                                              (map? (api/sexpr (first method-rest))))
                                                                       [(first method-rest) (rest method-rest)]
                                                                       [nil method-rest])]
                                             (when (= 2 (count method-rest))
                                               (let [[input-schemas-node output-schema-node] method-rest]
                                                 (when (vector-node? input-schemas-node)
                                                   (let [{:keys [pair-form? schema-nodes]} (parse-input-schemas-node
                                                                                            input-schemas-node)]
                                                     (when pair-form?
                                                       (concat schema-nodes [output-schema-node])))))))))
                                 (remove nil?))
        methods (mapv normalize-method rest-children)
        defprotocol-node (api/list-node (list* (api/token-node 'defprotocol)
                                               name-node
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
    {:node new-node}))

(defn extend [{:keys [node]}]
  (let [[_ & rest-children] (:children node)]
    {:node (api/list-node (cons (api/token-node 'extend-type) rest-children))}))

(defn implement [{:keys [node]}]
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
                           schema-def-node
                           instance-def-node
                           ;; Generate fake usages to prevent clojure-lsp from
                           ;; reporting unused-var lint warnings
                           (with-meta (api/token-node instance-var-sym) (meta node))
                           (with-meta (api/token-node schema-var-sym) (meta node))
                           new-node])}))
