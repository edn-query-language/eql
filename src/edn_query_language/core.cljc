(ns edn-query-language.core
  (:refer-clojure :exclude [ident?])
  (:require [clojure.spec.alpha :as s]
            [edn-query-language.spec.ast :as spec.ast]
            [edn-query-language.spec.query :as spec.query]))

(declare expr->ast)

(defn- mark-meta [source target]
  (cond-> target
    (meta source) (assoc :meta (meta source))))

(defn symbol->ast [k]
  {:dispatch-key k
   :key          k})

(defn keyword->ast [k]
  {:type         :prop
   :dispatch-key k
   :key          k})

(defn union-entry->ast [[k v]]
  (let [component (-> v meta :component)]
    (merge
      {:type      :union-entry
       :union-key k
       :query     v
       :children  (into [] (map expr->ast) v)}
      (when-not (nil? component)
        {:component component}))))

(defn union->ast [m]
  {:type     :union
   :query    m
   :children (into [] (map union-entry->ast) m)})

(defn call->ast [[f args :as call]]
  (if (= 'quote f)
    (assoc (expr->ast args) :target (or (-> call meta :target) :remote))
    (let [ast (update-in (expr->ast f) [:params] merge (or args {}))]
      (cond-> (mark-meta call ast)
        (symbol? (:dispatch-key ast)) (assoc :type :call)))))

(defn query->ast
  "Convert a query to its AST representation."
  [query]
  (let [component (-> query meta :component)]
    (merge
      (mark-meta query
        {:type     :root
         :children (into [] (map expr->ast) query)})
      (when-not (nil? component)
        {:component component}))))

(s/fdef query->ast
  :args (s/cat :query (s/nilable ::spec.query/query))
  :ret ::spec.ast/root)

(defn query->ast1
  "Call query->ast and return the first children."
  [query-expr]
  (-> (query->ast query-expr) :children first))

(s/fdef query->ast1
  :args (s/cat :query ::spec.query/query)
  :ret ::spec.ast/root)

(defn join->ast [join]
  (let [query-root? (-> join meta :query-root)
        [k v] (first join)
        ast         (expr->ast k)
        type        (if (= :call (:type ast)) :call :join)
        component   (-> v meta :component)]
    (merge ast
           (mark-meta join {:type type :query v})
           (when-not (nil? component)
             {:component component})
           (when query-root?
             {:query-root true})
           (when-not (or (number? v) (= '... v))
             (cond
               (vector? v) {:children (into [] (map expr->ast) v)}
               (map? v) {:children [(union->ast v)]}
               :else (throw
                       (ex-info (str "Invalid join, " join)
                         {:type :error/invalid-join})))))))

(defn ident->ast [[k id :as ref]]
  {:type         :prop
   :dispatch-key k
   :key          ref})

(defn expr->ast
  "Given a query expression convert it into an AST."
  [x]
  (cond
    (symbol? x) (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x) (join->ast x)
    (vector? x) (ident->ast x)
    (seq? x) (call->ast x)
    :else (throw
            (ex-info (str "Invalid expression " x)
              {:type :error/invalid-expression}))))

(defn wrap-expr [root? expr]
  (if root?
    (with-meta
      (cond-> expr (keyword? expr) list)
      {:query-root true})
    expr))

(defn parameterize [expr params]
  (if-not (empty? params)
    (list expr params)
    (list expr)))

(defn ast->expr
  "Given a query expression AST convert it back into a query expression."
  ([ast]
   (ast->expr ast false))
  ([{:keys [type component] ast-meta :meta :as ast} unparse?]
   (if (= :root type)
     (cond-> (into (with-meta [] ast-meta) (map #(ast->expr % unparse?)) (:children ast))
       (not (nil? component)) (vary-meta assoc :component component))
     (let [{:keys [key query query-root params]} ast]
       (wrap-expr query-root
         (if (and params (not= :call type))
           (let [expr (ast->expr (dissoc ast :params) unparse?)]
             (parameterize expr params))
           (let [key (if (= :call type) (parameterize key params) key)]
             (if (or (= :join type)
                     (and (= :call type) (:children ast)))
               (if (and (not= '... query) (not (number? query))
                        (or (true? unparse?)
                            (= :call type)))
                 (let [{:keys [children]} ast
                       query-meta (meta query)]
                   (if (and (== 1 (count children))
                            (= :union (:type (first children)))) ;; UNION
                     (with-meta
                       {key (into (cond-> (with-meta {} ast-meta)
                                    component (vary-meta assoc :component component))
                                  (map (fn [{:keys [union-key children component]}]
                                         [union-key
                                          (cond-> (into [] (map #(ast->expr % unparse?)) children)
                                            (not (nil? component)) (vary-meta assoc :component component))]))
                                  (:children (first children)))}
                       ast-meta)
                     (with-meta
                       {key (cond-> (into (with-meta [] query-meta) (map #(ast->expr % unparse?)) children)
                              (not (nil? component)) (vary-meta assoc :component component))}
                       ast-meta)))
                 (with-meta {key query} ast-meta))
               key))))))))

(defn ast->query [query-ast]
  "Given an AST convert it back into a query expression."
  (ast->expr query-ast true))

(s/fdef ast->query
  :args (s/cat :ast ::spec.ast/node)
  :ret ::spec.ast/root)

(defn ident?
  "Check if x is a EQL ident."
  [x]
  (and (vector? x)
       (keyword? (first x))
       (= 2 (count x))))

(s/fdef ident?
  :args (s/cat :x any?)
  :ret boolean?)

;; query processing helpers

(declare focus-subquery*)

(defn- focus-subquery-union*
  [query-ast sub-ast]
  (let [s-index (into {} (map #(vector (:union-key %) %)) (:children sub-ast))]
    (assoc query-ast
      :children
      (reduce
        (fn [children {:keys [union-key] :as union-entry}]
          (if-let [sub (get s-index union-key)]
            (conj children (focus-subquery* union-entry sub))
            (conj children union-entry)))
        []
        (:children query-ast)))))

(defn- focus-subquery*
  [query-ast sub-ast]
  (let [q-index (into {} (map #(vector (:key %) %)) (:children query-ast))]
    (assoc query-ast
      :children
      (reduce
        (fn [children {:keys [key type] :as focus}]
          (if-let [source (get q-index key)]
            (cond
              (= :join type (:type source))
              (conj children (focus-subquery* source focus))

              (= :union type (:type source))
              (conj children (focus-subquery-union* source focus))

              :else
              (conj children source))
            children))
        []
        (:children sub-ast)))))

(defn focus-subquery
  "Given a query, focus it along the specified query expression.

  Examples:
    (focus-query [:foo :bar :baz] [:foo])
    => [:foo]

    (fulcro.client.primitives/focus-query [{:foo [:bar :baz]} :woz] [{:foo [:bar]} :woz])
    => [{:foo [:bar]} :woz]"
  [query sub-query]
  (let [query-ast (query->ast query)
        sub-ast   (query->ast sub-query)]
    (ast->expr (focus-subquery* query-ast sub-ast) true)))

(s/fdef focus-subquery
  :args (s/cat :query ::spec.query/query :sub-query ::spec.query/query)
  :ret ::spec.query/query)

(defn transduce-children
  "Recursivelly transduce children on the AST, you can use this to apply filter/transformations
  on a whole AST. Each iteration of the transducer will get a single AST node to process.

  ```
  (->> [:a {:b [:c :d]} :e]
       (p/query->ast)
       (p/transduce-children (remove (comp #{:a :c} :key)))
       (p/ast->query))
  ; => [{:b [:d]} :e]
  ```"
  [xform {:keys [children] :as node}]
  (cond-> node
    (seq children)
    (update :children
      (fn [children]
        (into [] (comp xform (map #(transduce-children xform %))) children)))))

(s/fdef transduce-children
  :args (s/cat :xform fn? :node ::spec.ast/node)
  :ret ::spec.ast/node)

(defn union-children?
  "Given an AST point, check if the children is a union query type."
  [ast]
  (= :union (some-> ast :children first :type)))

(s/fdef union-children?
  :args (s/cat :ast ::spec.ast/node)
  :ret boolean?)

(defn update-property-param
  "Add property param, eg:

  ```
  (p/update-property-param :keyword assoc :foo \"bar\") => (:keyword {:foo \"bar\"})
  (p/update-property-param '(:keyword {:param \"prev\"}) assoc :foo \"bar\") => (:keyword {:foo \"bar\" :param \"prev\"})
  ```
  "
  [x f & args]
  (if (seq? x)
    (let [[k p] x]
      (list k (apply f p args)))

    (list x (apply f {} args))))

(s/fdef update-property-param
  :args (s/cat :x (s/or :property ::spec.query/property
                        :expr ::spec.query/param-expr)
               :f fn?
               :args (s/* any?))
  :ret ::spec.query/param-expr)

(defn merge-asts
  "Merges two ast's."
  [qa qb]
  (reduce (fn [ast {:keys [key type params] :as item-b}]
            (if-let [[idx item] (->> ast :children
                                     (keep-indexed #(if (-> %2 :key (= key)) [%1 %2]))
                                     first)]
              (cond
                (or (= :join (:type item) type)
                    (= :prop (:type item) type))
                (if (= (:params item) params)
                  (update-in ast [:children idx] merge-asts item-b)
                  (reduced nil))

                (and (= :prop (:type item))
                     (= :join type))
                (assoc-in ast [:children idx] item-b)

                (= :call type)
                (reduced nil)

                :else ast)
              (update ast :children conj item-b)))
    qa
    (:children qb)))

(s/fdef merge-asts
  :args (s/cat :qa ::spec.ast/node, :qb ::spec.ast/node)
  :ret (s/nilable ::spec.ast/node))

(defn merge-queries
  "Merges two queries"
  [qa qb]
  (some-> (merge-asts (query->ast qa) (query->ast qb))
    (ast->query)))

(s/fdef merge-queries
  :args (s/cat :qa ::spec.query/query, :qb ::spec.query/query)
  :ret (s/nilable ::spec.query/query))
