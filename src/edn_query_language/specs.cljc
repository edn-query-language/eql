(ns edn-query-language.specs
  (:require
    [clojure.spec.alpha :as s]
    [edn-query-language.core :as eql]))

; tag::specs[]
(s/def ::eql/property keyword?)
(s/def ::eql/special-property #{'*})
(s/def ::eql/ident-value any?)
(s/def ::eql/ident (s/tuple ::eql/property ::eql/ident-value))
(s/def ::eql/join-key (s/or :prop ::eql/property, :ident ::eql/ident, :param-exp ::eql/join-key-param-expr))
(s/def ::eql/join (s/map-of ::eql/join-key ::eql/join-query, :count 1, :conform-keys true))
(s/def ::eql/union (s/map-of ::eql/property ::eql/query, :min-count 1, :conform-keys true))
(s/def ::eql/recursion-depth nat-int?)
(s/def ::eql/recursion (s/or :depth ::eql/recursion-depth, :unbounded #{'...}))

(s/def ::eql/join-query
  (s/or :query ::eql/query
    :union ::eql/union
    :recursion ::eql/recursion))

(s/def ::eql/params
  map?)

(s/def ::eql/param-expr-key
  (s/or :prop ::eql/property
    :join ::eql/join
    :ident ::eql/ident))

(s/def ::eql/param-expr
  (s/and seq? (s/cat :expr ::eql/param-expr-key :params (s/? ::eql/params))))

(s/def ::eql/join-key-param-key (s/or :prop ::eql/property :ident ::eql/ident))

(s/def ::eql/join-key-param-expr
  (s/and seq? (s/cat :expr ::eql/join-key-param-key :params (s/? ::eql/params))))

(s/def ::eql/mutation-key symbol?)

(s/def ::eql/mutation-expr
  (s/and seq? (s/cat :mutate-key ::eql/mutation-key :params (s/? ::eql/params))))

(s/def ::eql/mutation-join
  (s/map-of ::eql/mutation-expr ::eql/query :count 1 :conform-keys true))

(s/def ::eql/mutation
  (s/or :mutation ::eql/mutation-expr
    :mutation-join ::eql/mutation-join))

(s/def ::eql/query-expr
  (s/or :prop ::eql/property
    :join ::eql/join
    :ident ::eql/ident
    :mutation ::eql/mutation
    :param-exp ::eql/param-expr
    :special ::eql/special-property))

(s/def ::eql/query
  (s/coll-of ::eql/query-expr :kind vector?))
; end::specs[]

;; ast specs

(s/def :edn-query-language.ast/query ::eql/join-query)
(s/def :edn-query-language.ast/key (s/or :prop ::eql/property :ident ::eql/ident :sym symbol?))
(s/def :edn-query-language.ast/dispatch-key (s/or :prop ::eql/property :sym symbol?))
(s/def :edn-query-language.ast/union-key ::eql/property)

(s/def :edn-query-language.ast/children
  (s/coll-of :edn-query-language.ast/node))

(s/def :edn-query-language.ast/root
  (s/and (s/keys :req-un [:edn-query-language.ast/type :edn-query-language.ast/children])
    #(= :root (:type %))
    (fn [x] (every? (comp #(contains? #{:prop :join :call nil} %) :type) (:children x)))))

(defmulti node-type :type)

(defmethod node-type nil [_]
  (s/keys :req-un [:edn-query-language.ast/key :edn-query-language.ast/dispatch-key]))

(defmethod node-type :prop [_]
  (s/keys :req-un [:edn-query-language.ast/type :edn-query-language.ast/key :edn-query-language.ast/dispatch-key]))

(defmethod node-type :join [_]
  (s/and (s/keys :req-un [:edn-query-language.ast/type :edn-query-language.ast/key :edn-query-language.ast/dispatch-key] :opt-un [:edn-query-language.ast/children :edn-query-language.ast/query])
    #(if (-> % :query first (= :recursion)) % (if (contains? % :children) % false))
    (fn [x] (every? (comp #(contains? #{:prop :join :union :call nil} %) :type) (:children x)))))

(defmethod node-type :union [_]
  (s/and (s/keys :req-un [:edn-query-language.ast/type :edn-query-language.ast/children] :opt-un [:edn-query-language.ast/query])
    #(every? (comp #{:union-entry} :type) (:children %))))

(defmethod node-type :union-entry [_]
  (s/and (s/keys :req-un [:edn-query-language.ast/type :edn-query-language.ast/union-key :edn-query-language.ast/children]
           :opt-un [:edn-query-language.ast/query])
    (fn [x] (every? (comp #(contains? #{:prop :join :call nil} %) :type) (:children x)))))

(defmethod node-type :call [_]
  (s/and (s/keys
           :req-un [:edn-query-language.ast/type :edn-query-language.ast/key :edn-query-language.ast/dispatch-key ::eql/params]
           :opt-un [:edn-query-language.ast/query :edn-query-language.ast/children])
    (fn [x] (every? (comp #(contains? #{:prop :join :call nil} %) :type) (:children x)))))

(defmethod node-type :root [_]
  (s/spec :edn-query-language.ast/root))

(s/def :edn-query-language.ast/type (set (keys (methods node-type))))
(s/def :edn-query-language.ast/node (s/multi-spec node-type :type))

(s/fdef query->ast
  :args (s/cat :query (s/nilable ::eql/query))
  :ret :edn-query-language.ast/root)

(s/fdef query->ast1
  :args (s/cat :query ::eql/query)
  :ret (s/nilable :edn-query-language.ast/node))

(s/fdef ast->query
  :args (s/cat :ast :edn-query-language.ast/node)
  :ret ::eql/query)

(s/fdef ident?
  :args (s/cat :x any?)
  :ret boolean?)

(s/fdef focus-subquery
  :args (s/cat :query ::eql/query :sub-query ::eql/query)
  :ret ::eql/query)

(s/fdef transduce-children
  :args (s/cat :xform fn? :node :edn-query-language.ast/node)
  :ret :edn-query-language.ast/node)

(s/fdef union-children?
  :args (s/cat :ast :edn-query-language.ast/node)
  :ret boolean?)

(s/fdef update-property-param
  :args (s/cat :x (s/or :property ::eql/property
                    :expr ::eql/param-expr)
          :f fn?
          :args (s/* any?))
  :ret ::eql/param-expr)

(s/fdef merge-asts
  :args (s/or
          :init (s/cat)
          :completion (s/cat :q :edn-query-language.ast/node)
          :step (s/cat :qa :edn-query-language.ast/node, :qb :edn-query-language.ast/node))
  :ret (s/nilable :edn-query-language.ast/node))

(s/fdef merge-queries
  :args (s/cat :qa (s/nilable ::eql/query), :qb (s/nilable ::eql/query))
  :ret (s/nilable ::eql/query))
