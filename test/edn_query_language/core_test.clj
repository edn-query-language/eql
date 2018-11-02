(ns edn-query-language.core-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as s.test]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [edn-query-language.core :as eql]))

(s.test/instrument)

;; spec tests

(defn valid-queries-props []
  (props/for-all [query (eql/make-gen {} ::eql/gen-query)]
    (s/valid? ::eql/query query)))

(test/defspec generator-makes-valid-queries {:max-size 12 :num-tests 50} (valid-queries-props))

(comment
  (tc/quick-check 50 (valid-queries-props) :max-size 12))

;; lib tests

(defn remove-meta [x]
  (eql/transduce-children (map #(dissoc % :meta)) x))

(defn tquery->ast [query]
  (remove-meta (eql/query->ast query)))

(deftest test-query->ast
  (testing "empty query"
    (is (= (tquery->ast [])
           {:type :root, :children []})))

  (testing "single property"
    (is (= (tquery->ast [:a])
           {:type :root, :children [{:type :prop, :dispatch-key :a, :key :a}]})))

  (testing "multiple properties"
    (is (= (tquery->ast [:a :b])
           {:type     :root,
            :children [{:type :prop, :dispatch-key :a, :key :a}
                       {:type :prop, :dispatch-key :b, :key :b}]})))

  (testing "blank join"
    (is (= (tquery->ast [{:a []}])
           {:type     :root,
            :children [{:type :join, :dispatch-key :a, :key :a, :query [], :children []}]})))

  (testing "simple join"
    (is (= (tquery->ast [{:a [:b]}])
           {:type     :root,
            :children [{:type         :join,
                        :dispatch-key :a,
                        :key          :a,
                        :query        [:b],
                        :children     [{:type :prop, :dispatch-key :b, :key :b}]}]})))

  (testing "param expression"
    (is (= (tquery->ast ['(:a {:foo "bar"})])
           {:type     :root,
            :children [{:type         :prop,
                        :dispatch-key :a,
                        :key          :a,
                        :params       {:foo "bar"},}]})))

  (testing "param join"
    (is (= (tquery->ast ['({:a [:sub]} {:foo "bar"})])
           {:type     :root,
            :children [{:type         :join,
                        :dispatch-key :a,
                        :key          :a,
                        :query        [:sub],
                        :children     [{:type :prop, :dispatch-key :sub, :key :sub}],
                        :params       {:foo "bar"},}]})))

  (testing "param join 2"
    (is (= (tquery->ast [{'(:a {:foo "bar"}) [:sub]}])
           {:type     :root
            :children [{:children     [{:dispatch-key :sub
                                        :key          :sub
                                        :type         :prop}]
                        :dispatch-key :a
                        :key          :a
                        :params       {:foo "bar"}
                        :query        [:sub]
                        :type         :join}]})))

  (testing "union query"
    (is (= (tquery->ast [{:foo {:a [:b]
                                :c [:d]}}])
           {:type     :root,
            :children [{:type         :join,
                        :dispatch-key :foo,
                        :key          :foo,
                        :query        {:a [:b], :c [:d]},
                        :children     [{:type     :union,
                                        :query    {:a [:b], :c [:d]},
                                        :children [{:type      :union-entry,
                                                    :union-key :a,
                                                    :query     [:b],
                                                    :children  [{:type :prop, :dispatch-key :b, :key :b}]}
                                                   {:type      :union-entry,
                                                    :union-key :c,
                                                    :query     [:d],
                                                    :children  [{:type :prop, :dispatch-key :d, :key :d}]}]}]}]})))

  (testing "unbounded recursion"
    (is (= (tquery->ast '[{:item [:a :b {:parent ...}]}])
           '{:type     :root,
             :children [{:type         :join,
                         :dispatch-key :item,
                         :key          :item,
                         :query        [:a :b {:parent ...}],
                         :children     [{:type :prop, :dispatch-key :a, :key :a}
                                        {:type :prop, :dispatch-key :b, :key :b}
                                        {:type :join, :dispatch-key :parent, :key :parent, :query ...}]}]})))

  (testing "bounded recursion"
    (is (= (tquery->ast '[{:item [:a :b {:parent 5}]}])
           '{:type     :root,
             :children [{:type         :join,
                         :dispatch-key :item,
                         :key          :item,
                         :query        [:a :b {:parent 5}],
                         :children     [{:type :prop, :dispatch-key :a, :key :a}
                                        {:type :prop, :dispatch-key :b, :key :b}
                                        {:type :join, :dispatch-key :parent, :key :parent, :query 5}]}]})))

  (testing "mutation expression"
    (is (= (tquery->ast ['(a {})])
           '{:type     :root,
             :children [{:dispatch-key a,
                         :key          a,
                         :params       {},
                         :type         :call}]})))

  (testing "mutation join expression"
    (is (= (tquery->ast [{'(a {}) [:sub-query]}])
           '{:type     :root,
             :children [{:dispatch-key a,
                         :key          a,
                         :params       {},
                         :type         :call,
                         :query        [:sub-query],
                         :children     [{:type :prop, :dispatch-key :sub-query, :key :sub-query}]}]}))))

(defn query<->ast-props []
  (props/for-all [query (eql/make-gen {::eql/gen-params
                                       (fn [_]
                                         (gen/map gen/keyword gen/string-alphanumeric))}
                          ::eql/gen-query)]
    (let [ast (-> query
                  eql/query->ast
                  eql/ast->query
                  eql/query->ast)]
      (= ast (-> ast
                 eql/ast->query
                 eql/query->ast)))))

(test/defspec query-ast-roundtrip {:max-size 12 :num-tests 100} (query<->ast-props))

(comment
  (tc/quick-check 100 (query<->ast-props) :max-size 12))

(deftest test-focus-subquery
  (is (= (eql/focus-subquery [] [])
         []))
  (is (= (eql/focus-subquery [:a :b :c] [])
         []))
  (is (= (eql/focus-subquery [:a :b :c] [:d])
         []))
  (is (= (eql/focus-subquery [:a :b :c] [:a])
         [:a]))
  (is (= (eql/focus-subquery [:a :b :c] [:a :b])
         [:a :b]))
  (is (= (eql/focus-subquery [:a {:b [:d]}] [:a :b])
         [:a {:b [:d]}]))
  (is (= (eql/focus-subquery [:a {:b [:c :d]}] [:a {:b [:c]}])
         [:a {:b [:c]}]))
  (is (= (eql/focus-subquery [:a '({:b [:c :d]} {:param "value"})] [:a {:b [:c]}])
         [:a '({:b [:c]} {:param "value"})]))

  ; in union case, keys absent from focus will be pulled anyway, given ones will focus
  (is (= (eql/focus-subquery [:a {:b {:c [:d :e]
                                       :f [:g :h]}}]
           [:a {:b {:f [:g]}}])
         [:a {:b {:c [:d :e] :f [:g]}}])))

(defn transduce-query [xform query]
  (->> query eql/query->ast
       (eql/transduce-children xform)
       eql/ast->query))

(deftest test-tranduce-children
  (is (= (transduce-query
           (comp (filter (comp #{:a :c} :key))
                 (map #(assoc % :params {:n 42})))
           [:a :b :c :d])
         '[(:a {:n 42}) (:c {:n 42})])))

(deftest test-merge-queries
  (is (= (eql/merge-queries nil nil)
         []))

  (is (= (eql/merge-queries [:a] nil)
         [:a]))

  (is (= (eql/merge-queries [] [])
         []))

  (is (= (eql/merge-queries [:a] [])
         [:a]))

  (is (= (eql/merge-queries [:a] [:a])
         [:a]))

  (is (= (eql/merge-queries [:a] [:b])
         [:a :b]))

  (is (= (eql/merge-queries [:a] [:b :c :d])
         [:a :b :c :d]))

  (is (= (eql/merge-queries [[:u/id 1]] [[:u/id 2]])
         [[:u/id 1] [:u/id 2]]))

  (is (= (eql/merge-queries [{:user [:name]}] [{:user [:email]}])
         [{:user [:name :email]}]))

  (is (= (eql/merge-queries [:a] [{:a [:x]}])
         [{:a [:x]}]))

  (is (= (eql/merge-queries [{:a [:x]}] [:a])
         [{:a [:x]}]))

  (testing "don't merge queries with different params"
    (is (= (eql/merge-queries ['({:user [:name]} {:login "u1"})]
             ['({:user [:email]} {:login "u2"})])
           nil)))

  (testing "don't merge queries with different params"
    (is (= (eql/merge-queries ['(:user {:login "u1"})]
             ['(:user {:login "u2"})])
           nil)))

  (testing "merge when params are same"
    (is (= (eql/merge-queries ['({:user [:name]} {:login "u1"})]
             ['({:user [:email]} {:login "u1"})])
           ['({:user [:name :email]} {:login "u1"})])))

  (testing "calls can't be merged when same name occurs"
    (is (= (eql/merge-queries ['(hello {:login "u1"})]
             ['(hello {:bla "2"})])
           nil)))

  (testing "even when parameters are the same"
    (is (= (eql/merge-queries ['(hello {:login "u1"})]
             ['(hello {:login "u1"})])
           nil))))
