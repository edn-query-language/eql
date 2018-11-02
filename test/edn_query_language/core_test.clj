(ns edn-query-language.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [edn-query-language.core :as eql]))

(deftest test-query->ast
  (testing "empty query"
    (is (= (eql/query->ast [])
           {:type :root, :children []})))

  (testing "single property"
    (is (= (eql/query->ast [:a])
           {:type :root, :children [{:type :prop, :dispatch-key :a, :key :a}]})))

  (testing "multiple properties"
    (is (= (eql/query->ast [:a :b])
           {:type     :root,
            :children [{:type :prop, :dispatch-key :a, :key :a}
                       {:type :prop, :dispatch-key :b, :key :b}]})))

  (testing "blank join"
    (is (= (eql/query->ast [{:a []}])
           {:type     :root,
            :children [{:type :join, :dispatch-key :a, :key :a, :query [], :children []}]})))

  (testing "simple join"
    (is (= (eql/query->ast [{:a [:b]}])
           {:type     :root,
            :children [{:type         :join,
                        :dispatch-key :a,
                        :key          :a,
                        :query        [:b],
                        :children     [{:type :prop, :dispatch-key :b, :key :b}]}]})))

  (testing "param expression"
    (is (= (eql/query->ast ['(:a {:foo "bar"})])
           {:type     :root,
            :children [{:type         :prop,
                        :dispatch-key :a,
                        :key          :a,
                        :params       {:foo "bar"},
                        :meta         {:line 35, :column 30}}]})))

  (testing "param join"
    (is (= (eql/query->ast ['({:a [:sub]} {:foo "bar"})])
           {:type     :root,
            :children [{:type         :join,
                        :dispatch-key :a,
                        :key          :a,
                        :query        [:sub],
                        :children     [{:type :prop, :dispatch-key :sub, :key :sub}],
                        :params       {:foo "bar"},
                        :meta         {:line 44, :column 30}}]})))

  (testing "param join 2"
    (is (= (eql/query->ast [{'(:a {:foo "bar"}) [:sub]}])
           {:type     :root
            :children [{:children     [{:dispatch-key :sub
                                        :key          :sub
                                        :type         :prop}]
                        :dispatch-key :a
                        :key          :a
                        :meta         {:column 31
                                       :line   55}
                        :params       {:foo "bar"}
                        :query        [:sub]
                        :type         :join}]})))

  (testing "union query"
    (is (= (eql/query->ast [{:foo {:a [:b]
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
    (is (= (eql/query->ast '[{:item [:a :b {:parent ...}]}])
           '{:type     :root,
             :children [{:type         :join,
                         :dispatch-key :item,
                         :key          :item,
                         :query        [:a :b {:parent ...}],
                         :children     [{:type :prop, :dispatch-key :a, :key :a}
                                        {:type :prop, :dispatch-key :b, :key :b}
                                        {:type :join, :dispatch-key :parent, :key :parent, :query ...}]}]})))

  (testing "bounded recursion"
    (is (= (eql/query->ast '[{:item [:a :b {:parent 5}]}])
           '{:type     :root,
             :children [{:type         :join,
                         :dispatch-key :item,
                         :key          :item,
                         :query        [:a :b {:parent 5}],
                         :children     [{:type :prop, :dispatch-key :a, :key :a}
                                        {:type :prop, :dispatch-key :b, :key :b}
                                        {:type :join, :dispatch-key :parent, :key :parent, :query 5}]}]})))

  (testing "mutation expression"
    (is (= (eql/query->ast ['(a {})])
           '{:type     :root,
             :children [{:dispatch-key a,
                         :key          a,
                         :params       {},
                         :meta         {:line 110, :column 30},
                         :type         :call}]})))

  (testing "mutation join expression"
    (is (= (eql/query->ast [{'(a {}) [:sub-query]}])
           '{:type     :root,
             :children [{:dispatch-key a,
                         :key          a,
                         :params       {},
                         :meta         {:line 119, :column 31},
                         :type         :call,
                         :query        [:sub-query],
                         :children     [{:type :prop, :dispatch-key :sub-query, :key :sub-query}]}]}))))
