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

  (testing "call expression"
    (is (= (eql/query->ast ['(a {})])
           '{:type     :root,
             :children [{:dispatch-key a,
                         :key          a,
                         :params       {},
                         :meta         {:line 35, :column 30},
                         :type         :call}]}))))
