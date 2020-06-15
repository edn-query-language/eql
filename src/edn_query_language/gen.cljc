(ns edn-query-language.gen
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [edn-query-language.gen-helpers :as genh]
    [edn-query-language.core :as eql]))

(when eql/INCLUDE_SPECS
  ;; query specs

  (def generators
    {::gen-max-depth
     4

     ::gen-property
     (fn gen-property [_] (gen/keyword-ns))

     ::gen-special-property
     (fn gen-special-property [_] (gen/return '*))

     ::gen-ident-key
     (fn gen-ident-key [_] (gen/keyword-ns))

     ::gen-ident-value
     (fn gen-ident-value [_]
       (gen/frequency [[15 (gen/simple-type-printable)]
                       [1 (gen/return '_)]]))

     ::gen-ident
     (fn gen-ident [{::keys [gen-ident-key gen-ident-value] :as env}]
       (gen/tuple
         (gen-ident-key env)
         (gen-ident-value env)))

     ::gen-params
     (fn gen-params [_] (gen/map (gen/any-printable) (gen/any-printable)))

     ::gen-join-key
     (fn gen-join-key [{::keys [gen-property gen-ident gen-join-key-param-expr] :as env}]
       (gen/frequency [[10 (gen-property env)]
                       [3 (gen-ident env)]
                       [1 (gen-join-key-param-expr env)]]))

     ::gen-join-key-param-key
     (fn gen-join-key-param-key [{::keys [gen-property gen-ident] :as env}]
       (gen/one-of [(gen-property env) (gen-ident env)]))

     ::gen-join-key-param-expr
     (fn gen-join-key-param-expr [{::keys [gen-join-key-param-key gen-params] :as env}]
       (genh/let [q (gen-join-key-param-key env)
                  p (gen-params env)]
         (list q p)))

     ::gen-join
     (fn gen-join [{::keys [gen-join-key gen-join-query] :as env}]
       (gen/map (gen-join-key env) (gen-join-query env) {:num-elements 1}))

     ::gen-join-query
     (fn gen-join-query [{::keys [gen-query gen-union gen-recursion] :as env}]
       (gen/frequency [[10 (gen-query env)]
                       [2 (gen-union env)]
                       [1 (gen-recursion env)]]))

     ::gen-union-key
     (fn gen-union-key [_] (gen/keyword-ns))

     ::gen-union
     (fn gen-union [{::keys [gen-union-key gen-query] :as env}]
       (gen/map (gen-union-key env) (gen-query env) {:min-elements 1}))

     ::gen-depth
     (fn gen-depth [_] (gen/large-integer* {:min 1 :max 5}))

     ::gen-recursion
     (fn gen-recursion [{::keys [gen-depth] :as env}]
       (gen/one-of [(gen-depth env) (gen/return '...)]))

     ::gen-param-expr-key
     (fn gen-param-expr-key [{::keys [gen-property gen-join gen-ident] :as env}]
       (gen/frequency [[20 (gen-property env)]
                       [8 (gen-join env)]
                       [4 (gen-ident env)]]))

     ::gen-param-expr
     (fn gen-param-expr [{::keys [gen-param-expr-key gen-params] :as env}]
       (genh/let [q (gen-param-expr-key env)
                  p (gen-params env)]
         (list q p)))

     ::gen-query-expr
     (fn gen-query-expr [{::keys [gen-property gen-join gen-ident gen-param-expr gen-special-property gen-mutation]
                          :as    env}]
       (gen/frequency [[20 (gen-property env)]
                       [6 (gen-join env)]
                       [1 (gen-ident env)]
                       [2 (gen-param-expr env)]
                       [1 (gen-mutation env)]
                       [1 (gen-special-property env)]]))

     ::gen-query
     (fn gen-query [{::keys [gen-property gen-query-expr gen-max-depth] :as env}]
       (if (> gen-max-depth 0)
         (gen/vector (gen-query-expr (update env ::gen-max-depth dec)))
         (gen/vector-distinct (gen-property env))))

     ::gen-mutation-key
     (fn gen-mutation-key [_] (gen/symbol))

     ::gen-mutation-expr
     (fn gen-mutation-expr [{::keys [gen-mutation-key gen-params] :as env}]
       (genh/let [key (gen-mutation-key env)
                  val (gen-params env)]
         (list key val)))

     ::gen-mutation-join
     (fn mutation-join [{::keys [gen-mutation-expr gen-query] :as env}]
       (gen/map (gen-mutation-expr env) (gen-query env) {:num-elements 1}))

     ::gen-mutation
     (fn gen-mutation [{::keys [gen-mutation-expr gen-mutation-join] :as env}]
       (gen/frequency [[5 (gen-mutation-expr env)]
                       [1 (gen-mutation-join env)]]))})

  (defn default-gen [name]
    #((get generators name) generators))

  (defn make-gen
    [env name]
    (let [env (merge generators env)
          gen (get env name)]
      (assert gen (str "No generator available for " name))
      ((get env name) env))))
