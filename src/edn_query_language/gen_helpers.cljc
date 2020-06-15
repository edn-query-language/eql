(ns edn-query-language.gen-helpers
  (:refer-clojure :exclude [let])
  (:require
    [clojure.spec.gen.alpha :as gen])
  #?(:cljs (:require-macros [edn-query-language.gen-helpers :refer [lazy-combinator let]])))

(alias 'c 'clojure.core)

#?(:clj
   (def dynaload #'gen/dynaload))

#?(:clj
   (defmacro ^:skip-wiki lazy-combinator
     "Implementation macro, do not call directly."
     [s]
     (c/let [fqn (symbol "clojure.test.check.generators" (name s))
             doc (str "Lazy loaded version of " fqn)]
       `(c/let [g# (delay (dynaload '~fqn))]
          (defn ~s
            ~doc
            [& ~'args]
            (apply @g# ~'args))))))

(def generator? #'gen/generator?)

#?(:cljs (cljs.spec.gen.alpha/lazy-combinator vector-distinct-by)
   :clj  (lazy-combinator vector-distinct-by))

#?(:clj
   (defmacro let
     "Macro for building generators using values from other generators.
     Uses a binding vector with the same syntax as clojure.core/let,
     where the right-hand side of the binding pairs are generators, and
     the left-hand side are names (or destructuring forms) for generated
     values.

     Subsequent generator expressions can refer to the previously bound
     values, in the same way as clojure.core/let.

     The body of the let can be either a value or a generator, and does
     the expected thing in either case. In this way let provides the
     functionality of both `bind` and `fmap`.

     Examples:

       (gen/let [strs (gen/not-empty (gen/list gen/string))
                 s (gen/elements strs)]
         {:some-strings strs
          :one-of-those-strings s})

       ;; generates collections of \"users\" that have integer IDs
       ;; from 0...N-1, but are in a random order
       (gen/let [users (gen/list (gen/hash-map :name gen/string-ascii
                                               :age gen/nat))]
         (->> users
              (map #(assoc %2 :id %1) (range))
              (gen/shuffle)))"
     {:added "0.9.0"}
     [bindings & body]
     (assert (vector? bindings)
             "First arg to gen/let must be a vector of bindings.")
     (assert (even? (count bindings))
             "gen/let requires an even number of forms in binding vector")
     (if (empty? bindings)
       `(c/let [val# (do ~@body)]
          (if (generator? val#)
            val#
            (gen/return val#)))
       (c/let [[binding gen & more] bindings]
         `(gen/bind ~gen (fn [~binding] (let [~@more] ~@body)))))))