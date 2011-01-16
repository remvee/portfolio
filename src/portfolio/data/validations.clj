;; Copyright (c) Remco van 't Veer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.  You must not remove this notice, or any other, from
;; this software.

(ns #^{:author "Remco van 't Veer"
       :doc "Basic validation framework for maps."}
  portfolio.data.validations
  (:use [clojure.test]))

(defn- merge-errors
  {:test #(are [expected map errors] (= expected
                                        (:errors (meta (merge-errors map errors))))
               {:bar [:quux]}
               {:foo 1} {:bar [:quux]}

               {:bar [:flarp :quux]}
               (with-meta {:foo 1} {:errors {:bar [:flarp]}}) {:bar [:quux]})}
  [map errors]
  (if (empty? errors)
    map
    (with-meta map {:errors (merge-with concat
                                        (:errors (meta map))
                                        errors)})))

(defn terminator
  "Stub validation for chaining."
  [before after]
  [before after])

(defmacro validator
  "Construct a validator function by composing validation functions.
Example: (validator (not-blank :name :nr) (numeric :nr) (less-than 10 :nr))"
  [& validators]
  `(fn [before# after#]
     (last ((-> terminator ~@validators) before# after#))))

(defn skel
  "Skeleton validation function to build validations on top of.  The
chain is the current validation composition to build on to, attr is
the attribute name to validate for, err the error symbol it may yield
and pred the function to preform the validation."
  [chain attr err pred & args]
  (fn [before after]
    (chain before (if (apply pred before after args)
                    (merge-errors after {attr [err]})
                    after))))

(defn not-blank [chain & attrs]
  "Attribute may not be nil or empty string validation."
  (reduce (fn [chain attr]
            (skel chain attr :not-blank (fn [_ after]
                                          (or (= "" (attr after))
                                              (nil? (attr after))))))
          chain attrs))

(defn numeric [chain & attrs]
  "Attribute must be numeric validation."
  (reduce (fn [chain attr]
            (skel chain attr :numeric (fn [_ after]
                                        (not (number? (attr after))))))
          chain attrs))

(defn less-than
  "Attribute must be less then validation."
  {:test #(are [expected attrs amount]
               (= expected
                  (:errors (meta ((validator (less-than amount :count))
                                  nil attrs))))
               
               nil nil nil
               nil {:count 1} 2
               {:count [{:less-than 2}]} {:count 3} 2)}
  [chain amount & attrs]
  (reduce (fn [chain attr]
            (skel chain attr {:less-than amount} (fn [_ after]
                                                   (and (number? (attr after))
                                                        (< amount (attr after))))))
          chain attrs))

(defn unique
  "Attribute must be uniq to collection returned by collfn."
  {:test #(are [expected before after coll]
               (= expected
                  (:errors (meta ((validator (unique (fn [] coll) :id))
                                  before after))))
               
               nil nil {} []
               nil nil {:id 1} []
               {:id [:unique]} nil {:id 1} [{:id 1}]
               nil {:id 1} {:id 1 :foo 1} [{:id 1}]
               {:id [:unique]} {:id 1 :bar 1} {:id 1 :foo 1} [{:id 1}]
               nil nil {:id 2} [{:id 1}])}
  [chain collfn & attrs]
  (reduce (fn [chain attr]
            (skel chain attr :unique (fn [before after]
                                       (some #(= (attr after) (attr %))
                                             (filter (partial not= before)
                                                     (collfn))))))
          chain attrs))

(deftest example
  (are [expected input]
       (= expected
          (:errors (meta ((validator (not-blank :name :nr)
                                     (numeric :nr)
                                     (less-than 10 :count))
                          {} input))))

       nil
       {:nr 1, :name "test", :count 5}
       
       {:nr [:numeric :not-blank]
        :name [:not-blank]}
       {:foo "bar"}
       
       {:name [:not-blank]
        :nr [:numeric]}
       {:nr "foo"}

       {:count [{:less-than 10}]}
       {:nr 1, :name "test", :count 11}))
