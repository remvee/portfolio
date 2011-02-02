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
chain is the current validation composition to build on to, attrs are
the attribute names to validate for, err the error symbol it may yield
and pred the function to preform the validation."
  [chain attrs err pred & args]
  (reduce (fn [chain attr]
            (fn [before after]
              (chain before (if (pred attr before after)
                              (merge-errors after {attr [err]})
                              after))))
          chain
          attrs))

(defn not-blank [chain & attrs]
  "Attribute may not be nil or empty string validation."
  (skel chain attrs :not-blank (fn [attr _ after]
                                 (or (= "" (attr after))
                                     (nil? (attr after))))))

(defn numeric [chain & attrs]
  "Attribute must be numeric validation."
  (skel chain attrs :numeric (fn [attr _ after]
                               (not (number? (attr after))))))

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
  (skel chain attrs {:less-than amount} (fn [attr _ after]
                                          (and (number? (attr after))
                                               (< amount (attr after))))))

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
  (skel chain attrs :unique (fn [attr before after]
                              (some #(= (attr after) (attr %))
                                    (filter (partial not= before)
                                            (collfn))))))

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
