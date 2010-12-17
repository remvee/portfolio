(ns portfolio.data.validations
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

(defn terminator [before after] [before after])

(defmacro validator [& validators]
  `(fn [before# after#]
     (last ((-> terminator ~@validators) before# after#))))

(defn skel [chain attr err pred & args]
  (fn [before after]
    (chain before (if (apply pred before after args)
                    (merge-errors after {attr [err]})
                    after))))

(defn not-blank [chain & attrs]
  (reduce (fn [chain attr]
            (skel chain attr :not-blank (fn [_ after]
                                          (or (= "" (attr after))
                                              (nil? (attr after))))))
          chain attrs))

(defn numeric [chain & attrs]
  (reduce (fn [chain attr]
            (skel chain attr :numeric (fn [_ after]
                                        (not (number? (attr after))))))
          chain attrs))

(defn less-than
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
