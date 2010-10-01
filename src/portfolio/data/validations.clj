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

(defn- terminator [before after] [before after])

(defmacro validator [& validators]
  `(fn [before# after#] (last ((-> terminator ~@validators) before# after#))))

(defn- skel [chain attr err pred & args]
  (fn [before after]
    (chain before (if (apply pred before after args)
                    (merge-errors after {attr [err]})
                    after))))
       
(defn not-blank [chain attr]
  (skel chain attr :not-blank (fn [before after]
                            (or (= "" (attr after))
                                (nil? (attr after))))))

(defn numeric [chain attr]
  (skel chain attr :numeric (fn [before after]
                              (not (number? (attr after))))))

(defn unique
  {:test #(are [expected before after coll] (= expected (:errors (meta ((validator (unique :id (fn[] coll)))
                                                                        before after))))
               nil nil {} []
               nil nil {:id 1} []
               {:id [:unique]} nil {:id 1} [{:id 1}]
               nil {:id 1} {:id 1 :foo 1} [{:id 1}]
               {:id [:unique]} {:id 1 :bar 1} {:id 1 :foo 1} [{:id 1}]
               nil nil {:id 2} [{:id 1}])}
  [chain attr coll]
  (skel chain attr :unique (fn [before after]
                             (some #(= (attr after) (attr %))
                                   (filter (partial not= before)
                                           (coll))))))

(deftest example
  (are [expected input] (= expected (:errors (meta ((validator (not-blank :name)
                                                               (not-blank :nr)
                                                               (numeric :nr))
                                                    {} input))))
       {:nr [:numeric :not-blank]
        :name [:not-blank]}
       {:foo "bar"}
       
       {:name [:not-blank]
        :nr [:numeric]}
       {:nr "foo"}))
