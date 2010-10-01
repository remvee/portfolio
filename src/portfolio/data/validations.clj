(ns portfolio.data.validations
  (:use [clojure.test]))

(defn- merge-errors
  {:test #(are [expected map errors] (= expected
                                        (merge-errors map errors))
               
               {:foo 1, :errors {:bar [:quux]}}
               {:foo 1} {:bar [:quux]}

               {:foo 1, :errors {:bar '(:flarp :quux)}}
               {:foo 1, :errors {:bar [:flarp]}} {:bar [:quux]})}
  [map errors]
  (if (empty? errors)
    map
    (assoc map :errors (merge-with concat
                                   (:errors map)
                                   errors))))

(defn- terminator [before after] [before after])

(defmacro validator [& validators]
  `(fn [before# after#] (last ((-> terminator ~@validators) before# after#))))

(defn- skel [chain attr err pred & args]
  (fn [before after]
    (chain before (if (apply pred before after args)
                    (merge-errors after {attr [err]})
                    after))))
       
(defn blank [chain attr]
  (skel chain attr :blank (fn [before after]
                            (or (= "" (attr after))
                                (nil? (attr after))))))

(defn numeric [chain attr]
  (skel chain attr :numeric (fn [before after]
                              (not (number? (attr after))))))

(defn unique
  {:test #(are [expected before after coll] (= expected (last ((unique terminator :id (fn[] coll))
                                                               before after)))
               {} nil {} []
               {:id 1} nil {:id 1} []
               {:id 1 :errors {:id [:unique]}} nil {:id 1} [{:id 1}]
               {:id 1 :foo 1} {:id 1} {:id 1 :foo 1} [{:id 1}]
               {:id 1 :foo 1 :errors {:id [:unique]}} {:id 1 :bar 1} {:id 1 :foo 1} [{:id 1}]
               {:id 2} nil {:id 2} [{:id 1}])}
  [chain attr coll]
  (skel chain attr :unique (fn [before after]
                             (some #(= (attr after) (attr %))
                                   (filter (partial not= before)
                                           (coll))))))

(deftest example
  (are [expected input] (= expected ((validator (blank :name)
                                                (blank :nr)
                                                (numeric :nr))
                                     {} input))
       {:foo "bar" :errors {:nr [:numeric :blank]
                            :name [:blank]}}
       {:foo "bar"}
       
       {:nr "foo" :errors {:name [:blank]
                           :nr [:numeric]}}
       {:nr "foo"}))
