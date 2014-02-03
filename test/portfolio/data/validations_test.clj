(ns portfolio.data.validations-test
  (:use clojure.test)
  (:require [portfolio.data.validations :as v]))

(deftest merge-errors
  (are [expected map errors] (= expected
                                (:errors (meta (v/merge-errors map errors))))
       {:bar [:quux]}
       {:foo 1} {:bar [:quux]}
       
       {:bar [:flarp :quux]}
       (with-meta {:foo 1} {:errors {:bar [:flarp]}}) {:bar [:quux]}))

(deftest less-than
  (are [expected attrs amount]
    (= expected
       (:errors (meta ((v/validator (v/less-than amount :count))
                       nil attrs))))
    
    nil nil nil
    nil {:count 1} 2
    {:count [{:less-than 2}]} {:count 3} 2))

(deftest unique
  (are [expected before after coll]
    (= expected
       (:errors (meta ((v/validator (v/unique (fn [] coll) :id))
                       before after))))
    
    nil nil {} []
    nil nil {:id 1} []
    {:id [:unique]} nil {:id 1} [{:id 1}]
    nil {:id 1} {:id 1 :foo 1} [{:id 1}]
    nil {:id 1 :foo 1} {:id 1 :foo 2} [{:id 1 :foo 1}]
    {:id [:unique]} {:id 1 :bar 1} {:id 1 :foo 1} [{:id 1}]
    nil nil {:id 2} [{:id 1}]))

(deftest combination
  (are [expected input]
    (= expected
       (:errors (meta ((v/validator
                          (v/not-blank :name :nr)
                          (v/numeric :nr)
                          (v/less-than 10 :count))
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
