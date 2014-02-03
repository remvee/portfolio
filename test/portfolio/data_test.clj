(ns portfolio.data-test
  (:use clojure.test)
  (:require [portfolio.data :as d]))

(deftest name->slug
  (do
    (is (= "name-alt" (d/name->slug "name" ["name"])))
    (is (= "name-alt-alt" (d/name->slug "name" ["name" "name-alt"])))))
