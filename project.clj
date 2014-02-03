(defproject portfolio "1.0.0-SNAPSHOT"
  :description "simple portfolio website"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [compojure "1.1.3"]
                 [hiccup "1.0.1"]
                 [ring/ring-jetty-adapter "1.1.6"]
                 [ring-basic-authentication "1.0.5"]]
  :main portfolio.core)
