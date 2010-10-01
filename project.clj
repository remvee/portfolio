(defproject portfolio "1.0.0-SNAPSHOT"
  :description "simple portfolio website"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [compojure "0.4.1"]
                 [hiccup "0.2.6"]
                 [ring/ring-jetty-adapter "0.3.0"]
                 [ring/ring-devel "0.3.0"]
                 [ring-basic-authentication "0.0.1-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [lein-run "1.0.0"]]
  :run-aliases {:server ["script/server.clj"]})
