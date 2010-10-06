(require 'portfolio)

(use 'ring.adapter.jetty
     'compojure.core
     'portfolio.web
     'portfolio.util)

(run-jetty app {:port 8080})
