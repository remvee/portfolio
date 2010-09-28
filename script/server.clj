(require 'portfolio)

(use 'ring.adapter.jetty
     'compojure.core
     'portfolio.web
     'portfolio.util)

(wrap! admin :force-ssl)

(run-jetty app {:port 8080})
