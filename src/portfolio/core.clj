(ns portfolio.core
  (:use portfolio.web
        ring.adapter.jetty))

(defn -main []
  (run-jetty app {:port 8080}))
