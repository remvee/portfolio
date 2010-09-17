(use 'ring.adapter.jetty)
(require 'portfolio)
(run-jetty portfolio.web/app {:port 8080})
