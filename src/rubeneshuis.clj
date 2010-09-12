(ns rubeneshuis (:use [compojure.core             :only [defroutes wrap! GET POST ANY]]
                      [hiccup.core]
                      [hiccup.page-helpers]
                      [hiccup.form-helpers]
                      [ring.util.response         :only [redirect]]
                      [ring.middleware.stacktrace :only [wrap-stacktrace]]
                      [ring.middleware.file       :only [wrap-file]]))

(def *name* "Ruben Eshuis Photography")
(def *copyright* "Copyright all images Ruben Eshuis Photography")
(def *address* ["cel: +31 (0)6-16476240"
                "fax: +31 (0)20-4286008"
                "mail: <a href='mailto:r.e.eshuis@chello.nl'>r.e.eshuis@chello.nl</a>"
                "www: <a href='http://www.rubeneshuis.com'>www.rubeneshuis.com</a>"])

;; models
(def *collections*
     (ref [{:name "eerst collectie",
            :slug "test-1",
            :photos [{:title "cool",
                      :file "cool.jpg"},
                     {:title "not so cool",
                      :file "lame.jpg"}]}
           {:name "test 2",
            :slug "test-2"}
           {:name "test 3",
            :slug "test-3"}]))

(defn collections
  ([] (deref *collections*))
  ([where]
     (filter (fn [c]
               (reduce (fn [m k]
                         (and m
                              (= (get where k)
                                 (get c k))))
                       true
                       (keys where)))
             (collections))))

;; routes
(defn collection-url [collection]
  (str "/collection/" (:slug collection)))
(defn home-url []
  "/home")

;; views
(defn layout [& body]
  {:headers {"Content-Type" "text/html"}
   :body
   (html
    (doctype :html4)
    [:html
     [:head
      [:title
       (h *name*)]
      (include-css "/css/screen.css")]
     [:body
      [:div.header
       [:h1 (link-to (home-url) (h *name*))]]
      [:div.content
       body]
      [:div.footer
       (h *copyright*)]]])})

(defn index-view []
  (layout [:ul
           (map (fn [collection]
                  [:li
                   [:h2
                    [:a {:href (collection-url collection)}
                     (h (:name collection))]]])
                (collections))]
          [:div.address
           (interpose [:br] *address*)]))

(defn collection-view [slug]
  (let [collection (first (collections {:slug slug}))]
    (layout [:h2
             [:a {:href (home-url)}
              (h (:name collection))]]
            [:ul.thumbs
             (map (fn [photo]
                    [:li.thumb
                     [:img {:src (:file photo),
                            :alt (:title photo)}]])
                  (:photos collection))])))

(defroutes app
  (GET "/home" [] (index-view))
  (GET "/collection/:slug" [slug] (collection-view slug)))

(wrap! app
       :stacktrace
       (:file "public"))
