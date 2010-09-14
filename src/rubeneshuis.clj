(ns rubeneshuis (:use [images]
                      [clojure.contrib.str-utils]
                      [compojure.core             :only [defroutes wrap! GET POST ANY]]
                      [hiccup.core]
                      [hiccup.page-helpers]
                      [hiccup.form-helpers]
                      [ring.util.response         :only [redirect]]
                      [ring.middleware.stacktrace :only [wrap-stacktrace]]
                      [ring.middleware.file       :only [wrap-file]]
                      [ring.middleware.multipart-params :only [wrap-multipart-params]]))

(def *name* "Ruben Eshuis Photography")
(def *copyright* "Copyright all images Ruben Eshuis Photography")
(def *address* ["cel: +31 (0)6-16476240"
                "fax: +31 (0)20-4286008"
                "mail: <a href='mailto:r.e.eshuis@chello.nl'>r.e.eshuis@chello.nl</a>"
                "www: <a href='http://www.rubeneshuis.com'>www.rubeneshuis.com</a>"])

;; state
(def *admin* false)
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

;; models
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

(defn collections-slug [name]
  (re-gsub #"-+" "-"
           (re-gsub #"[^a-z0-9_-]" "-" name)))

(defn collections-create [name]
  (dosync (commute *collections*
                   conj
                   {:name name
                    :slug (collections-slug name)})))

(defn collections-update [collection attrs]
  (let [new (merge collection attrs)]
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))
    new))

;; routes
(defn collections-url
  ([] (if *admin* "/admin/collections" "/collections"))
  ([collection] (str (collections-url) "/" (:slug collection))))

(defn thumb-url [collection photo]
  (str "/thumbs/" (:slug collection) "/" (:file photo)))

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
     [:body (when *admin* {:class "admin"})
      [:div.header
       [:h1 (link-to (collections-url) (h *name*))]]
      [:div.content
       body]
      [:div.footer
       (h *copyright*)]]])})

(defn collection-create-form []
  (form-to [:POST (collections-url)]
           (text-field :name)
           (submit-button "save")))

(defn collection-update-form [collection]
  (form-to [:POST (collections-url collection)]
           (text-field :name (:name collection))
           (submit-button "save")))

(defn index-view []
  (layout [:ul
           (map (fn [collection]
                  [:li
                   [:h2
                    [:a {:href (collections-url collection)}
                     (h (:name collection))]]])
                (collections))
           (when *admin*
             [:li
              (collection-create-form)])]
          [:div.address
           (interpose [:br] *address*)]))

(defn collection-view [collection]
  (layout [:h2
           [:a {:href (collections-url)}
            (h (:name collection))]]
          (when *admin*
            (collection-update-form collection))
          [:ul.thumbs
           (map (fn [photo]
                  [:li.thumb
                   [:img {:src (thumb-url collection photo),
                          :alt (:title photo)}]])
                (:photos collection))]))

(defn thumb-view [photo]
  (let [image (-> (java.io.File. (str "/tmp/" (:file photo))) file->image)
        [width height] (dimensions image)
        min (min width height)]
    {:content-type "image/jpeg"
     :body (-> image
               (crop (if (> width min) (/ (- width min) 2) 0)
                     (if (> height min) (/ (- height min) 2) 0)
                     min min)
               (scale 100 100)
               image->stream)}))

;(thumb-view {:file "cool.jpg"})

;; controllers
(defmacro with-admin [& form]
  `(binding [*admin* true]
     ~@form))

(defmacro defroutes-with-admin [name & routes]
  (list* 'defroutes name
         (mapcat (fn [route]
                   (list route
                         `(~(first route)
                           ~(str "/admin" (second route))
                           ~(nth route 2)
                           (with-admin ~@(drop 3 route)))))
                 routes)))

(defroutes-with-admin frontend
  (GET "/collections" []
       (index-view))
  (GET "/collections/:slug" [slug]
       (collection-view (first (collections {:slug slug}))))
  (GET ["/thumbs/:slug/:file", :file  #"[a-z0-9.-]+"] [slug file]
       (let [collection (first (collections {:slug slug}))
             photo (first (filter #(= file (:file %)) (:photos collection)))]
         (thumb-view photo))))

(defroutes admin
  (POST "/admin/collections" [name]
        (with-admin
          (collections-create name)
          (redirect (collections-url))))
  (POST "/admin/collections/:slug" [slug name]
        (with-admin
          (let [collection (first (collections {:slug slug}))]
            (redirect
             (collections-url
              (collections-update collection {:name name})))))))

(defroutes app frontend admin
  (ANY "/*" [] (redirect "/")))

(wrap! app
       :stacktrace
       (:file "public"))
