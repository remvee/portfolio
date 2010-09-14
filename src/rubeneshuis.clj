(ns rubeneshuis (:use [images]
                      [clojure.contrib.str-utils]
                      [clojure.contrib.io :as io :only [copy file]]
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
(def *collections* (ref []))

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

(defn collection-by-slug [slug]
  (first (collections {:slug slug})))

(defn name->slug [name]
  (re-gsub #"-+" "-"
           (re-gsub #"[^a-z0-9_-]" "-" name)))

(defn collections-create [name]
  (dosync (commute *collections*
                   conj
                   {:name name
                    :slug (name->slug name)})))

(defn collections-update [collection attrs]
  (let [new (merge collection attrs)]
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))
    new))

(defn photo-by-slug [slug]
  (first (filter #(= slug (str (:slug %)))
                 (flatten (map :photos @*collections*)))))

(defn photo-file [photo]
  (str "/tmp/photo-" (:slug photo)))
  
(defn photo-add [collection upload]
  (let [photo {:slug (name->slug (:filename upload))
               :title (:filename upload)}
        new (assoc collection :photos (conj (or (:photos collection) [])
                                            photo))]
    (io/copy (:tempfile upload) (io/file (photo-file photo)))
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))))

; view helpers
(defmulti htmlify class)

(defmethod htmlify java.util.Map [value]
  (html
   [:dl
    (map #(html [:dt (str (first %))]
                [:dd (htmlify (last %))]) value)]))

(defmethod htmlify java.util.List [value]
  (html
   [:ul
    (map #(html [:li (htmlify %)]) value)]))

(defmethod htmlify java.lang.Object [value]
  (html
   [:pre
    (h (str "<" (-> value class .getName) " " value ">"))]))

(defmethod htmlify nil [_]
  (html
   [:em "nil"]))
    
;; routes
(defn collections-url
  ([] (if *admin* "/admin/collections" "/collections"))
  ([collection] (str (collections-url) "/" (:slug collection))))

(defn thumb-url [photo]
  (str "/thumbs/" (:slug photo)))

(defn photo-add-url [collection]
  (str "/admin/collections/" (:slug collection) "/photo"))

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
           (submit-button "create")))

(defn collection-update-form [collection]
  (form-to [:POST (collections-url collection)]
           (text-field :name (:name collection))
           (submit-button "update")))

(defn photo-add-form [collection]
  (form-to {:enctype "multipart/form-data"}
           [:POST (photo-add-url collection)]
           (file-upload :photo) 
           (submit-button "upload")))

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
          [:ul.thumbs
           (map (fn [photo]
                  [:li.thumb
                   [:img {:src (thumb-url photo),
                          :alt (:title photo)}]])
                (:photos collection))]
          (when *admin*
            (photo-add-form collection))))

(defn thumb-view [photo]
  (let [image (-> (java.io.File. (photo-file photo)) file->image)
        [width height] (dimensions image)
        min (min width height)]
    {:content-type "image/jpeg"
     :body (-> image
               (crop (if (> width min) (/ (- width min) 2) 0)
                     (if (> height min) (/ (- height min) 2) 0)
                     min min)
               (scale 100 100)
               image->stream)}))

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
       (collection-view (collection-by-slug slug)))
  (GET "/thumbs/:slug" [slug]
       (thumb-view (photo-by-slug slug))))

(defroutes admin
  (POST "/admin/collections" [name]
        (with-admin
          (collections-create name)
          (redirect (collections-url))))
  
  (POST "/admin/collections/:slug" [slug name photo]
        (with-admin
          (let [collection (collection-by-slug slug)]
            (collections-update collection {:name name})
            (redirect (collections-url collection)))))

  (POST "/admin/collections/:slug/photo" [slug photo]
        (with-admin
          (let [collection (collection-by-slug slug)]
            (photo-add collection photo)
            (redirect (collections-url collection))))))

(defroutes app frontend admin
  (ANY "/*" [] (redirect "/")))

(wrap! app
       :stacktrace
       :multipart-params
       (:file "public"))
