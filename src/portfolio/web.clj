(ns portfolio.web (:use [clojure.walk                     :only [keywordize-keys]]
                        [portfolio.images                 :as images]
                        [portfolio.data                   :as data]
                        [portfolio.util]
                        [hiccup.core]
                        [hiccup.page-helpers]
                        [hiccup.form-helpers]
                        [compojure.core                   :only [defroutes wrap! GET POST ANY]]
                        [ring.util.response               :only [redirect]]
                        [ring.middleware.stacktrace       :only [wrap-stacktrace]]
                        [ring.middleware.file             :only [wrap-file]]
                        [ring.middleware.file-info        :only [wrap-file-info]]
                        [ring.middleware.multipart-params :only [wrap-multipart-params]]
                        [remvee.ring.middleware.basic-authentication]))

;; state
(def production? (not (nil? (System/getenv "APP_DATA"))))
(def *admin* false)

;; routes
(defn collections-url []
  (if *admin* "/admin/collections" "/collections"))

(defn collection-url [c]
  (str (if *admin* "/admin/collection" "/collection") "/" (:slug c)))

(defn collection-remove-url [c]
  (str (collection-url c) "/remove"))

(defn photo-url [p]
  (str (if *admin* "/admin/photo" "/photo")
       "/" (:slug p)))

(defn photo-add-url [c]
  (str (collection-url c) "/add"))

(defn photo-remove-url [p]
  (str (photo-url p) "/remove"))

(defn image-url [p size]
  (str "/image/" (:slug p) "/" size ".jpg"))

;; views
(defn layout [title & body]
  {:headers {"Content-Type" "text/html; charset=UTF-8"}
   :body
   (html
    (doctype :html4)
    [:html
     [:head
      [:meta {:http-equiv "Content-Type",
              :content "text/html; charset=utf-8"}]
      [:title
       (h (str (data/site :name)
               (if title (str " / " title) "")))]
      (include-css "/css/screen.css")
      (when *admin*
        (include-js "/js/jquery.js" "/js/jquery-ui.js" "/js/admin.js"))]
     [:body (when *admin* {:class "admin"})
      [:div.body
       [:div.header
        [:h1 (link-to (collections-url) (h (data/site :name)))]]
       [:div.content
        body]
       [:div.footer
        (h (data/site :copyright))]]]])})

(defn collection-add-form
  ([c] (form-to {:class "add"}
                [:POST (collections-url)]
                (form-field :name c "name")
                (submit-button {:class "submit"} "add")))
  ([] (collection-add-form nil)))

(defn collection-update-form [c]
  (form-to {:class "update"}
           [:POST (collection-url c)]
           (form-field :name c "name")
           (form-field :description c "description" text-area)
           (submit-button {:class "submit"} "update")))

(defn collection-remove-form [c]
  (form-to {:class "remove"}
           [:POST (collection-remove-url c)]
           (submit-button {:class "remove"
                           :onclick "return confirm('Sure?')"}
                          "remove")))

(defn photo-update-title-form [p]
  (form-to {:class "update"}
           [:POST (photo-url p)]
           (form-field :title p "title")
           (submit-button {:class "submit"} "update")))

(defn photo-update-caption-form [p]
  (form-to {:class "update"}
           [:POST (photo-url p)]
           (form-field :caption p "caption" text-area)
           (submit-button {:class "submit"} "update")))

(defn photo-add-form [c p]
  (form-to {:class "add"
            :enctype "multipart/form-data"}
           [:POST (photo-add-url c)]
           (form-field :title p "title")
           (form-field :data p "photo" file-upload)
           (submit-button {:class "submit"} "upload")))

(defn photo-remove-form [p]
  (form-to {:class "remove"}
           [:POST (photo-remove-url p)]
           (submit-button {:class "remove"
                           :onclick "return confirm('Sure?')"}
                          "remove")))

(defn collections-view
  ([c]
     (layout nil
             [:ul.collections
              (map (fn [c]
                     [:li.collection {:id (str "c-" (:slug c))}
                      [:h2
                       [:a {:href (collection-url c)}
                        (h (:name c))]]])
                   (collections))]
             (when *admin*
               (collection-add-form c))
             [:div.address
              (interpose [:br] (data/site :address))]))
  ([] (collections-view nil)))

(defn collection-view
  ([c p]
     (layout (:name c)
             [:div.collection {:id (str "c-" (:slug c))}
              (if *admin*
                [:div.admin
                 (collection-update-form c)
                 (collection-remove-form c)]
                [:h2
                 [:a {:href (collection-url c)}
                  (h (:name c))]])
              (when-not (or *admin* (empty? (:description c)))
                [:div.description (simple-format (:description c))])
              [:ul.thumbs
               (map (fn [p]
                      [:li.thumb {:id (str "p-" (:slug p))}
                       [:a {:href (photo-url p)}
                        [:img {:src (image-url p 'thumb),
                               :alt (:title p)}]]])
                    (:photos c))]
              (when *admin*
                (photo-add-form c p))]))
  ([c] (collection-view c nil)))

(defn photo-view [c p]
  (layout (str (:name c) " / " (:title p))
          [:div.photo {:id (str "p-" (:slug p))
                       }
           [:h2
            [:a {:href (collection-url c)}
             (h (:name c))]
            " / "
            (if *admin*
              (photo-update-title-form p)
              [:span.title (h (:title p))])]
           (when *admin*
             [:div
              (photo-remove-form p)])
           [:a {:href (collection-url c)}
            [:img {:src (image-url p 'preview)
                   :alt (:title p)}]]
           (if *admin*
             (photo-update-caption-form p)
             (when-not (empty? (:caption p))
               [:div.caption (simple-format (:caption p))]))]))

(def *thumb-dimensions* [100 100])
(def *preview-dimensions* [500 375])
(def *background-dimensions* [800 600])

(defn rfc1123-date-format [] (doto (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss z")
                               (.setTimeZone (java.util.TimeZone/getTimeZone "GMT"))))

(defn image-response [p size]
  (let [image (-> (java.io.File. (photo-file p)) images/from-file)
        [width height] (images/dimensions image)
        min (min width height)
        max (max width height)]
    {:headers {"Content-Type"  "image/jpeg"
               "Last-Modified" (.format (rfc1123-date-format)
                                        (java.util.Date. (long (or (:mtime p) 0))))
               "Expires"       (.format (rfc1123-date-format)
                                        (java.util.Date. (+ (System/currentTimeMillis)
                                                            (* 1000 60 60 24 365))))}
     :body (condp = size
               "thumb"   (-> image
                             (images/crop [(if (> width min) (/ (- width min) 2) 0)
                                           (if (> height min) (/ (- height min) 2) 0)]
                                          [min min])
                             (images/scale *thumb-dimensions*)
                             images/to-stream)
               "preview" (-> image
                             (images/scale (images/bounding-box image *preview-dimensions*))
                             (images/copyright (data/site :copyright)
                                               9
                                               (images/color 0 0 0 0.5)
                                               (images/color 1 1 1 0.25))
                             images/to-stream))}))

;; controllers
(defroutes public-routes
  (GET "/collections" []
       (collections-view))

  (GET "/collection/:slug" [slug]
       (collection-view (data/collection-by-slug slug)))

  (GET "/photo/:slug" [slug]
       (let [p (data/photo-by-slug slug)
             c (data/collection-by-photo p)]
         (photo-view c p)))

  (GET "/image/:slug/:size.jpg" [slug size]
       (image-response (data/photo-by-slug slug) size)))

(defroutes private-routes
  (POST "/collections" [name]
        (let [c (data/collection-add name)]
          (if (:errors (meta c))
            (collections-view c)
            (redirect (str (collections-url) "#c-" (:slug c))))))

  (POST "/collections/reorder" {{slugs "slugs[]"} :params}
        (let [c (map data/collection-by-slug slugs)]
          (data/update-site (fn [site]
                              (assoc site
                                :collections
                                (vec c))))
          {:status 200}))

  (POST "/collection/:slug" {{:strs [slug] :as params} :params}
        (let [c (data/collection-update (data/collection-by-slug slug)
                                        (select-keys (keywordize-keys params)
                                                     [:name :description]))]
          (if (:errors (meta c))
            (collection-view c)
            (redirect (str (collection-url c) "#c-" (:slug c))))))

  (POST "/collection/:slug/remove" [slug]
        (let [c (data/collection-by-slug slug)]
          (data/collection-remove c)
          (redirect (collections-url))))

  (POST "/collection/:slug/add" [slug data title]
        (let [c (data/collection-by-slug slug)
              p (data/photo-add c {:title title} data)]
          (if (:errors (meta p))
            (collection-view c p)
            (redirect (str (collection-url c) "#p-" (:slug p))))))

  (POST "/collection/:slug/reorder" {{slug "slug", slugs "slugs[]"} :params}
        (let [c (data/collection-by-slug slug)
              p (map data/photo-by-slug slugs)]
          (data/collection-update c {:photos (vec  p)})
          {:status 200}))

  (POST "/photo/:slug" {{:strs [slug] :as params} :params}
        (let [p (data/photo-by-slug slug)]
          (data/photo-update p (select-keys (keywordize-keys params)
                                            [:title :caption]))
          (redirect (str (photo-url p) "#p-" (:slug p)))))

  (POST "/photo/:slug/remove" [slug]
        (let [p (data/photo-by-slug slug)
              c (data/collection-by-photo p)]
          (data/photo-remove p)
          (redirect (collection-url c)))))

(defn authenticated? [u p]
  (and (= u (data/site :username))
       (= p (data/site :password))))

(defn wrap-admin [app]
  (ANY "/admin/*" {{path "*"} :params}
       (let [app (wrap-basic-authentication
                  (fn [req]
                    (binding [*admin* true]
                      (app (assoc req :uri (str "/" path)))))
                  authenticated?)]
         (if production?
           (wrap-force-ssl app)
           app))))

(defroutes admin
  (wrap-admin public-routes)
  (wrap-admin private-routes))

(defroutes app
  public-routes (var admin))

(wrap! app
       :multipart-params
       (:file "public")
       :file-info)
