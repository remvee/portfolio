(ns portfolio.web (:use [portfolio.images :as images]
                        [portfolio.data   :as data]
                        [portfolio.util]
                        [hiccup.core]
                        [hiccup.page-helpers]
                        [hiccup.form-helpers]
                        [compojure.core                   :only [defroutes wrap! GET POST ANY]]
                        [ring.util.response               :only [redirect]]
                        [ring.middleware.stacktrace       :only [wrap-stacktrace]]
                        [ring.middleware.file             :only [wrap-file]]
                        [ring.middleware.file-info        :only [wrap-file-info]]
                        [ring.middleware.multipart-params :only [wrap-multipart-params]]))

;; state
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
(defn layout [& body]
  {:headers {"Content-Type" "text/html"}
   :body
   (html
    (doctype :html4)
    [:html
     [:head
      [:title
       (h (:name (data/site)))]
      (include-css "/css/screen.css")]
     [:body (when *admin* {:class "admin"})
      [:div.header
       [:h1 (link-to (collections-url) (h (:name (data/site))))]]
      [:div.content
       body]
      [:div.footer
       (h (:copyright (data/site)))]]])})

(defn collection-add-form
  ([c] (form-to [:POST (collections-url)]
                (form-field :name c "name")
                (submit-button "add")))
  ([] (collection-add-form nil)))

(defn collection-update-form [c]
  (form-to [:POST (collection-url c)]
           (form-field :name c "name")
           (submit-button "update")))

(defn collection-remove-form [c]
  (form-to [:POST (collection-remove-url c)]
           (submit-button "remove")))

(defn photo-update-form [p]
  (form-to [:POST (photo-url p)]
           (form-field :title p "title")
           (submit-button "update")))

(defn photo-add-form [c]
  (form-to {:enctype "multipart/form-data"}
           [:POST (photo-add-url c)]
           (form-field :title c "title")
           (form-field :photo c "photo" file-upload)
           (submit-button "upload")))

(defn photo-remove-form [p]
  (form-to [:POST (photo-remove-url p)]
           (submit-button "remove")))
  
(defn collections-view
  ([c]
     (layout [:ul
              (map (fn [c]
                     [:li
                      [:h2
                       [:a {:href (collection-url c)}
                        (h (:name c))]]])
                   (collections))
              (when *admin*
                [:li
                 (collection-add-form c)])]
             [:div.address
              (interpose [:br] (:address (data/site)))]))
  ([] (collections-view nil)))

(defn collection-view [c]
  (layout (if *admin*
            [:div.admin
             (collection-update-form c)
             (collection-remove-form c)]
            [:h2
             [:a {:href (collection-url c)}
              (h (:name c))]])
          [:ul.thumbs
           (map (fn [p]
                  [:li.thumb
                   [:a {:href (photo-url p)}
                    [:img {:src (image-url p 'thumb),
                           :alt (:title p)}]]])
                (:photos c))]
          (when *admin*
            (photo-add-form c))))

(defn photo-view [c p]
  (layout [:h2
           [:a {:href (collection-url c)}
            (h (:name c))]]
          [:div.photo
           (if *admin*
             [:div
              (photo-update-form p)
              (photo-remove-form p)]
             [:h3
              (h (:title p))])
           [:a {:href (collection-url c)}
            [:img {:src (image-url p 'preview)
                   :alt (:title p)}]]]))

(def *thumb-dimensions* [100 100])
(def *preview-dimensions* [500 375])

(def rfc1123 (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss z"))
(.setTimeZone rfc1123 (java.util.TimeZone/getTimeZone "GMT"))

(defn image-response [p size]
  (let [image (-> (java.io.File. (photo-file p)) images/from-file)
        [width height] (images/dimensions image)
        min (min width height)
        max (max width height)]
    {:headers {"Content-Type" "image/jpeg"
               "Expire" (.format rfc1123
                                 (java.util.Date. (+ (System/currentTimeMillis)
                                                     (* 1000 60 60 24 365))))}
     :body (condp = size
               "thumb" (-> image
                           (images/crop [(if (> width min) (/ (- width min) 2) 0)
                                         (if (> height min) (/ (- height min) 2) 0)]
                                        [min min])
                           (images/scale *thumb-dimensions*)
                           images/to-stream)
               "preview" (images/to-stream (if (> (/ width (first *preview-dimensions*))
                                                  (/ height (last *preview-dimensions*)))
                                             (images/scale image [(first *preview-dimensions*) -1])
                                             (images/scale image [-1 (last *preview-dimensions*)]))))
               }))
  
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
       (collections-view))
  
  (GET "/collection/:slug" [slug]
       (collection-view (data/collection-by-slug slug)))
  
  (GET "/photo/:slug" [slug]
       (let [p (data/photo-by-slug slug)
             c (data/collection-by-photo p)]
         (photo-view c p)))
  
  (GET "/image/:slug/:size.jpg" [slug size]
       (image-response (data/photo-by-slug slug) size)))

(defroutes admin
  (POST "/admin/collections" [name]
        (with-admin
          (let [c (data/collection-add name)]
            (if (:errors c)
              (collections-view c)
              (redirect (collections-url))))))
  
  (POST "/admin/collection/:slug" [slug name]
        (with-admin
          (let [c (data/collection-update (data/collection-by-slug slug)
                                          {:name name})]
            (if (:errors c)
              (collection-view c)
              (redirect (collection-url c))))))
  
  (POST "/admin/collection/:slug/remove" [slug]
        (with-admin
          (let [c (data/collection-by-slug slug)]
            (data/collection-remove c)
            (redirect (collections-url)))))
  
  (POST "/admin/collection/:slug/add" [slug photo title]
        (with-admin
          (let [c (data/collection-by-slug slug)]
            (when (> (:size photo) 0)
              (data/photo-add c (merge {:title title} photo)))
            (redirect (collection-url c)))))

  (POST "/admin/photo/:slug" [slug title]
        (with-admin
          (let [p (data/photo-by-slug slug)]
            (data/photo-update p {:title title})
            (redirect (photo-url p)))))

  (POST "/admin/photo/:slug/remove" [slug]
        (with-admin
          (let [p (data/photo-by-slug slug)
                c (data/collection-by-photo p)]
            (data/photo-remove p)
            (redirect (collection-url c))))))

(defroutes app frontend admin
  (ANY "/*" [] (redirect "/")))

(wrap! app
       :stacktrace ; TODO remove me
       :multipart-params
       (:file "public")
       :file-info)
