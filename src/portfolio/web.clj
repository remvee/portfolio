(ns portfolio.web (:use [portfolio.images :as images]
                        [portfolio.data   :as data]
                        [portfolio.util   :as util]
                        [hiccup.core]
                        [hiccup.page-helpers]
                        [hiccup.form-helpers]
                        [compojure.core                   :only [defroutes wrap! GET POST ANY]]
                        [ring.util.response               :only [redirect]]
                        [ring.middleware.stacktrace       :only [wrap-stacktrace]]
                        [ring.middleware.file             :only [wrap-file]]
                        [ring.middleware.multipart-params :only [wrap-multipart-params]]))

;; state
(def *admin* false)
    
;; routes
(defn collections-url
  ([] (if *admin* "/admin/collections" "/collections"))
  ([c] (str (collections-url) "/" (:slug c))))

(defn thumb-url [p]
  (str "/thumbs/" (:slug p)))

;; helpers
(def uniq-id-counter (ThreadLocal.))

(defn uniq-id [key]
  (let [current (or (.get uniq-id-counter) {})
        registered (assoc current key (inc (get current key 0)))]
    (.set uniq-id-counter registered)
    (str (name key) "-" (get registered key))))

(defn form-field
  ([attr fields text]
     (form-field attr fields text text-field))
  ([attr fields text f]
     (let [id    (uniq-id attr)
           error (when (:errors fields) (attr (:errors fields)))]
       [:div {:class (str "field" (when error " field-with-error"))}
        [:label {:for id} text]
        (if (= f file-upload)
          (f {:id id} attr)
          (f {:id id} attr (attr fields)))
        [:span {:class "error-message"} error]])))

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

(defn collection-create-form
  ([c] (form-to [:POST (collections-url)]
                (form-field :name c "name")
                (submit-button "create")))
  ([] (collection-create-form nil)))

(defn collection-update-form [c]
  (form-to {:enctype "multipart/form-data"}
           [:POST (collections-url c)]
           (form-field :name c "name")
           (form-field :photo c "photo" file-upload)
           (submit-button "update")))

(defn index-view []
  (layout [:ul
           (map (fn [c]
                  [:li
                   [:h2
                    [:a {:href (collections-url c)}
                     (h (:name c))]]])
                (collections))
           (when *admin*
             [:li
              (collection-create-form)])]
          [:div.address
           (interpose [:br] *address*)]))

(defn collection-view [c]
  (layout [:h2
           [:a {:href (collections-url)}
            (h (:name c))]]
          [:ul.thumbs
           (map (fn [p]
                  [:li.thumb
                   [:img {:src (thumb-url p),
                          :alt (:title p)}]])
                (:photos c))]
          (when *admin*
            (collection-update-form c))))

(defn thumb-view [p]
  (let [image (-> (java.io.File. (photo-file p)) images/from-file)
        [width height] (images/dimensions image)
        min (min width height)]
    {:content-type "image/jpeg"
     :body (-> image
               (images/crop (if (> width min) (/ (- width min) 2) 0)
                            (if (> height min) (/ (- height min) 2) 0)
                            min min)
               (images/scale 100 100)
               images/to-stream)}))
  
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
       (collection-view (data/collection-by-slug slug)))
  (GET "/thumbs/:slug" [slug]
       (thumb-view (data/photo-by-slug slug))))

(defroutes admin
  (POST "/admin/collections" [name]
        (with-admin
          (data/collections-create name)
          (redirect (collections-url))))
  
  (POST "/admin/collections/:slug" [slug name photo]
        (with-admin
          (let [c (data/collection-by-slug slug)]
            (data/collections-update c {:name name})
            (when (> (:size photo) 0)
              (data/photo-add c photo))
            (redirect (collections-url c))))))

(defroutes app frontend admin
  (ANY "/*" [] (redirect "/")))

(wrap! app
       :stacktrace ; TODO remove me
       :multipart-params
       (:file "public"))
