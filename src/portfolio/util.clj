(ns portfolio.util
  (:use [clojure.contrib.str-utils]
        [hiccup.core]
        [hiccup.form-helpers]))

;; view helpers
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

(defmethod htmlify java.lang.String [value]
  (html
   [:pre
    (h (str "\"" value "\""))]))

(defmethod htmlify java.lang.Object [value]
  (html
   [:pre
    (h (str "<" (-> value class .getName) " " value ">"))]))

(defmethod htmlify nil [_]
  (html
   [:em "nil"]))

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
     (let [id     (uniq-id attr)
           errors (and (:errors (meta fields)) (attr (:errors (meta fields))))]
       [:div {:class (str "field" (when errors " field-with-error"))}
        [:label {:for id} text]
        (if (= f file-upload)
          (f {:id id} attr)
          (f {:id id} attr (attr fields)))
        (when errors
          [:span {:class "error-message"} (map name errors)])])))

(defn simple-format [text]
  (html (map #(vec (list* :p (interpose
                              [:br]
                              (map h (re-split #"\n"
                                               (chomp %))))))
             (re-split #"\n\s*?\n" text))))
  
;; wrappers
(defn wrap-force-ssl
  "Wrap response to ensure requests are protected by SSL."
  ([app change-schema-fn]
     (fn [req]
       (if (or (= :https (:scheme req))
               (= "https" (get (:headers req) "x_forwarded_proto")))
         (app req)
         (let [url (change-schema-fn req)]
           {:status  302
            :headers {"Location"     url
                      "Content-Type" "text/html"}
            :body    (str "<a href='" url "'>&rarr;</a.>")}))))
  ([app]
     (wrap-force-ssl app #(str "https://"
                               (:server-name %)
                               (:uri %)))))