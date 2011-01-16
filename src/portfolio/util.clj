;; Copyright (c) Remco van 't Veer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.  You must not remove this notice, or any other, from
;; this software.

(ns #^{:author "Remco van 't Veer"
       :doc "HTML helper functions and middleware."}
  portfolio.util
  (:use [clojure.contrib.str-utils]
        [hiccup.core]
        [hiccup.form-helpers]))

;; view helpers
(defmulti htmlify "Render object to HTML." class)

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

(defn uniq-id
  "Create an unique identifier to be used in HTML."
  [key]
  (let [current (or (.get uniq-id-counter) {})
        registered (assoc current key (inc (get current key 0)))]
    (.set uniq-id-counter registered)
    (str (name key) "-" (get registered key))))

(defn form-field
  "Render a form field for a given map attribute.  It includes a label
tag and error messages when available as meta data."
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

(defn simple-format
  "Very basic plain text to HTML formatter."
  [text]
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