(ns portfolio.util
  (:use [hiccup.core]
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
     (let [id    (uniq-id attr)
           error (when (:errors fields) (attr (:errors fields)))]
       [:div {:class (str "field" (when error " field-with-error"))}
        [:label {:for id} text]
        (if (= f file-upload)
          (f {:id id} attr)
          (f {:id id} attr (attr fields)))
        [:span {:class "error-message"} error]])))
