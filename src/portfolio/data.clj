(ns portfolio.data
  (:use [clojure.contrib.str-utils]
        [clojure.contrib.io :as io :only [copy file]]))

;; fixtures
(def *data-dir* (or (get (System/getenv) "APP_DATA") "/tmp"))
(def *data-file* (str *data-dir* "/portfolio.sexp"))

;; state
(declare *site*)
(defn- store! [] (spit *data-file* (pr-str (deref *site*))))
(defn- read! [] (if (.canRead (io/file *data-file*)) (read-string (slurp *data-file*)) {}))
(def *site* (ref (read!)))

;; helpers
(defn name->slug [name]
  (re-gsub #"-+" "-"
           (re-gsub #"[^a-z0-9_-]" "-" name)))

;; models
(defn site
  ([] (deref *site*))
  ([k] (get (site) k)))

(defn collections
  ([] (site :collections))
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

(defn collection-create [name]
  (dosync (commute *site*
                   assoc
                   :collections
                   (vec (conj (collections)
                              {:name name
                               :slug (name->slug name)}))))
  (store!))

(defn collection-update [collection attrs]
  (let [new (merge collection attrs)]
    (dosync (commute *site*
                     assoc
                     :collections
                     (vec (replace {collection new}
                                   (collections)))))
    (store!)
    new))

(defn collection-by-photo [photo]
  (first (filter #(some (partial = photo) (:photos %))
                 (collections))))

(defn photo-by-slug [slug]
  (first (filter #(= slug (str (:slug %)))
                 (flatten (map :photos (collections))))))

(defn photo-file [photo]
  (str *data-dir* "/photo-" (:slug photo)))
  
(defn photo-add [collection attrs]
  (let [photo {:slug (name->slug (:filename attrs))
               :title (:title attrs)}
        new (assoc collection :photos (conj (or (:photos collection) [])
                                            photo))]
    (io/copy (:tempfile attrs) (io/file (photo-file photo)))
    (collection-update collection new)))

(defn photo-remove [photo]
  (let [collection (collection-by-photo photo)
        new (assoc collection :photos (vec (filter #(not= (:slug photo)
                                                          (:slug %))
                                                   (:photos collection))))]
    (when photo
      (collection-update collection new)
      (io/delete-file (photo-file photo)))))

(defn photo-update [photo attrs]
  (let [collection (collection-by-photo photo)
        new (assoc collection :photos (replace {photo (merge photo attrs)}
                                               (:photos collection)))]
    (collection-update collection new)
    (store!)))
 