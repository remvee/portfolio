(ns portfolio.data
  (:use [clojure.contrib.str-utils]
        [clojure.contrib.io :as io :only [copy file]]))

;; fixtures
(def *name* "Ruben Eshuis Photography")
(def *copyright* "Copyright all images Ruben Eshuis Photography")
(def *address* ["cel: +31 (0)6-16476240"
                "fax: +31 (0)20-4286008"
                "mail: <a href='mailto:r.e.eshuis@chello.nl'>r.e.eshuis@chello.nl</a>"
                "www: <a href='http://www.rubeneshuis.com'>www.rubeneshuis.com</a>"])

;; state
(declare *collections*)
(def *data-file* "/tmp/portfolio.sexp")
(defn- store! [] (spit *data-file* (pr-str (deref *collections*))))
(defn- read! [] (if (.canRead (io/file *data-file*)) (read-string (slurp *data-file*)) []))

(def *collections* (ref (read!)))

;; helpers
(defn name->slug [name]
  (re-gsub #"-+" "-"
           (re-gsub #"[^a-z0-9_-]" "-" name)))

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

(defn collections-create [name]
  (dosync (commute *collections*
                   conj
                   {:name name
                    :slug (name->slug name)}))
  (store!))

(defn collections-update [collection attrs]
  (let [new (merge collection attrs)]
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))
    (store!)
    new))

(defn photo-by-slug [slug]
  (first (filter #(= slug (str (:slug %)))
                 (flatten (map :photos @*collections*)))))

(defn photo-file [photo]
  (str "/tmp/photo-" (:slug photo)))
  
(defn photo-add [collection attrs]
  (let [photo {:slug (name->slug (:filename attrs))
               :title (:title attrs)}
        new (assoc collection :photos (conj (or (:photos collection) [])
                                            photo))]
    (io/copy (:tempfile attrs) (io/file (photo-file photo)))
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))
    (store!)))

(defn photo-remove [collection slug]
  (let [photo (photo-by-slug slug)
        new (assoc collection :photos (filter #(not= slug (:slug %))
                                              (:photos collection)))]
    (when photo
      (dosync (commute *collections*
                       (fn [coll]
                         (replace {collection new}
                                  coll))))
      (store!)
      (io/delete-file (photo-file photo)))))

(defn photo-update [collection slug attrs]
  (let [photo (photo-by-slug slug)
        new (assoc collection :photos (replace {photo (merge photo attrs)}
                                               (:photos collection)))]
    (dosync (commute *collections*
                     (fn [coll]
                       (replace {collection new}
                                coll))))
    (store!)))
