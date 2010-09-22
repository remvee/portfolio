(ns portfolio.data
  (:use [clojure.test]
        [clojure.contrib.str-utils]
        [clojure.java.io :as io :only [copy file]]))

;; fixtures
(def *data-dir* (or (get (System/getenv) "APP_DATA") "/tmp"))
(def *data-file* (str *data-dir* "/portfolio.sexp"))
(def *default-site* {:name "Acme Photography"
                     :copyright "Acme Corp. Inc. Limited."
                     :address ["Acme Headquarter"
                               "Somewhere"
                               "In the world"]})
;; state
(declare *site*)

(defn- store! [] (spit *data-file* (pr-str (deref *site*))))
(defn- read! [] (if (.canRead (io/file *data-file*)) (read-string (slurp *data-file*)) *default-site*))

(def *site* (ref (read!)))

;; helpers
(defn name->slug [name]
  (re-gsub #"-+" "-"
           (re-gsub #"[^a-z0-9_-]" "-" (or name ""))))

(defn errors
  "Collect errors in attrs map."
  {:test #(do
            (is (= {:email "may not be blank"}
                   (errors {:name "test"} {:not-blank [:name :email]})))
            (is (= {:email "may not be blank" :name "may not be blank"}
                   (errors {} {:not-blank [:name :email]})))
            (is (= {:name "may not be blank", :count "not a number"}
                   (errors {:name "", :count "42"} {:not-blank [:name], :numeric [:count]})))
            (is (empty?
                 (errors {:name "test", :count 42} {:not-blank [:name], :numeric [:count]}))))}
  [attrs {not-blank :not-blank numeric :numeric}]
  (reduce merge
          (concat
           (map #(when (or (= "" (get attrs %))
                           (nil? (get attrs %)))
                   {% "may not be blank"})
                not-blank)
           (map #(when (not (number? (get attrs %)))
                   {% "not a number"})
                numeric))))

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

(defn collection-validate [before after]
  (merge-with merge
              after
              {:errors (errors after {:not-blank [:name]})}
              (if (some #(= (:name after) (:name %))
                        (remove (partial = before) (collections)))
                {:errors {:name "already taken"}}
                nil)))
  
(defn collection-add [name]
  (let [c (collection-validate nil
                               {:name name
                                :slug (name->slug name)})]
    (when-not (:errors c)
      (dosync (commute *site*
                       assoc
                       :collections
                       (vec (conj (collections)
                                  (dissoc c :errors)))))
      (store!))
    c))

(defn collection-update [collection attrs]
  (let [c (collection-validate collection
                               (merge collection attrs))]
    (when-not (:errors c)
      (dosync (commute *site*
                       assoc
                       :collections
                       (vec (replace {collection
                                      (dissoc c :errors)}
                                     (collections)))))
      (store!))
    c))

(declare photo-remove)

(defn collection-remove [collection]
  (doseq [p (:photos collection)]
    (photo-remove p))
  (dosync (commute *site*
                   assoc
                   :collections
                   (vec (remove #(= (:slug collection)
                                    (:slug %))
                                (collections)))))
  (store!))

(defn collection-by-photo [photo]
  (first (filter #(some (partial = photo) (:photos %))
                 (collections))))

(defn photo-validate [before after]
  (merge-with merge
              after
              {:errors (errors after {:not-blank [:title]})}))

(defn photo-by-slug [slug]
  (first (filter #(= slug (str (:slug %)))
                 (flatten (map :photos (collections))))))

(defn photo-file [photo]
  (str *data-dir* "/photo-" (:slug photo)))
  
(defn photo-add [collection attrs data] ; TODO title needs to be uniq in collection
  (let [slug (str (:slug collection) "-" (name->slug (:title attrs)))
        photo (merge-with merge
                          (photo-validate nil (assoc attrs :slug slug))
                          (if (not (> (:size data) 0))
                            {:errors {:data "may not be blank"}}
                            nil))]
    (when-not (:errors photo)
      (let [new (assoc collection :photos (conj (or (:photos collection)
                                                    [])
                                                (dissoc photo :errors)))]
        (io/copy (:tempfile data) (io/file (photo-file photo)))
        (collection-update collection new)))
    photo))

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
 