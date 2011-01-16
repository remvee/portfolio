;; Copyright (c) Remco van 't Veer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.  You must not remove this notice, or any other, from
;; this software.

(ns #^{:author "Remco van 't Veer"
       :doc "Site data accessors."}
  portfolio.data
  (:use [clojure.test]
        [clojure.contrib.str-utils]
        [clojure.java.io :as io :only [copy file]]
        [portfolio.data.validations :as v]))

;; fixtures
(def *data-dir* (or (System/getenv "APP_DATA") "/tmp"))
(def *data-file* (str *data-dir* "/portfolio.sexp"))
(def *default-site* {:name "Acme Photography"
                     :copyright "Acme Corp. Inc. Limited."
                     :address ["Acme Headquarter"
                               "Somewhere"
                               "In the world"]
                     :username "t"
                     :password "t"})

;; state
(declare *site*)

(defn store
  "Store site data in a file."
  [file]
  (spit file (pr-str @*site*))
  file)

(defn fetch
  "Fetch site data form a file."
  [file]
  (if (.canRead (io/file file))
    (read-string (slurp file))
    *default-site*))

(def *site* (atom (fetch *data-file*)))

(def backup-agent (agent *data-file*))

(defn update-site
  "Swap in new site data and send-off agent to store site in file."
  [f]
  (dosync (swap! *site* f)
          (send-off backup-agent store)))

;; helpers
(defn name->slug
  "Translate a name into a URL friendly slug."
  {:test #(do
            (is (= "name-alt" (name->slug "name" ["name"])))
            (is (= "name-alt-alt" (name->slug "name" ["name" "name-alt"]))))}
  ([name]
     (re-gsub #"-+" "-"
              (re-gsub #"(?i)[^a-z0-9_-]" "-" (or name ""))))
  ([name existing]
     (let [candidate (name->slug name)]
       (if (some (partial = candidate) existing)
         (recur (str candidate "-alt") existing)
         candidate))))

;; models
(defn site
  "Access site data."
  ([] @*site*)
  ([attr] (attr @*site*)))

(defn collections
  "All collections on the site."
  ([] (:collections @*site*))
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
  "Get a collection by slug."
  (first (collections {:slug slug})))

(def collection-validator (v/validator (v/not-blank :name)
                                       (v/unique collections :name)))

(defn collection-validate
  "Validate collection fields."
  [before after]
  (collection-validator before after))

(defn collection-add
  "Add new collection with given name and return it."
  [name]
  (let [c (collection-validate nil
                               {:name name
                                :slug (name->slug name)})]
    (when-not (:errors (meta c))
      (update-site (fn [site]
                     (assoc site
                       :collections
                       (vec (conj (collections) c))))))
    c))

(defn collection-update
  "Update existing collection."
  [before after]
  (let [c (collection-validate before
                               (merge before after))]
    (when-not (:errors (meta c))
      (update-site (fn [site]
                     (assoc site
                       :collections
                       (vec (replace {before c}
                                     (collections)))))))
    c))

(declare photo-remove)

(defn collection-remove
  "Remove given collection from site."
  [collection]
  (doseq [p (:photos collection)]
    (photo-remove p))
  (update-site (fn [site]
                 (assoc site
                   :collections
                   (vec (remove #(= (:slug collection)
                                    (:slug %))
                                (collections)))))))

(defn collection-by-photo
  "Get a collection by photo."
  [photo]
  (first (filter #(some (partial = photo) (:photos %))
                 (collections))))

(def photo-validator (v/validator (v/not-blank :title :slug)
                                  (v/skel :data :need-data
                                          (fn [before after]
                                            (and (empty? before)
                                                 (or (empty? (:data after))
                                                     (= 0 (:size (:data after)))))))))

(defn photo-validate
  "Validate photo fields."
  [before after]
  (photo-validator before after))

(defn photo-by-slug
  "Get a photo by slug."
  [slug]
  (first (filter #(= slug (str (:slug %)))
                 (flatten (map :photos (collections))))))

(defn photo-file
  "Make a filename for a photo."
  [photo]
  (str *data-dir* "/photo-" (:slug photo)))

(defn photo-add
  "Add a photo to a collection."
  [collection attrs data]
  (let [slug (name->slug (str (:slug collection)
                              "-"
                              (:title attrs))
                         (map :slug (:photos collection)))
        photo (photo-validate nil (assoc attrs :slug slug :data data))]
    (when-not (:errors (meta photo))
      (let [new (assoc collection :photos (conj (or (:photos collection) [])
                                                (dissoc photo :data)))]
        (io/copy (:tempfile data) (io/file (photo-file photo)))
        (collection-update collection new)))
    photo))

(defn photo-remove
  "Remove a photo form the site."
  [photo]
  (let [collection (collection-by-photo photo)
        new (assoc collection :photos (vec (filter #(not= (:slug photo)
                                                          (:slug %))
                                                   (:photos collection))))]
    (when photo
      (collection-update collection new)
      (io/delete-file (photo-file photo)))))

(defn photo-update
  "Update an existing photo."
  [photo attrs]
  (let [collection (collection-by-photo photo)
        new (assoc collection :photos (replace {photo (merge photo attrs)}
                                               (:photos collection)))]
    (collection-update collection new)))
