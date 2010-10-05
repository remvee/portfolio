(ns portfolio.images
  (:use [clojure.contrib.math]))

(defn scale [image [width height]]
  (.getScaledInstance image width height java.awt.Image/SCALE_SMOOTH))

(defn crop [image [x y] [width height]]
  (.getSubimage image x y width height))

(defn dimensions [image]
  [(.getWidth image) (.getHeight image)])

(defn bounding-box [image [box-width box-height]]
  (let [[width height] (dimensions image)]
    (map round
         (if (> (/ width box-width)
                (/ height box-height))
           [box-width (/ height (/ width box-width))]
           [(/ width (/ height box-height)) box-height]))))

(defn fade
  ([image color]
     (let [result (java.awt.image.BufferedImage.
                   (.getWidth image)
                   (.getHeight image)
                   java.awt.image.BufferedImage/TYPE_INT_RGB)]
       (doto (.createGraphics result)
         (.drawImage image nil nil)
         (.setColor color)
         (.fillRect 0 0 (.getWidth image) (.getHeight image))
         .dispose)
       (.flush result)
       result))
    ([image #^Float red #^Float green #^Float blue #^Float alpha]
     (fade image (java.awt.Color. red green blue alpha))))

(defn from-file [file]
  (. javax.imageio.ImageIO read file))

(defn to-stream [image]
  (let [in (java.io.PipedInputStream.)
        out (java.io.PipedOutputStream. in)
        result (java.awt.image.BufferedImage.
                (.getWidth image)
                (.getHeight image)
                java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (doto (.createGraphics result) (.drawImage image nil nil) .dispose)
    (.flush result)
    (doto (Thread. (fn []
                     (. javax.imageio.ImageIO write result "JPG" out)
                     (doto out .flush .close))) .start)
    in))
