;; Copyright (c) Remco van 't Veer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license.  You must not remove this notice, or any other, from
;; this software.

(ns #^{:author "Remco van 't Veer"
       :doc "Operations on bitmap images operations."}
  portfolio.images
  (:use [clojure.math.numeric-tower]))

(defn scale
  "Scale image."
  [image [width height]]
  (.getScaledInstance image width height java.awt.Image/SCALE_SMOOTH))

(defn crop
  "Crop image."
  [image [x y] [width height]]
  (.getSubimage image x y width height))

(defn dimensions
  "Determine image width and height."
  [image]
  [(.getWidth image) (.getHeight image)])

(defn bounding-box
  "Determine bounding box in box-width x box-height for given image."
  [image [box-width box-height]]
  (let [[width height] (dimensions image)]
    (map round
         (if (> (/ width box-width)
                (/ height box-height))
           [box-width (/ height (/ width box-width))]
           [(/ width (/ height box-height)) box-height]))))

(defn color
  "Return a RGBA java.awt.Color instance."
  [#^Float red #^Float green #^Float blue #^Float alpha]
  (java.awt.Color. red green blue alpha))

(defn fade
  "Fade image by applying an overlay of a given color.  Use color
alpha channel to adjust opacity."
  [image color]
  (let [result (java.awt.image.BufferedImage.
                (.getWidth image)
                (.getHeight image)
                java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (doto (.createGraphics result)
      (.drawImage image nil nil)
      (.setColor color)
      (.fillRect 0 0 (.getWidth image) (.getHeight image))
      .dispose)
    (.flush result)
    result))

(defn copyright
  "Insert box with copyright notice in the bottom right corner."
  [image text font-size fg-color bg-color]
  (let [[width height] (dimensions image)
        result (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics result)
        font (java.awt.Font. java.awt.Font/SANS_SERIF java.awt.Font/BOLD font-size)
        frc (.getFontRenderContext graphics)
        layout (java.awt.font.TextLayout. text font frc)
        bounds (.getBounds layout)
        x (int (- width
                  (.getWidth bounds)))
        y (int (- height
                  (.getHeight bounds)))
        padding (/ (.getSize font) 4)]
    (doto graphics
      (.drawImage image nil nil)
      (.setColor bg-color)
      (.fillRect (- x padding) (- y padding) (.getWidth image) (.getHeight image))
      (.setColor fg-color)
      (.setFont font)
      (.drawString text
                   (int (- x (.getX bounds)))
                   (int (- y (.getY bounds)))))
    result))

(defn from-file [file]
  "Read a file into a javax.imageio.ImageIO instance."
  (. javax.imageio.ImageIO read file))

(defn to-stream [image]
  "Create a 100% quality JPEG input stream from given image."
  (let [writer (.next (javax.imageio.ImageIO/getImageWritersByFormatName "JPG"))
        params (.getDefaultWriteParam writer)
        stream (java.io.ByteArrayOutputStream.)
        output (javax.imageio.stream.MemoryCacheImageOutputStream. stream)
        result (java.awt.image.BufferedImage.
                (.getWidth image)
                (.getHeight image)
                java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (doto (.createGraphics result)
      (.drawImage image nil nil)
      .dispose)
    (doto params
      (.setCompressionMode javax.imageio.ImageWriteParam/MODE_EXPLICIT)
      (.setCompressionQuality 1.0))
    (doto writer
      (.setOutput output)
      (.write nil (javax.imageio.IIOImage. result nil nil) params))
    (java.io.ByteArrayInputStream. (.toByteArray stream))))
