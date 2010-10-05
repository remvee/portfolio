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

(defn color [#^Float red #^Float green #^Float blue #^Float alpha]
  (java.awt.Color. red green blue alpha))

(defn fade [image color]
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

(defn copyright [image text font-size fg-color bg-color]
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
  (. javax.imageio.ImageIO read file))

(defn to-stream [image]
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
