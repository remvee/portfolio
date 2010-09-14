(ns portfolio.images)

(defn scale [image width height]
  (.getScaledInstance image width height java.awt.Image/SCALE_SMOOTH))

(defn crop [image x y width height]
  (.getSubimage image x y width height))

(defn dimensions [image]
  [(.getWidth image) (.getHeight image)])

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
