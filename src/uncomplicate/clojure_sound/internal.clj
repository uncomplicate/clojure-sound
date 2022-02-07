(ns uncomplicate.clojure-sound.internal
  (:require [clojure.string :as str]))

(defprotocol Support
  (supported [feature] [feature object]))

(defprotocol SequenceSource
  (set-sequence [source sequencer!])
  (get-sequence [source]))

(defprotocol Load
  (load-instruments [source synth])
  (unload-instruments [source synth]))

(defn name-key [s]
  (-> (str/trim s)
      (str/replace " " "")
      str/lower-case
      (str/replace "_" "-")
      keyword))

(defmethod print-method (Class/forName "[Ljava.lang.Object;")
  [objects ^java.io.Writer w]
  (.write w (pr-str (seq objects))))

(defn simple-name [^Class class]
  (.getSimpleName class))
