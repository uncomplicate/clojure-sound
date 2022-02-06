(ns uncomplicate.clojure-sound.internal
  (:require [clojure.string :as str]))

(defprotocol Support
  (supported [feature] [feature object]))

(defn name-key [s]
  (-> (str/trim s)
      (str/replace " " "")
      str/lower-case
      (str/replace "_" "-")
      keyword))

(defmethod print-method (Class/forName "[Ljava.lang.Object;")
  [objects ^java.io.Writer w]
  (.write w (pr-str (seq objects))))
