;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

(defprotocol ReceiverProvider
  (get-receiver [this]))

(defn name-key [s]
  (-> (str/trim s)
      (str/replace " " "")
      str/lower-case
      (str/replace "_" "-")
      keyword))

(defn key-name [k]
  (-> (name k)
      (str/trim)
      (str/replace "-" "_")))

(defmethod print-method (Class/forName "[Ljava.lang.Object;")
  [objects ^java.io.Writer w]
  (.write w (pr-str (seq objects))))

(defn simple-name [^Class class]
  (.getSimpleName class))
