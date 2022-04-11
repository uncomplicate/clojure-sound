(ns uncomplicate.clojure-sound.sampled-test
  (:require [midje.sweet :refer [facts throws => =not=> roughly truthy]]
            [clojure.string :refer [includes?]]
            [uncomplicate.commons.core :refer [close! info with-release]]
            [uncomplicate.clojure-sound
             [core :refer :all]
             [sampled :refer :all]])
  (:import [javax.sound.sampled SourceDataLine TargetDataLine Clip]))

(facts "Getting a mixer."
       (pos? (count (mixer-info))) => true
       (keys (info (first (mixer-info)))) => [:description :name :vendor :version]
       (info (first (mixer-info)) :vendor) => "ALSA (http://www.alsa-project.org)"
       (mixer) => (map mixer (mixer-info))
       (mixer (first (mixer-info))) => (first (mixer))
       (mixer-info (first (mixer))) => (first (mixer-info)))

(let [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))
      noise-format (audio-format noise)
      mxr (first (filter #(includes? (info % :name) "[default]")(mixer)))]

  (facts "Getting a line directly from audio system."
         (info noise-format :encoding) => :pcm-signed
         (encoding noise-format) => (encoding :pcm-signed)
         (let [noise-info (line-info :target noise-format)
               noise-line (line (line-info :target noise-format))]
           (try
             (line-class noise-info) => javax.sound.sampled.TargetDataLine
             (open? noise-line) => false
             (open! noise-line noise-format) => noise-line
             (supported? :line-out) => false
             (supported? (port-info :line-out)) => false
             (line (line-info :line-out)) => (throws IllegalArgumentException)
             (filter identity (map supported? (vals port-info))) => []
             (count (filter seq (map source-info (vals port-info)))) => 0
             (count (filter seq (map target-info (vals port-info)))) => 0
             (finally
               (close! noise-line)))))

  (facts "Audio format."
         (encoding noise-format) => (encoding :pcm-signed)
         (channels noise-format) => 1
         (frame-rate noise-format) => 48000.0
         (frame-size noise-format) => 2
         (sample-rate noise-format) => 48000.0
         (sample-size-bits noise-format) => 16
         (big-endian? noise-format) => false)

  (facts "Getting a line from a mixer."
         (try
           (filter identity (map (partial supported? mxr) (vals port-info))) => []
           (open! mxr)
           (filter (partial supported? mxr) (vals port-info)) => []
           (pos? (count (source-info mxr))) => true
           (pos? (count (target-info mxr))) => true
           (info (first (map line (target-info mxr))) :class) => "TargetDataLine"
           (map (comp line-class line) (source-info mxr)) => [SourceDataLine Clip]
           (count (source mxr)) => 0
           (let [l (line (first (source-info mxr)))]
             (pos? (long (available l))) => false
             (open! l) => l
             (open? l) => true
             (pos? (long (available l))) => true
             (encoding (audio-format l)) => (encoding :pcm-signed)
             (channels (audio-format l)) => 2
             (close! l) => l)
           (finally
             (close! mxr))))



  )
