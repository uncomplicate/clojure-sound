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

(with-release [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))]
  (let [noise-format (audio-format noise)
        mxr (first (filter #(includes? (info % :name) "[default]") (mixer)))]

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
               (close! mxr))))))

(facts "Using a Clip."
       (with-release [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))
                      clip (line (line-info :clip (audio-format noise)))]
         (open? clip) => false
         (open! clip noise) => clip
         (level clip) => -1.0
         (active? clip) => false
         (start! clip) => clip
         (active? clip) => true
         (Thread/sleep 5)
         (stop! clip) => clip
         (close! clip) => clip))

(facts "Using a SourceDataLine."
       (with-release [src (line (line-info :source (audio-format 44100.0 16 2)))]
         (open? src) => false
         (open! src 767) => src
         (level src) => -1.0
         (running? src) => false
         (available src) => 764
         (start! src) => src
         (active? src) => false
         (running? src) => false
         (write! (byte-array (repeatedly 2756 (partial rand-int 77))) src 0)
         (active? src) => true
         (running? src) => true
         (stop! src) => src
         (running? src) => false
         (active? src) => false
         (start! src) => src
         (drain! src) => src
         (flush! src) => src
         (close! src)))

(facts "Monitoring a line's status."
       (with-release [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))
                      clip (line (line-info :clip (audio-format noise)))]
         (let [finished? (promise)]
           (listen! clip (partial deliver finished?) :stop)
           (open! clip noise) => clip
           (start! clip) => clip
           @finished?)))
