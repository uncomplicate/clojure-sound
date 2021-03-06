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
       (mixer) => (mixer nil)
       (mixer (first (filter #(includes? (info % :name) "[default]") (mixer-info)))) => (mixer))

(with-release [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))]
  (let [noise-format (audio-format noise)
        mxr (mixer)]

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
             (count (open-sources mxr)) => 0
             (let [l (line (first (source-info mxr)))]
               (pos? (long (available l))) => false
               (open! l) => l
               (count (open-sources mxr)) => 1
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

(facts "Synchronizing audio on multiple lines."
       (with-release [noise (audio-input-stream (clojure.java.io/resource "Noise.wav"))
                      noise-format (audio-format noise)
                      mxr (mixer)
                      clip1 (line mxr (line-info :clip noise-format))
                      clip2 (line mxr (line-info :clip noise-format))
                      finished? (promise)]
         (listen! clip1 (partial deliver finished?) :stop)
         (supported? mxr [clip1 clip2]) => false
         (sync-supported? mxr [clip1 clip2]) => false
         (sync! mxr [clip1 clip2]) => (throws IllegalArgumentException)
         (open! clip1 noise) => clip1
         (open! clip2 noise) => clip2
         (start! clip1) => clip1
         (start! clip2) => clip2
         @finished?))

(facts "Capturing audio."
       (with-release [mxr (mixer)
                      tgt (line mxr (line-info :target (audio-format 44100.0 16 2)))
                      src (line (line-info :source (audio-format 44100.0 16 2)))
                      finished? (promise)]
         (listen! tgt (partial deliver finished?) :stop)
         (open! tgt) => tgt
         (flush! tgt) => tgt
         (start! tgt) => tgt
         (read! tgt (byte-array 100)) => 100
         (read! tgt (byte-array 100) 1 3) => (throws IllegalArgumentException)
         (seq (doto (byte-array (repeat 8 99))
                (read! tgt 2 4))) => [99 99 0 0 0 0 99 99]
         (stop! tgt) => tgt
         (drain! tgt) => tgt
         @finished?))

(facts "Getting a line that has the desired controls."
       (with-release [mxr (open! (mixer))
                      sdl (open! (first (source mxr)))
                      gain-control (control sdl :master-gain)]
         (info gain-control :type) => :master-gain
         (info (map itype (control sdl)) :name) => [:master-gain :mute :balance :pan]
         (info (control sdl) :kind) => [:float :boolean :float :float]
         (value (control sdl :mute)) => false
         (value (value! (control sdl :mute) true)) => true
         (maximum gain-control) => (roughly 6 0.1)
         (max-label gain-control) => "Maximum"
         (minimum gain-control) => -80.0
         (min-label gain-control) => "Minimum"
         (units gain-control) => "dB"
         (precision gain-control) => 0.625
         (update-period gain-control) => -1
         (shift? gain-control) => false))

(facts "Getting a line that has the desired controls."
       (with-release [sdl (open! (first (source (line-info :source))))]
         (info (control sdl) :type) => [:master-gain :mute :balance :pan]))
