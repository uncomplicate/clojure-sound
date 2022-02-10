(ns uncomplicate.clojure-sound.midi-test
  (:require [midje.sweet :refer [facts throws => roughly truthy]]
            [uncomplicate.commons.core :refer [close]]
            [uncomplicate.clojure-sound
             [core :refer :all]
             [midi :refer :all]])
  (:import [javax.sound.midi MidiUnavailableException]))

(facts "Obtaining Default Devices."
       (myname (sequencer)) => "Real Time Sequencer"
       (myname (synthesizer)) => "Gervill"
       (receiver) => (throws MidiUnavailableException)
       (transmitter) => (throws MidiUnavailableException)
       (<= 2 (count (device-info))) => true
       (< 0 (count (filter (comp sequencer? device) (device-info)))) => true
       (< 0 (count (filter (comp synthesizer? device) (device-info)))) => true)

(let [gervill (first (filter (comp (partial = "Gervill") myname) (device-info)))]
  (facts "Test Gervill: Info."
         (description gervill) => "Software MIDI Synthesizer"
         (myname gervill) => "Gervill"
         (vendor gervill) => "OpenJDK"
         (<= 1.0 (Double/parseDouble (version gervill))) => true)

  (facts "Test Gervill: InfoProvider."
         (info gervill) => gervill)

  (facts "Test Gervill: Opening Devices."
         (device gervill) => truthy
         (let [dev (device gervill)]
           (sequencer? dev) => false
           (synthesizer? dev) => true
           (max-polyphony dev) => 64
           (open? dev) => false
           (open dev) => dev
           (open? dev) => true
           (close dev) => dev
           (open? dev) => false)))
