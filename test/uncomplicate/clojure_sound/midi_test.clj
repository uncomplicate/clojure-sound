(ns uncomplicate.clojure-sound.midi-test
  (:require [midje.sweet :refer [facts throws => roughly truthy]]
            [uncomplicate.commons.core :refer [close]]
            [uncomplicate.clojure-sound
             [core :refer :all]
             [midi :refer :all]]))

(facts "Query Midi System for device information."
       (< 0 (count (device-info))) => true)

(let [gervill (first (filter (comp (partial = "Gervill") myname) (device-info)))]
  (facts "Test Gervill: Info."
         (description gervill) => "Software MIDI Synthesizer"
         (myname gervill) => "Gervill"
         (vendor gervill) => "OpenJDK"
         (<= 1.0 (Double/parseDouble (version gervill))) => true)

  (facts "Test Gervill: InfoProvider."
         (info gervill) => gervill)

  (facts "Test Gervill: MidiDevice."
         (device gervill) => truthy
         (let [dev (device gervill)]
           )))
