(ns uncomplicate.clojure-sound.midi-test
  (:require [midje.sweet :refer [facts throws => =not=> roughly truthy]]
            [uncomplicate.commons.core :refer [close!]]
            [uncomplicate.clojure-sound
             [core :refer :all]
             [midi :refer :all]])
  (:import [javax.sound.midi MidiUnavailableException]))

(facts "Obtaining Default Devices."
       (myname (sequencer)) => "Real Time Sequencer"
       (myname (synthesizer)) => "Gervill"
       (close! (receiver)) => truthy
       (close! (transmitter)) => truthy
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
           (open! dev) => dev
           (open? dev) => true
           (close! dev) => dev
           (open? dev) => false)))

(facts "Transmitting and receiving MIDI messages."
       (let [msg (short-message)
             rcvr (receiver)]
         (try
           (message! msg :on 0 60 93) => msg
           (send! rcvr msg -1) => rcvr
           (number? (micro-position (device rcvr))) => true
           (send! rcvr msg) => rcvr
           (device rcvr) => (device rcvr)
           (finally (close! rcvr) => rcvr))))

(facts "Connecting transmitters and receivers."
       (let [sq (sequencer)
             tr (transmitter)
             synth (synthesizer)
             rc (receiver)]
         (try
           (connect! tr rc) => tr
           (synthesizer? sq) => (sequencer? synth)
           (finally
             (close! rc) => rc
             (close! synth) => synth
             (close! tr) => tr
             (close! sq) => sq))))

(facts "Connecting to more than one device."
       (let [synth (synthesizer)
             sq (sequencer)
             input (first (remove (comp zero? max-transmitters)
                                  (drop 2 (map device (device-info)))))
             tr1 (transmitter input)
             tr2 (transmitter input)
             rc-synth (receiver synth)
             rc-sq (receiver sq)]
         (try
           tr1 =not=> tr2
           (max-transmitters input) => -1
           (max-receivers input) => 0
           (connect! tr1 rc-synth) => tr1
           (connect! tr2 rc-sq) => tr2
           (synthesizer? sq) => (sequencer? synth)
           (finally
             (close! input) => input
             (close! sq) => sq
             (close! synth) => synth))))

(facts "Connecting to more than one device with automatic receivers and transmitters."
       (let [synth (synthesizer)
             sq (sequencer)
             input (first (remove (comp zero? max-transmitters)
                                  (drop 2 (map device (device-info)))))
             tr-count (count (transmitters input))]
         (try
           (max-transmitters input) => -1
           (let [tr1 (connect! input synth)
                 tr2 (connect! input sq)]
             (filter #{tr1 tr2} (transmitters input)) => [tr1 tr2]
             (- (count (transmitters input)) tr-count) => 2
             (close! tr1)
             (close! tr2)
             (count (transmitters input)) => tr-count
             (connect! input synth)
             (count (transmitters (close! input))) => 0
             (count (receivers synth)) => 2
             (count (receivers (close! synth))) => 0
             (count (receivers sq)) => 1
             (count (receivers (close! sq))) => 0)
           (finally
             (close! input) => input
             (close! sq) => sq
             (close! synth) => synth))))

(facts "Using Sequencer."
       (let [sqcr (sequencer)
             synth (synthesizer)
             maple (sequence (clojure.java.io/resource "maple.mid"))]
         ;;(connect! sqcr synth)
         (open! sqcr) => sqcr
         ;;(open! synth) => synth
         (sequence! sqcr maple) => sqcr
         (listen! sqcr #((when (= (mytype %2) :end-of-track)
                           (Thread/sleep 200)
                           (stop! sqcr)
                           (close! synth)
                           (close! sqcr))))
         (start! sqcr)
         (running? sqcr) => true))
