(ns uncomplicate.clojure-sound.midi-test
  (:require [midje.sweet :refer [facts throws => =not=> roughly truthy]]
            [uncomplicate.commons.core :refer [close! info]]
            [uncomplicate.clojure-sound
             [core :refer :all]
             [midi :refer :all]])
  (:import [javax.sound.midi MidiUnavailableException]))

(facts "Obtaining Default Devices."
       (info (sequencer) :name) => "Real Time Sequencer"
       (info (synthesizer) :name) => "Gervill"
       (close! (receiver)) => truthy
       (close! (transmitter)) => truthy
       (<= 2 (count (device-info))) => true
       (< 0 (count (filter (comp sequencer? device) (device-info)))) => true
       (< 0 (count (filter (comp synthesizer? device) (device-info)))) => true)

(let [gervill (first (filter #(= "Gervill" (info % :name)) (device-info)))]
  (facts "Test Gervill: Info."
         (info gervill :description) => "Software MIDI Synthesizer"
         (info gervill :name) => "Gervill"
         (info gervill :vendor) => "OpenJDK"
         (<= 1.0 (Double/parseDouble (info gervill :version))) => true)

  (facts "Test Gervill: InfoProvider."
         (sound-info gervill) => gervill)

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
             maple (sequence (clojure.java.io/resource "maple.mid"))
             cleanup (promise)]
         (try
           (info (device (receiver (first (transmitters (sequencer))))) :name) => "Gervill"
           (open! sqcr) => sqcr
           (sequence! sqcr maple) => sqcr
           (listen! sqcr (meta-listener :end-of-track
                                        (fn [_]
                                          (stop! sqcr)
                                          (close! sqcr) ;; Auto-close
                                          (deliver cleanup :confirmed))))
           (start! sqcr) => sqcr
           (stop! sqcr) => sqcr
           (running? sqcr) => false
           (start! sqcr) => sqcr
           (running? sqcr) => true
           (deref cleanup) => :confirmed
           (finally ;; So I can interrupt the music while testing...
             (close! sqcr)))))

;;TODO
#_(facts "Recording and saving sequences."
         (let [player (sequencer)
               maple (sequence (clojure.java.io/resource "maple.mid"))
               synth (synthesizer)
               recorder (sequencer)
               copy (sequence :ppq (resolution maple) (tracks maple))]
           (try
             (sequence! player maple) => player
             (sequence! recorder copy) => recorder
             (dotimes [i (tracks recorder)]
               (rec! recorder  i))
             (open! recorder) => recorder
             (open! player) => player
             (connect! player recorder)
             (rec! recorder) => recorder
             (recording? recorder) => true
             (start! player) => player
             (Thread/sleep 2000)
             (dotimes [i 16]
               (mute! player i true))
             (stop! recorder)
             (stop-rec! recorder) => recorder
             (doseq [t (tracks copy)]
               (stop-rec! recorder t))
             ;;(write! recorder (clojure.java.io/resource "maple-copy.mid") 0)
             (stop! player) => player
             (println "now")
             (sequence! recorder)
             (connect! recorder synth)
             (open! synth) => synth
             (open? recorder) => true
             (println ">" (tick-position recorder))
             (println (start-point recorder))
             (println (end-point recorder))
             (println (loop-count recorder))
             (tick-position! recorder 0)
             (start-point! recorder 0)
             (end-point! recorder (ticks recorder))
             (loop-count! recorder -1)
             (println (map event-count (tracks copy)))
             (start! recorder) => recorder
             (running? recorder) => true
             (Thread/sleep 2000)
             (finally
               (close! synth)
               (close! player)
               (close! recorder)))))

(let [sqcr (sequencer)
      maple (sequence (clojure.java.io/resource "maple.mid"))
      maple2 (sequence :ppq (resolution maple))
      finished? (promise)]
  (try
    (facts "Editing a Sequence."
           (open! sqcr) => sqcr
           (let [t2 (track! maple2)
                 t1 (first (tracks maple))]
             (String. ^bytes (data (message (event t1 0)))) => "control track"
             (events t1) => 6
             (take 3 (map #(String. ^bytes (data (message (event t1 %1)))) (range (events t1))))
             => ["control track" "creator: " "GNU LilyPond 2.14.2           "]
             (delete! maple2 t2) => true)
           (map events (tracks maple)) => [6 2391 2751]
           (map ticks (tracks maple)) => [0 110592 110592]
           (doseq [t (tracks maple)]
             (let [t2 (track! maple2)]
               (dotimes [i (events t)]
                 (add! t2 (event t i)))))
           (map ticks (tracks maple2)) => [0 110592 110592]
           (sequence! sqcr maple2) => sqcr)
    (facts "Using advanced Sequencer features."
           (tick-position sqcr) => 0
           (micro-position sqcr) => 0
           (tick-position! sqcr 110092) => sqcr
           (micro-position sqcr) => 143348958
           (tempo-factor sqcr) => 1.0
           (tempo-factor! sqcr 3.0) => sqcr
           (tempo-bpm sqcr) => 120.0
           (tempo-mpq sqcr) => 500000.0
           (tempo-bpm! sqcr 360) => sqcr
           (tempo-mpq sqcr) => 166666.671875
           (mute sqcr 1) => false
           (solo sqcr 1) => false
           (mute! sqcr 2) => sqcr
           (tempo-factor! sqcr 1.0) => sqcr
           (tempo-bpm! sqcr 120) => sqcr
           (listen! sqcr (partial deliver finished?) :end-of-track)
           (start! sqcr)
           (deref finished?) => truthy
           (tick-position sqcr) => 110592
           (micro-position! sqcr 0) => sqcr
           (tick-position sqcr) => 0)
    (finally
      (Thread/sleep 1000)
      (stop! sqcr)
      (close! sqcr))))

(let [synth (synthesizer)
      sqcr (sequencer)
      fluid (soundbank (clojure.java.io/resource "FluidR3_GM.sf2"));; Note: This soundbank is freely available on the internet. Please google it.
      maple (sequence (clojure.java.io/resource "maple.mid"))
      finished? (promise)]
  (try
    (facts "Managing instruments and sound banks."
           (count (instruments synth)) => 0
           (info (soundbank synth) :name) => "Emergency GM sound set"
           (count (available synth)) => 129
           (load! synth (first (available synth))) => false
           (open! synth)
           (load! synth (first (available synth))) => true
           (load! synth (available synth))
           (count (instruments synth)) => 129
           (unload! synth) => synth
           (count (instruments synth)) => 0
           (load! synth) => true
           (unload! synth) => synth
           (info fluid :name) => "Fluid R3 GM"
           (load! synth fluid) => true
           (count (instruments synth)) => 189
           (open! sqcr)
           (sequence! sqcr maple)
           (open! synth)
           (connect! sqcr synth)
           (tick-position! sqcr 110092)
           (listen! sqcr (partial deliver finished?) :end-of-track)
           (start! sqcr)
           @finished?)
    (facts "Querying the synthesizer's capabilities and current state."
           (latency synth) => 120000
           (max-polyphony synth) => 64
           (count (voice-status synth)) => 64)
    (facts "Using channels."
           (send! (receiver synth) (short-message :on 4 60 93))
           (Thread/sleep 1000)
           (on! (first (drop 4 (channels synth))) 60 93)
           (Thread/sleep 1000))
    (finally
      (Thread/sleep 1000)
      (close! sqcr)
      (close! synth))))
