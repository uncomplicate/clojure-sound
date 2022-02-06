(ns uncomplicate.clojure-sound.core
  (:require [uncomplicate.clojure-sound.internal :refer [supported]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol Info
  (description [this])
  (myname [this])
  (vendor [this])
  (version [this]))

(defprotocol InfoProvider
  (info [this]))

(defprotocol Open
  (open [this] [this buffer-size] [this format data offset buffer-size])
  (open? [this]))

(defprotocol Timestamp
  (ms-position [this]))

(defprotocol Reset
  (re-set! [this]))

(defn supported?
  ([feature]
   (supported feature))
  ([this feature]
   (supported feature this)))

(defmulti write! (partial mapv class))

(defmethod write! :default [& args]
  (throw (ex-info (format "Unsupported write request.") {:type :sound-error :args args })))
