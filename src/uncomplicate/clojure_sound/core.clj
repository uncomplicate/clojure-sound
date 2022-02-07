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
  (open [this!] [this! buffer-size] [this! format data offset buffer-size])
  (open? [this!]))

(defprotocol Timestamp
  (ms-length [this])
  (ms-position [this])
  (ms-position! [this microseconds]))

(defprotocol Reset
  (re-set! [this!]))

(defprotocol Broadcast
  (listen! [this! listener] [this! listener params])
  (ignore! [this! listener]  [this! listener params]))

(defprotocol Activity
  (running? [this])
  (recording? [this])
  (start! [this])
  (stop! [this]))

(defprotocol Type
  (mytype [this]))

(defprotocol Format
  (get-format [this])
  (property [this key])
  (properties [this])
  (byte-length [this]))

(defn supported?
  ([feature]
   (supported feature))
  ([this feature]
   (supported feature this)))

(defmulti write! (partial mapv class))

(defmethod write! :default [& args]
  (throw (ex-info (format "Unsupported write request.") {:type :sound-error :args args})))

(defmethod print-method (Class/forName "[I")
  [arr ^java.io.Writer w]
  (.write w (pr-str (seq arr))))

(defmethod print-method (Class/forName "[B")
  [arr ^java.io.Writer w]
  (.write w (pr-str (seq arr))))

(defmethod print-method (Class/forName "[J")
  [arr ^java.io.Writer w]
  (.write w (pr-str (seq arr))))
