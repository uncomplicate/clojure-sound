;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-sound.core
  (:require [uncomplicate.clojure-sound.internal :refer [supported]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol SoundSystemProcedures
  (file-format [this]))

(defprotocol SoundInfoProvider
  (sound-info [this]))

(defprotocol Open
  (open! [this!] [this! buffer-size] [this! format data offset buffer-size])
  (open? [this!]))

(defprotocol Timing
  (resolution [this])
  (division [this])
  (micro-length [this])
  (micro-position [this])
  (micro-position! [this microseconds]))

(defprotocol Reset
  (re-set! [this!]))

(defprotocol Broadcast
  (listen! [this! listener] [this! listener params])
  (ignore! [this! listener]  [this! listener params]))

(defprotocol Activity
  (running? [this])
  (active? [this])
  (start! [this])
  (stop! [this]))

(defprotocol Type
  (itype [this]))

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

(defmulti write! (fn [& args] (mapv class args)))

(defmethod write! :default [& args]
  (throw (ex-info "Unsupported write request." {:type :sound-error :args args})))

(defmulti connect! (fn [& args] (mapv class args)))

(defmethod connect! :default [& args]
  (throw (ex-info "Unsupported connection between these endpoints." {:type :sound-error :args args})))

(defmethod print-method (Class/forName "[I")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[B")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[J")
  [this w]
  (print-method (seq this) w))
