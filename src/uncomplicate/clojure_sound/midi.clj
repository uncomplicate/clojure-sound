(ns uncomplicate.clojure-sound.midi
  (:refer-clojure :exclude [sequence])
  (:require [uncomplicate.commons.core :refer [Releaseable]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support]]
             [core :refer [write! Info InfoProvider Open Timestamp Reset]]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol MidiSystemProcedures
  (file-format [this])
  (sequence [this])
  (soundbank [this]))

(extend-protocol MidiSystemProcedures
  File
  (file-format [file]
    (MidiSystem/getMidiFileFormat file))
  (sequence [file]
    (MidiSystem/getSequence file))
  (soundbank [file]
    (MidiSystem/getSoundbank file))
  InputStream
  (file-format [stream]
    (MidiSystem/getMidiFileFormat stream))
  (sequence [stream]
    (MidiSystem/getSequence stream))
  (soundbank [stream]
    (MidiSystem/getSoundbank stream))
  URL
  (file-format [url]
    (MidiSystem/getMidiFileFormat url))
  (sequence [url]
    (MidiSystem/getSequence url))
  (soundbank [url]
    (MidiSystem/getSoundbank url)))

(defn device-info []
  (MidiSystem/getMidiDeviceInfo))

(defn device [info]
  (MidiSystem/getMidiDevice info))

(defn file-types
  (^ints []
   (MidiSystem/getMidiFileTypes))
  (^ints [sequence]
   (MidiSystem/getMidiFileTypes sequence)))

(defn receiver []
  (MidiSystem/getReceiver))

(defn sequencer
  ([]
   (MidiSystem/getSequencer))
  ([connected?]
   (MidiSystem/getSequencer connected?)))

(defn synthesizer []
  (MidiSystem/getSynthesizer))

(defn transmitter []
  (MidiSystem/getTransmitter))

(extend-protocol Support
  Integer
  (supported [file-type]
    (MidiSystem/isFileTypeSupported file-type))
  Long
  (supported [file-type]
    (MidiSystem/isFileTypeSupported file-type))
  Short
  (supported [file-type]
    (MidiSystem/isFileTypeSupported file-type))
  Byte
  (supported [file-type]
    (MidiSystem/isFileTypeSupported file-type))
  Sequence
  (supported [sequence file-type]
    (MidiSystem/isFileTypeSupported file-type sequence)))

(defmethod write! [Sequence File]
  [in out! file-type]
  (MidiSystem/write ^Sequence in ^long file-type ^File out!))

(defmethod write! [Sequence OutputStream]
  [in out! file-type]
  (MidiSystem/write ^Sequence in ^long file-type ^OutputStream out!))

;; ============================= MidiDevice ================================

(extend-type MidiDevice$Info
  Info
  (description [info]
    (.getDescription info))
  (myname [info]
    (.getName info))
  (vendor [info]
    (.getVendor info))
  (version [info]
    (.getVersion info)))

(extend-type MidiDevice
  InfoProvider
  (info [device]
    (.getDeviceInfo device))
  Info
  (description [device]
    (.getDescription (.getDeviceInfo device)))
  (myname [device]
    (.getName (.getDeviceInfo device)))
  (vendor [device]
    (.getVendor (.getDeviceInfo device)))
  (version [device]
    (.getVersion (.getDeviceInfo device)))
  Open
  (open [device]
    (.open device))
  (.isOpen [device]
    (.isOpen device))
  Timestamp
  (ms-position [device]
    (.getMicrosecondPosition device)))

(defn ^long max-receivers [^MidiDevice device]
  (.getMaxReceivers device))

(defn ^long max-transmitters [^MidiDevice device]
  (.getMaxTransmitters device))

(defn receiver [^MidiDevice device]
  (.getReceiver device))

(defn receivers [^MidiDevice device]
  (.getReceivers device))

(defn transmitter [^MidiDevice device]
  (.getTransmitter device))

(defn transmitters [^MidiDevice device]
  (.getTransmitters device))

;; ============================= MidiChannel ================================

(extend-type MidiChannel
  Reset
  (re-set! [channel!]
    (.resetAllControllers channel!)
    channel!))

(defn off!
  ([^MidiChannel channel!]
   (.allNotesOff channel!)
   channel!)
  ([^MidiChannel channel! ^long note]
   (.noteOff channel! note)
   channel!)
  ([^MidiChannel channel! ^long note ^long velocity]
   (.noteOff channel! note velocity)
   channel!))

(defn on! [^MidiChannel channel! ^long note ^long velocity]
  (.noteOff channel! note velocity)
  channel!)

(defn sound-off [^MidiChannel channel!]
  (.allSoundOff channel!)
  channel!)

(defn pressure
  (^long [^MidiChannel channel!]
   (.getChannelPressure channel!))
  (^long [^MidiChannel channel! ^long note]
   (.getPolyPressure channel! note)))

(defn pressure! [^MidiChannel channel! ^long pressure]
  (.setChannelPressure channel! pressure)
  channel!)

(defn controller ^long [^MidiChannel channel! ^long controller]
  (.getController channel! controller))

(defn mono [^MidiChannel channel!]
  (.getMono channel!))

(defn mono!
  ([^MidiChannel channel!]
   (.setMono channel! true)
   channel!)
  ([^MidiChannel channel! on]
   (.setMono channel! on)
   channel!))

(defn mute [^MidiChannel channel!]
  (.getMute channel!))

(defn mute!
  ([^MidiChannel channel!]
   (.setMute channel! true)
   channel!)
  ([^MidiChannel channel! mute]
   (.setMute channel! mute)
   channel!))

(defn omni [^MidiChannel channel!]
  (.getOmni channel!))

(defn omni!
  ([^MidiChannel channel!]
   (.setOmni channel! true)
   channel!)
  ([^MidiChannel channel! on]
   (.setOmni channel! on)
   channel!))

(defn bend ^long [^MidiChannel channel!]
  (.getPitchBend channel!))

(defn bend! [^MidiChannel channel! bend]
  (.setPitchBend channel! bend)
  channel!)

(defn program ^long [^MidiChannel channel!]
  (.getProgram channel!))

(defn solo [^MidiChannel channel!]
  (.getSolo channel!))

(defn solo
  ([^MidiChannel channel!]
   (.setSolo channel! true)
   channel!)
  ([^MidiChannel channel! on]
   (.setSolo channel! on)
   channel!))

(defn control!
  ([^MidiChannel channel! on]
   (.localControl channel! on))
  ([^MidiChannel channel! ^long controller ^long val]
   (.controlChange channel! controller val)
   channel!))

(defn patch!
  ([^MidiChannel channel! ^long bank ^long program]
   (.programChange channel! bank program)
   channel!)
  ([^MidiChannel channel! ^long program]
   (.programChange channel! program)
   channel!))

(def program! patch!)

;; =================== User friendly printing ==========================================

(defmethod print-method MidiDevice$Info
  [info ^java.io.Writer w]
  (.write w (pr-str (bean info))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiDevice$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))
