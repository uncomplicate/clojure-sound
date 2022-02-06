(ns uncomplicate.clojure-sound.midi
  (:refer-clojure :exclude [sequence])
  (:require [uncomplicate.commons.core :refer [Releaseable]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support]]
             [core :refer [write!]]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol MidiSystemProcedures
  (mfile-format [this])
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
