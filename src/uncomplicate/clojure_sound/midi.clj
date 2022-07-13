;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-sound.midi
  (:refer-clojure :exclude [sequence])
  (:require [clojure
             [string :refer [trim join]]
             [walk :refer [keywordize-keys]]]
            [uncomplicate.commons.core :refer [Releaseable Closeable close! Info info]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support SequenceSource set-sequence get-sequence
                               Load load-instruments unload-instruments simple-name key-name
                               ReceiverProvider get-receiver extend-array-info]]
             [core :refer [write! SoundInfoProvider Open Timing Reset Broadcast Activity Type
                           Format active? connect! micro-length division resolution
                           properties property itype SoundSystemProcedures Available Channels]]])
  (:import [clojure.lang ILookup IFn]
           java.lang.reflect.Field
           java.net.URL
           java.util.Arrays
           [java.io File InputStream OutputStream]
           [java.nio ByteBuffer ByteOrder]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol Instruments
  (instruments [this]))

(defprotocol Program
  (program [this]))

(defprotocol Data
  (data [this])
  (message! [message arg] [message arg1 arg2 arg3] [message command channel data1 data2]))

(defprotocol Tick
  (ticks [this]))

(defprotocol Event
  (event [this arg]))

(defprotocol Code
  (encode [this])
  (decode [this]))

;; ===================== Keyword coding ================================================

(def sync-mode
  {:internal Sequencer$SyncMode/INTERNAL_CLOCK
   :midi-sync Sequencer$SyncMode/MIDI_SYNC
   :midi-time-code Sequencer$SyncMode/MIDI_TIME_CODE
   :no-sync Sequencer$SyncMode/NO_SYNC})

(def sync-mode-key
  {Sequencer$SyncMode/INTERNAL_CLOCK :internal
   Sequencer$SyncMode/MIDI_SYNC :midi-sync
   Sequencer$SyncMode/MIDI_TIME_CODE :midi-time-code
   Sequencer$SyncMode/NO_SYNC :no-sync})

(def ^:const timing-type
  {:ppq Sequence/PPQ
   :smpte24 Sequence/SMPTE_24
   :smpte25 Sequence/SMPTE_25
   :smpte30 Sequence/SMPTE_30
   :smpte30drop Sequence/SMPTE_30DROP})

(def ^:const timing-type-key
  {Sequence/PPQ :ppq
   Sequence/SMPTE_24 :smpte24
   Sequence/SMPTE_25 :smpte25
   Sequence/SMPTE_30 :smpte30
   Sequence/SMPTE_30DROP :smpte30drop})

(def ^:const command-type
  {:active-sensing ShortMessage/ACTIVE_SENSING
   :channel-pressure ShortMessage/CHANNEL_PRESSURE
   :continue ShortMessage/CONTINUE
   :control-change ShortMessage/CONTROL_CHANGE
   :end-of-exclusive ShortMessage/END_OF_EXCLUSIVE
   :time-code ShortMessage/MIDI_TIME_CODE
   :off ShortMessage/NOTE_OFF
   :on ShortMessage/NOTE_ON
   :bend ShortMessage/PITCH_BEND
   :poly-pressure ShortMessage/POLY_PRESSURE
   :program-change ShortMessage/PROGRAM_CHANGE
   :song-position ShortMessage/SONG_POSITION_POINTER
   :song-select ShortMessage/SONG_SELECT
   :start ShortMessage/START
   :stop ShortMessage/STOP
   :reset ShortMessage/SYSTEM_RESET
   :clock ShortMessage/TIMING_CLOCK
   :tune ShortMessage/TUNE_REQUEST
   :special-system-exclusive SysexMessage/SPECIAL_SYSTEM_EXCLUSIVE
   :system-exclusive SysexMessage/SYSTEM_EXCLUSIVE})

(def ^:const command-type-key
  {ShortMessage/ACTIVE_SENSING :active-sensing
   ShortMessage/CHANNEL_PRESSURE :channel-pressure
   ShortMessage/CONTINUE :continue
   ShortMessage/CONTROL_CHANGE :control-change
   ShortMessage/END_OF_EXCLUSIVE :end-of-exclusive
   ShortMessage/MIDI_TIME_CODE :time-code
   ShortMessage/NOTE_OFF :off
   ShortMessage/NOTE_ON :on
   ShortMessage/PITCH_BEND :bend
   ShortMessage/POLY_PRESSURE :poly-pressure
   ShortMessage/PROGRAM_CHANGE :program-change
   ShortMessage/SONG_POSITION_POINTER :song-position
   ShortMessage/SONG_SELECT :song-select
   ShortMessage/START :start
   ShortMessage/STOP :stop
   ShortMessage/SYSTEM_RESET :reset
   ShortMessage/TIMING_CLOCK :clock
   ShortMessage/TUNE_REQUEST :tune})

(def ^:const controller-type
  {0 :bank
   1 :modulation-wheel
   2 :breath-control
   4 :foot-control
   5 :portamento-time
   6 :data-entry
   7 :volume
   8 :balance
   10 :pan
   11 :expression-control
   12 :effect-control-1
   13 :effect-control-2
   16 :gp-control-1
   17 :gp-control-2
   18 :gp-control-3
   19 :gp-control-4
   32 :bank
   33 :modulation-wheel
   34 :breath-control
   36 :foot-control
   37 :portamento-time
   38 :data-entry
   39 :volume
   40 :balance
   42 :pan
   43 :expression-control
   44 :effect-control-1
   45 :effect-control-2
   48 :gp-control-1
   49 :gp-control-2
   50 :gp-control-3
   51 :gp-control-4
   64 :sustain
   65 :portamento
   66 :sostenuto
   67 :soft-pedal
   68 :legato-footswitch
   69 :hold-2
   70 :sc-sound-variation
   71 :sc-timbre
   72 :sc-release-time
   73 :sc-attack-time
   74 :sc-brightness
   75 :sc-6
   76 :sc-7
   77 :sc-8
   78 :sc-9
   79 :sc-10
   80 :gp-control-5
   81 :gp-control-6
   82 :gp-control-7
   83 :gp-control-8
   84 :portamento-control
   91 :effects-1-depth
   92 :effects-2-depth
   93 :effects-3-depth
   94 :effects-4-depth
   95 :effects-5-depth
   96 :data-inc
   97 :data-dec
   98 :non-registered-param-number-lsb
   99 :non-registered-param-number-msb
   100 :registered-param-number-lsb
   101 :registered-param-number-msb
   120 :all-sound-off
   121 :reset-all-controllers
   122 :local-controll-on-off
   123 :all-notes-off
   124 :omni-off
   125 :omni-on
   126 :poly-on-off
   127 :poly-on})

(def ^:const sysex-status-key
  {SysexMessage/SPECIAL_SYSTEM_EXCLUSIVE :special-system-exclusive
   SysexMessage/SYSTEM_EXCLUSIVE :system-exclusive})

(def ^:const meta-message-type
  {:sequence-number 0x00
   :text 0x01
   :copyright-notice 0x02
   :track-name 0x03
   :instrument-name 0x04
   :lyrics 0x05
   :marker 0x06
   :cue-point 0x07
   :channel-prefix 0x20
   :end-of-track 0x2F
   :set-tempo 0x51
   :smpte-offset 0x54
   :time-signature 0x58
   :key-signature 0x59
   :sequencer-specific 0x7F})

(def ^:const meta-message-type-key
  {0x00 :sequence-number
   0x01 :text
   0x02 :copyright-notice
   0x03 :track-name
   0x04 :instrument-name
   0x05 :lyrics
   0x06 :marker
   0x07 :cue-point
   0x20 :channel-prefix
   0x2F :end-of-track
   0x51 :set-tempo
   0x54 :smpte-offset
   0x58 :time-signature
   0x59 :key-signature
   0x7F :sequencer-specific})

(def ^:const midi-file-type
  {:single 0
   :multi 1
   :collection 2})

(def ^:const midi-file-type-key
  {0 :single
   1 :multi
   2 :collection})

;; =========================== MidiSystem ====================================

(defprotocol MidiSystemProcedures
  (soundbank [this])
  (device [this]))

(declare sequence tracks)

(extend-protocol SequenceSource
  File
  (get-sequence [file]
    (MidiSystem/getSequence file))
  InputStream
  (get-sequence [stream]
    (MidiSystem/getSequence stream))
  URL
  (get-sequence [url]
    (MidiSystem/getSequence url))
  Sequencer
  (get-sequence [sequencer]
    (.getSequence sequencer))
  Sequence
  (get-sequence [sq]
    (sequence (division sq) (resolution sq) (tracks sq)))
  nil
  (ge-sequence [_]
    (throw (ex-info "nil is not allowed as a sequence source."))))

(extend-protocol SoundSystemProcedures
  File
  (file-format [file]
    (MidiSystem/getMidiFileFormat file))
  InputStream
  (file-format [stream]
    (MidiSystem/getMidiFileFormat stream))
  URL
  (file-format [url]
    (MidiSystem/getMidiFileFormat url)))

(extend-protocol MidiSystemProcedures
  File
  (soundbank [file]
    (MidiSystem/getSoundbank file))
  InputStream
  (file-format [stream]
    (MidiSystem/getMidiFileFormat stream))
  (soundbank [stream]
    (MidiSystem/getSoundbank stream))
  URL
  (soundbank [url]
    (MidiSystem/getSoundbank url))
  MidiDeviceReceiver
  (device [receiver]
    (.getMidiDevice receiver))
  MidiDeviceTransmitter
  (device [transmitter]
    (.getMidiDevice transmitter))
  MidiDevice$Info
  (device [info]
    (MidiSystem/getMidiDevice info))
  Synthesizer
  (soundbank [synth]
    (.getDefaultSoundbank synth))
  SoundbankResource
  (soundbank [resource]
    (.getSoundbank resource)))

(defn device-info
  ([]
   (MidiSystem/getMidiDeviceInfo))
  ([^MidiDevice device]
   (.getDeviceInfo device)))

(defn file-types
  (^ints []
   (MidiSystem/getMidiFileTypes))
  (^ints [sequence]
   (MidiSystem/getMidiFileTypes sequence)))

(defn receiver
  ([]
   (MidiSystem/getReceiver))
  ([provider]
   (get-receiver provider)))

(defn sequencer
  ([]
   (MidiSystem/getSequencer))
  ([connected?]
   (MidiSystem/getSequencer connected?)))

(defn sequencer? [device]
  (instance? Sequencer device))

(defn synthesizer []
  (MidiSystem/getSynthesizer))

(defn synthesizer? [device]
  (instance? Synthesizer device))

(defn transmitter
  ([]
   (MidiSystem/getTransmitter))
  ([^MidiDevice device]
    (.getTransmitter device)))

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
  (info
    ([this]
     {:description (.getDescription this)
      :name (.getName this)
      :vendor (.getVendor this)
      :version (.getVersion this)})
    ([this info-type]
     (case info-type
       :description (.getDescription this)
       :name (.getName this)
       :vendor (.getVendor this)
       :version (.getVersion this)
       nil)))
  SoundInfoProvider
  (sound-info [info]
    info))

(defn ^long max-receivers [^MidiDevice device]
  (.getMaxReceivers device))

(defn ^long max-transmitters [^MidiDevice device]
  (.getMaxTransmitters device))

(defn receiver? [device]
  (not= 0 (max-receivers device)))

(defn transmitter? [device]
  (not= 0 (max-transmitters device)))

(defn receivers [^MidiDevice device]
  (.getReceivers device))

(defn transmitters [^MidiDevice device]
  (.getTransmitters device))

(extend-type MidiDevice
  Info
  (info
    ([this]
     (merge {:class (simple-name (class this))
             :status (if (.isOpen this) :open :closed)
             :micro-position (.getMicrosecondPosition this)}
            (info (.getDeviceInfo this))))
    ([this info-type]
     (case info-type
       :class (simple-name (class this))
       :status (if (.isOpen this) :open :closed)
       :micro-position (.getMicrosecondPosition this)
       (info (.getDeviceInfo this) info-type))))
  Releaseable
  (release [this]
    (close! this)
    true)
  SoundInfoProvider
  (sound-info [device]
    (.getDeviceInfo device))
  ReceiverProvider
  (get-receiver [device]
    (.getReceiver device))
  Open
  (open! [device]
    (.open device)
    device)
  (open? [device]
    (.isOpen device))
  Closeable
  (close! [device]
    (close! (transmitters device))
    (close! (receivers device))
    (.close device)
    device)
  Timing
  (micro-position [device]
    (.getMicrosecondPosition device)))

;; ============================= MidiChannel ================================

(extend-type MidiChannel
  Info
  (info
    ([this]
     {:mono (.getMono this)
      :mute (.getMute this)
      :omni (.getOmni this)
      :solo (.getSolo this)
      :program (.getProgram this)})
    ([this info-type]
     (case info-type
       :mono (.getMono this)
       :mute (.getMute this)
       :omni (.getOmni this)
       :solo (.getSolo this)
       :program (.getProgram this)
       nil)))
  Reset
  (re-set! [channel!]
    (.resetAllControllers channel!)
    channel!)
  Program
  (program [channel]
    (.getProgram channel)))

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
  (.noteOn channel! note velocity)
  channel!)

(defn sound-off! [^MidiChannel channel!]
  (.allSoundOff channel!)
  channel!)

(defn pressure
  (^long [^MidiChannel channel]
   (.getChannelPressure channel))
  (^long [^MidiChannel channel ^long note]
   (.getPolyPressure channel note)))

(defn pressure! [^MidiChannel channel! ^long pressure]
  (.setChannelPressure channel! pressure)
  channel!)

(defn controller ^long [^MidiChannel channel controller]
  (.getController channel (get controller-type controller controller)))

(defn mono [^MidiChannel channel]
  (.getMono channel))

(defn mono!
  ([^MidiChannel channel!]
   (.setMono channel! (not (.getMono channel!)))
   channel!)
  ([^MidiChannel channel! on]
   (.setMono channel! on)
   channel!))

(defn mute
  ([^MidiChannel channel]
   (.getMute channel))
  ([^Sequencer sequencer track]
   (.getTrackMute sequencer track)))

(defn mute!
  ([^MidiChannel channel!]
   (.setMute channel! (not (.getMute channel!)))
   channel!)
  ([this! arg]
   (if (instance? MidiChannel this!)
     (.setMute ^MidiChannel this! arg)
     (.setTrackMute ^Sequencer this! arg (not (.getTrackMute ^Sequencer this! arg))))
   this!)
  ([^Sequencer sequencer! track mute]
   (.setTrackMute sequencer! track mute)
   sequencer!))

(defn omni [^MidiChannel channel]
  (.getOmni channel))

(defn omni!
  ([^MidiChannel channel!]
   (.setOmni channel! (not (.getOmni channel!)))
   channel!)
  ([^MidiChannel channel! on]
   (.setOmni channel! on)
   channel!))

(defn bend ^long [^MidiChannel channel]
  (.getPitchBend channel))

(defn bend!
  ([^MidiChannel channel!]
   (.setPitchBend channel! (not (.getPitchBend channel!)))
   channel!)
  ([^MidiChannel channel! bend]
   (.setPitchBend channel! bend)
   channel!))

(defn solo
  ([^MidiChannel channel]
   (.getSolo channel))
  ([^Sequencer sequencer track]
   (.getTrackSolo sequencer track)))

(defn solo!
  ([^MidiChannel channel!]
   (.setSolo channel! (not (.getSolo channel!)))
   channel!)
  ([this! arg]
   (if (instance? MidiChannel this!)
     (.setSolo ^MidiChannel this! arg)
     (.setTrackSolo ^Sequencer this! arg (not (.getTrackSolo ^Sequencer this! arg))))
   this!)
  ([^Sequencer sequencer! track solo]
   (.setTrackSolo sequencer! track solo)
   sequencer!))

(defn control!
  ([^MidiChannel channel! on]
   (.localControl channel! on))
  ([^MidiChannel channel! controller ^long val]
   (.controlChange channel! (get controller-type controller controller) val)
   channel!))

(defn program!
  ([^MidiChannel channel! ^long bank ^long program]
   (.programChange channel! bank program)
   channel!)
  ([^MidiChannel channel! ^long program]
   (.programChange channel! program)
   channel!))

;; ============================= Receiver ================================

(extend-type Receiver
  Info
  (info
    ([this]
     {:class (simple-name (class this))
      :id (System/identityHashCode this)})
    ([this info-type]
     (case info-type
       :class (simple-name (class this))
       :id (System/identityHashCode this)
       nil)))
  Releaseable
  (release [this]
    (close! this)
    true))

(deftype ReceiverFunction [f]
  Receiver
  (send [_ message timestamp]
    (f message timestamp)))

(extend-type IFn
  ReceiverProvider
  (get-receiver [f]
    (->ReceiverFunction f)))

(defn send!
  ([^Receiver receiver! ^MidiMessage message ^long timestamp]
   (.send receiver! message timestamp)
   receiver!)
  ([^Receiver receiver! ^MidiMessage message]
   (send! receiver! message -1)))

;; ============================= Transmitter ================================

(extend-type Transmitter
  Info
  (info
    ([this]
     {:class (simple-name (class this))
      :id (System/identityHashCode this)})
    ([this info-type]
     (case info-type
       :class (simple-name (class this))
       :id (System/identityHashCode this)
       nil)))
  ReceiverProvider
  (get-receiver [transmitter]
    (.getReceiver transmitter))
  Releaseable
  (release [this]
    (close! this)
    true))

(defmethod connect! [Transmitter Receiver]
  [^Transmitter transmitter! receiver]
  (.setReceiver transmitter! receiver)
  transmitter!)

(defmethod connect! [MidiDevice Receiver]
  [device receiver]
  (let [tr (transmitter device)]
    (.setReceiver ^Transmitter tr receiver)
    tr))

(defmethod connect! [Transmitter MidiDevice]
  [^Transmitter transmitter! device]
  (let [rc (receiver device)]
    (.setReceiver transmitter! rc)
    transmitter!))

(defmethod connect! [MidiDevice MidiDevice]
  [in out]
  (let [tr (transmitter in)
        rc (receiver out)]
    (.setReceiver ^Transmitter tr rc)
    tr))

;; ============================= Sequencer ================================

(deftype MetaEventListenerFunction [f ^int meta-type]
  Info
  (info [_]
    {:fn f
     :type (get meta-message-type-key meta-type meta-type)})
  (info [_ info-type]
    (case info-type
      :fn f
      :type (get meta-message-type-key meta-type meta-type)
      nil))
  MetaEventListener
  (meta [_ message]
    (when (or (< meta-type 0) (= meta-type (.getType message)))
      (f message))))

(deftype MetaEventListenerWrapper [^MetaEventListener f ^int meta-type]
  Info
  (info [_]
    {:fn f
     :type (get meta-message-type-key meta-type meta-type)})
  (info [_ info-type]
    (case info-type
      :fn f
      :type (get meta-message-type-key meta-type meta-type)
      nil))
  MetaEventListener
  (meta [_ message]
    (when (or (< meta-type 0) (= meta-type (.getType message)))
      (.meta f message))))

(defn meta-listener
  ([meta-type f]
   (let [meta-type (get meta-message-type meta-type meta-type)]
     (if (instance? MetaEventListener f)
       (->MetaEventListenerWrapper f meta-type)
       (->MetaEventListenerFunction f meta-type))))
  ([f]
   (if (instance? MetaEventListener f)
     f
     (meta-listener -1 f))))

(deftype ControllerEventListenerFunction [f]
  Info
  (info [_]
    {:fn f})
  (info [_ info-type]
    (case info-type
      :fn f
      nil))
  ControllerEventListener
  (controlChange [_ message]
    (f message)))

(defn ctrl-listener [f]
  (if (instance? ControllerEventListener f)
    f
    (->ControllerEventListenerFunction f)))

(extend-type Sequencer
  Broadcast
  (listen!
    ([sequencer! listener]
     (let [listener (if (instance? MetaEventListener listener)
                      listener
                      (meta-listener listener))]
       (.addMetaEventListener sequencer! listener)
       listener))
    ([sequencer! listener selection]
     (if (or (number? selection) (keyword? selection))
       (let [listener (meta-listener selection listener)]
         (.addMetaEventListener sequencer! listener)
         listener)
       (let [listener (if (instance? ControllerEventListener listener)
                        listener
                        (ctrl-listener listener))]
         (.addControllerEventListener sequencer! listener
                                      (if (sequential? selection)
                                        (int-array selection)
                                        selection))
         listener))))
  (ignore!
    ([sequencer! listener]
     (.removeMetaEventListener sequencer! listener)
     sequencer!)
    ([sequencer! listener controllers]
     (.removeControllerEventListener sequencer! listener
                                     (if (sequential? controllers)
                                       (int-array (map (fn [c]
                                                         (get controller-type c c))
                                                       controllers))
                                       controllers))))
  Timing
  (micro-length [sequencer]
    (.getMicrosecondLength sequencer))
  (micro-position [sequencer]
    (.getMicrosecondPosition sequencer))
  (micro-position! [sequencer! microseconds]
    (.setMicrosecondPosition sequencer! microseconds)
    sequencer!)
  Activity
  (running? [sequencer]
    (.isRunning sequencer))
  (start! [sequencer!]
    (.start sequencer!)
    sequencer!)
  (stop! [sequencer!]
    (.stop sequencer!)
    sequencer!)
  Tick
  (ticks [sequencer]
    (.getTickLength sequencer)))

(extend-protocol SequenceSource
  InputStream
  (set-sequence [stream sequencer!]
    (.setSequence ^Sequencer sequencer! stream))
  Sequence
  (set-sequence [s sequencer!]
    (.setSequence ^Sequencer sequencer! s)))

(defn sequence!
  ([^Sequencer sequencer!]
   (.setSequence sequencer! (.getSequence sequencer!))
   sequencer!)
  ([sequencer! source]
   (set-sequence source sequencer!)
   sequencer!))

(defn loop-count ^long [^Sequencer sequencer]
  (.getLoopCount sequencer))

(defn loop-count! [^Sequencer sequencer! count]
  (.setLoopCount sequencer! count)
  sequencer!)

(defn end-point ^long [^Sequencer sequencer]
  (.getLoopEndPoint sequencer))

(defn end-point! [^Sequencer sequencer! tick]
  (.setLoopEndPoint sequencer! tick)
  sequencer!)

(defn start-point ^long [^Sequencer sequencer]
  (.getLoopStartPoint sequencer))

(defn start-point! [^Sequencer sequencer! tick]
  (.setLoopStartPoint sequencer! tick)
  sequencer!)

(defn master-sync [^Sequencer sequencer]
  (get sync-mode-key (.getMasterSyncMode sequencer)
       (throw (ex-info "Unknown sync mode." {:type :sound-error
                                             :mode (.getMasterSyncMode sequencer)
                                             :supported (keys sync-mode-key)}))))

(defn master-sync! [^Sequencer sequencer! sync]
  (.setMasterSyncMode sequencer! (get sync-mode sync sync))
  sequencer!)

(defn master-modes [^Sequencer sequencer]
  (map sync-mode-key (.getMasterSyncModes sequencer)))

(defn slave-sync [^Sequencer sequencer]
  (get sync-mode-key (.getSlaveSyncMode sequencer)
       (throw (ex-info "Unknown sync mode." {:type :sound-error
                                             :mode (.getSlaveSyncMode sequencer)
                                             :supported (keys sync-mode-key)}))))

(defn slave-sync! [^Sequencer sequencer! sync]
  (.setSlaveSyncMode sequencer! (get sync-mode sync sync))
  sequencer!)

(defn slave-modes [^Sequencer sequencer]
  (get sync-mode-key (.getSlaveSyncModes sequencer)))

(defn tempo-factor ^double [^Sequencer sequencer]
  (.getTempoFactor sequencer))

(defn tempo-factor! [^Sequencer sequencer! factor]
  (.setTempoFactor sequencer! factor)
  sequencer!)

(defn tempo-bpm ^double [^Sequencer sequencer]
  (.getTempoInBPM sequencer))

(defn tempo-bpm! [^Sequencer sequencer! bpm]
  (.setTempoInBPM sequencer! bpm)
  sequencer!)

(defn tempo-mpq ^double [^Sequencer sequencer]
  (.getTempoInMPQ sequencer))

(defn tempo-mpq! [^Sequencer sequencer! mpq]
  (.setTempoInMPQ sequencer! mpq)
  sequencer!)

(defn tick-position ^long [^Sequencer sequencer]
  (.getTickPosition sequencer))

(defn tick-position! [^Sequencer sequencer! position]
  (.setTickPosition sequencer! position)
  sequencer!)

(defn recording? [^Sequencer sequencer]
  (.isRecording sequencer))

(defn rec!
  ([^Sequencer sequencer!]
   (.startRecording sequencer!)
   sequencer!)
  ([^Sequencer sequencer! track channel]
   (.recordEnable sequencer! track channel)
   sequencer!))

(defn stop-rec!
  ([^Sequencer sequencer!]
   (.stopRecording sequencer!)
   sequencer!)
  ([^Sequencer sequencer! track]
    (.recordDisable sequencer! track)
    sequencer!))

;; =================== Synthesizer =====================================================

(extend-type Synthesizer
  Instruments
  (instruments [synth]
    (.getLoadedInstruments synth))
  Load
  (load-instruments [source synth!]
    (every? identity (map #(.loadInstrument ^Synthesizer synth! %)
                          (.getAvailableInstruments source))))
  (unload-instruments [source synth!]
    (let [instruments (.getLoadedInstruments source)]
      (dotimes [i (alength instruments)]
        (.unloadInstrument ^Synthesizer synth! (aget instruments i)))
      synth!))
  Available
  (available [synth]
    (.getAvailableInstruments synth))
  Channels
  (channels [synth]
    (.getChannels synth)))

(defn latency ^long [^Synthesizer synth]
  (.getLatency synth))

(defn max-polyphony ^long [^Synthesizer synth]
  (.getMaxPolyphony synth))

(defn voice-status [^Synthesizer synth]
  (.getVoiceStatus synth))

(defn load!
  ([synth!]
   (load-instruments synth! synth!))
  ([synth! source]
   (if (seqable? source)
     (every? identity (map #(load-instruments % synth!) source))
     (load-instruments source synth!)))
  ([^Synthesizer synth! soundbank patches]
   (.loadInstruments synth! soundbank
                     (if (sequential? patches) (into-array Patch patches) patches))))

(defn unload!
  ([synth!]
   (unload-instruments synth! synth!)
   synth!)
  ([synth! source]
   (if (seqable? source)
     (doseq [instrument source]
       (unload-instruments source synth!))
     (unload-instruments source synth!))
   synth!))

(defn remap! [^Synthesizer synth! from to]
  (.remapInstrument synth! from to))

;; ============================= Soundbank  ================================

(extend-type Soundbank
  Info
  (info
    ([this]
     {:class (simple-name (class this))
      :description (.getDescription this)
      :name (.getName this)
      :vendor (.getVendor this)
      :version (.getVersion this)})
    ([this info-type]
     (case info-type
       :class (simple-name (class this))
       :description (.getDescription this)
       :name (.getName this)
       :vendor (.getVendor this)
       :version (.getVersion this)
       nil)))
  Instruments
  (instruments [soundbank]
    (.getInstruments soundbank))
  Load
  (load-instruments [soundbank synth!]
    (.loadAllInstruments ^Synthesizer synth! soundbank))
  (unload-instruments [soundbank synth!]
    (.unloadAllInstruments ^Synthesizer synth! soundbank)
    synth!)
  Support
  (supported [soundbank synth]
    (.isSoundbankSupported ^Synthesizer synth soundbank)))

(defn instrument
  ([^Soundbank soundbank patch]
   (.getInstrument soundbank patch)))

(defn resources [^Soundbank soundbank]
  (.getResources soundbank))

;; =================== SoundbankResource ==========================================

(extend-type SoundbankResource
  Info
  (info
    ([this]
     {:name (.getName this)})
    ([this info-type]
     (case info-type
       :name (.getName this)
       nil)))
  Data
  (data [resource]
    (.getData resource)))

(defn data-class [^SoundbankResource resource]
  (.getDataClass resource))

;; =================== Instrument ======================================================

(extend-type Instrument
  Load
  (load-instruments [instrument synth!]
    (.loadInstrument ^Synthesizer synth! instrument))
  (unload-instruments [instrument synth!]
    (.unloadInstrument ^Synthesizer synth! instrument)
    synth!))

;; =================== Patch ===========================================================

(extend-type Patch
  Info
  (info
    ([this]
     {:bank (.getBank this)
      :program (.getProgram this)})
    ([this info-type]
     (case info-type
       :bank (.getBank this)
       :program (.getProgram this)
       nil)))
  Program
  (program [patch]
    (.getProgram patch)))

(defn patch
  ([^Instrument instrument]
   (.getPatch instrument))
  ([bank program]
   (Patch. bank program)))

(defn bank ^long [^Patch patch]
  (.getBank patch))

;; =================== MidiFileFormat ==================================================

(extend-type MidiFileFormat
  Info
  (info
    ([this]
     (into {:type (itype this)}
           (map (fn [[k v]] [(name-key k) v]) (properties this))))
    ([this info-type]
     (case info-type
       :type (itype this)
       (property this (key-name info-type)))))
  Timing
  (micro-length [mff]
    (.getMicrosecondLength mff))
  (division [mff]
    (.getDivisionType mff))
  (resolution [mff]
    (.getResolution mff))
  Format
  (property [mff key]
    (.getProperty mff (name key)))
  (properties [mff]
    (.properties mff))
  (byte-length [mff]
    (.getByteLength mff))
  Type
  (itype [mff]
    (let [mff-type (.getType mff)]
      (get midi-file-type mff-type mff-type))))

(defn midi-file-format
  ([type division resolution bytes microseconds]
   (MidiFileFormat. type (get timing-type division division)
                    resolution bytes microseconds))
  ([type division resolution bytes microseconds properties]
   (MidiFileFormat. type (get timing-type division division)
                    resolution bytes microseconds properties)))

;; =================== MidiMessage =====================================================

(extend-type MidiMessage
  Info
  (info
    ([this]
     {:status (.getStatus this)
      :length (.getLength this)
      :bytes (.getMessage this)})
    ([this info-type]
     (case info-type
       :status (.getStatus this)
       :length (.getLength this)
       :bytes (.getMessage this)
       nil)))
  Event
  (event [message tick]
    (MidiEvent. message tick)))

(defn message-bytes [^MidiMessage message]
  (.getMessage message))

(defn message-length ^long [^MidiMessage message]
  (.getLength message))

(defn status [^MidiMessage message]
  (.getStatus message))

;; =================== MetaMessage =====================================================

(defn decode-integer [^bytes data]
  (case (alength data)
    1 (.get (ByteBuffer/wrap data) 0)
    2 (.getShort (ByteBuffer/wrap data) 0)
    3 (bit-or (bit-and (aget data 2) 0xFF)
              (bit-shift-left (bit-and (aget data 1) 0xFF) 8)
              (bit-shift-left (bit-and (aget data 0) 0xFF) 16))
    4 (.getInt (ByteBuffer/wrap data) 0)
    8 (.getLong (ByteBuffer/wrap data) 0)
    (throw (ex-info "Not an integer data buffer." {:type :sound-error
                                                   :length (alength data) :supported #{1 2 4 8}}))))

(defn decode-smpte [^bytes data]
  {:hours (aget data 0)
   :minutes (aget data 1)
   :seconds (aget data 2)
   :frames (aget data 3)
   :fractional-frames (aget data 4)})

(defn decode-time-signature [^bytes data]
  {:numerator (aget data 0)
   :denumerator (Math/pow 2 (aget data 1))
   :ticks-per-beat (aget data 2)
   :notes-per-beat (aget data 3)})

(defn decode-key-signature [^bytes data]
  {:mode (case (aget data 1)
           0 :major
           1 :minor
           :unknown)
   :key (aget data 0)})

(defn decode-vendor-specific [^bytes data]
  (or (< 0 (alength data))
      (if (= 0 (aget data 0))
        {:vendor-id (decode-integer (Arrays/copyOfRange data 0 3))
         :data (Arrays/copyOfRange data 3 (alength data))}
        {:vendor-id (aget data 0)
         :data (Arrays/copyOfRange data 1 (alength data))})))

(extend-type MetaMessage
  Info
  (info
    ([this]
     {:status (.getStatus this)
      :length (.getLength this)
      :bytes (.getMessage this)
      :type (itype this)
      :data (decode this)})
    ([this info-type]
     (case info-type
       :status (.getStatus this)
       :length (.getLength this)
       :bytes (.getMessage this)
       :type (itype this)
       :data (decode this)
       nil)))
  Type
  (itype [message]
    (let [mt (.getType message)]
      (get meta-message-type-key mt mt)))
  Data
  (data [message]
    (.getData message))
  (message! [mm! type data]
    (.setMessage mm! type data (alength data))
    mm!)
  (message! [mm! type data length]
    (.setMessage mm! type data length)
    mm!)
  Code
  (decode [message]
    (let [data (.getData message)]
      (case (.getType message)
        0x00 (decode-integer data)
        0x01 (String. data)
        0x02 (String. data)
        0x03 (String. data)
        0x04 (String. data)
        0x05 (String. data)
        0x06 (String. data)
        0x07 (String. data)
        0x20 (aget data 0)
        0x2F nil
        0x51 (decode-integer data)
        0x54 (decode-smpte data)
        0x58 (decode-time-signature data)
        0x59 (decode-key-signature data)
        0x7F data
        0xFF (decode-vendor-specific data)
        data))))

(defn meta-message
  ([]
   (MetaMessage.))
  ([type ^bytes data]
   (MetaMessage. type data (alength data)))
  ([type data length]
   (MetaMessage. type data length)))

;; =================== ShortMessage =====================================================

(defn channel [^ShortMessage message]
  (.getChannel message))

(defn command [^ShortMessage message]
  (.getCommand message))

(defn data1 ^long [^ShortMessage message]
  (.getData1 message))

(defn data2 ^long [^ShortMessage message]
  (.getData2 message))

(defn short-little-endian
  (^long [^long data1 ^long data2]
   (+ (* 128 data2) data1))
  (^long [^bytes data]
   (+ (* 128 (aget data 1)) (aget data 0))))

(defn decode-controller [^long controller ^long value]
  (let [controller-type (controller-type controller)]
    (merge {:controller controller
            :value value}
           (if controller-type
             {:control controller-type}
             nil))))

(extend-type ShortMessage
  Info
  (info
    ([message]
     (let [decoded-data (decode message)]
       (merge {:channel (channel message)
               :command (get command-type-key (command message))}
              (if (map? decoded-data)
                decoded-data
                {:data decoded-data}))))
    ([message info-type]
     (case info-type
       :status (status message)
       :length (message-length message)
       :bytes (message-bytes message)
       :channel (channel message)
       :command (get command-type-key (command message))
       :data1 (data1 message)
       :data2 (data2 message)
       :data (decode message)
       nil)))
  Data
  (message!
    ([sm! status]
     (.setMessage sm! (get command-type status status))
     sm!)
    ([sm! status data]
     (.setMessage sm! (get command-type status status) (aget ^bytes data 0) (aget ^bytes data 1))
     sm!)
    ([sm! status data1 data2]
     (.setMessage sm! (get command-type status status) data1 data2)
     sm!)
    ([sm! command channel data1 data2]
     (.setMessage sm! (get command-type command command) channel data1 data2)
     sm!))
  Code
  (decode [message]
    (case (.getCommand message)
      128 {:key (data1 message) :velocity (data2 message)}
      144 {:key (data1 message) :velocity (data2 message)}
      160 {:key (data1 message) :velocity (data2 message)}
      176 (decode-controller (data1 message) (data2 message))
      192 (data1 message)
      208 (data1 message)
      224 (short-little-endian (data1 message) (data2 message))
      242 (short-little-endian (data1 message) (data2 message))
      243 (data1 message)
      246 :tune
      nil)))

(defn short-message
  ([]
   (ShortMessage.))
  ([status]
   (ShortMessage. (get command-type status status)))
  ([status data1 data2]
   (ShortMessage. (get command-type status status) data1 data2))
  ([command channel data1 data2]
   (ShortMessage. (get command-type command command) channel data1 data2)))

;; =================== SysexMessage =====================================================

(extend-type SysexMessage
  Info
  (info
    ([message]
     {:status (status message)
      :length (message-length message)
      :bytes (message-bytes message)
      :data (decode message)})
    ([message info-type]
     (case info-type
       :status (status message)
       :length (message-length message)
       :bytes (message-bytes message)
       (get (decode message) info-type nil))))
  Data
  (data [message]
    (.getData message))
  (message!
    ([sm! data]
     (.setMessage sm! data (alength ^bytes data))
     sm!)
    ([sm! arg1 arg2]
     (if (integer? arg2)
       (.setMessage sm! arg1 arg2)
       (message! sm! arg1 arg2 (alength ^bytes arg2)))
     sm!)
    ([sm! status data length]
     (.setMessage sm! (get command-type status status) data length)
     sm!))
  Code
  (decode [message]
    (decode-vendor-specific (.getData message))))

(defn sysex-message
  ([]
   (SysexMessage.))
  ([data length]
   (SysexMessage. data length))
  ([status data length]
   (SysexMessage. (get command-type status status) data length)))

;; =================== MidiEvent ======================================================

(extend-type MidiEvent
  Info
  (info
    ([this]
     (assoc (info (.getMessage this))
            :tick (.getTick this)))
    ([this info-type]
     (case info-type
       :tick (.getTick this)
       (info (.getMessage this))))))

(defn message [^MidiEvent event]
  (.getMessage event))

(defn tick [^MidiEvent event]
  (.getTick event))

(defn tick! [^MidiEvent event! tick]
  (.setTick event! tick)
  event!)

;; =================== Sequence ========================================================

(extend-type Sequence
  Info
  (info
    ([this]
     {:ticks (.getTickLength this)})
    ([this info-type]
     (case info-type
       :id (System/identityHashCode this)
       :ticks (.getTickLength this)
       nil)))
  Timing
  (micro-length [this]
    (.getMicrosecondLength this))
  (division [this]
    (.getDivisionType this))
  (resolution [this]
    (.getResolution this))
  Tick
  (ticks [this]
    (.getTickLength this)))

(defn sequence
  ([source]
   (get-sequence source))
  ([division ^long resolution]
   (Sequence. (get timing-type division division) resolution))
  ([division ^long resolution ^long num-tracks]
   (Sequence. (get timing-type division division) resolution num-tracks)))

(defn tracks [^Sequence sequence]
  (.getTracks sequence))

(defn patches [^Sequence sequence]
  (.getPatchList sequence))

(defn track! [^Sequence sequence!]
  (.createTrack sequence!))

(defn delete! [^Sequence sequence! track]
  (.deleteTrack sequence! track))

;; =================== Track ==========================================

(extend-type Track
  Info
  (info
    ([this]
     {:size (.size this)
      :ticks (.ticks this)})
    ([this info-type]
     (case info-type
       :size (.size this)
       :ticks (.ticks this)
       nil)))
  Event
  (event [track i]
    (.get track i))
  Tick
  (ticks [track]
    (.ticks track)))

(defn add! [^Track track! event]
  (.add track! event))

(defn remove! [^Track track! event]
  (.remove track! event))

(defn events ^long [^Track track]
  (.size track))

;; =================== VoiceStatus ==========================================

(defn voice-status-info
  ([^VoiceStatus status]
   (into {:class "VoiceStatus"}
         (if (active? status)
           (remove nil?
                   (map (fn [^Field field]
                          (try
                            (vector (name-key (.getName field)) (.get field status))
                            (catch IllegalAccessException e nil)))
                        (.getDeclaredFields VoiceStatus)))
           [[:active false]])))
  ([^VoiceStatus status key]
   (if (active? status)
     (case key
       :class "VoiceStatus"
       :active (.active status)
       :bank (.bank status)
       :channel (.channel status)
       :note (.note status)
       :program (.program status)
       :volume (.volume status)
       (try
         (.get (.getDeclaredField (class status) (key-name key)) status)
         (catch NoSuchFieldException e nil)
         (catch IllegalAccessException e nil)))
     (if (= key :active) false nil))))

(extend-type VoiceStatus
  Info
  (info
    ([this]
     (voice-status-info this))
    ([this info-type]
     (voice-status-info this info-type)))
  Activity
  (active? [status]
    (.active status)))

;; =================== Sequential info for sampled arrays ==============================

(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiDevice$Info;") )
(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiDevice;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiChannel;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Receiver;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Transmitter;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Soundbank;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.SoundbankResource;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Instrument;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Patch;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiFileFormat;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Sequencer$SyncMode;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Sequence;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.VoiceStatus;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiEvent;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.MidiMessage;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.MetaMessage;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.ShortMessage;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.SysexMessage;"))
(extend-array-info (Class/forName "[Ljavax.sound.midi.Track;"))

;; =================== User friendly printing ==========================================

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiDevice$Info;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiDevice;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiChannel;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Receiver;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Transmitter;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Soundbank;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.SoundbankResource;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Instrument;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Patch;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiFileFormat;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Sequencer$SyncMode;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Sequence;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.VoiceStatus;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiEvent;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiMessage;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MetaMessage;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.ShortMessage;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.SysexMessage;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Track;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method MetaEventListener
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method ControllerEventListener
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiDevice$Info
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiDevice
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiChannel
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Receiver
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Transmitter
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Soundbank
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method SoundbankResource
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Patch
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiFileFormat
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiMessage
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MetaMessage
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method MidiEvent
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Sequence
  [this ^java.io.Writer w]
  (.write w (pr-str (assoc (info this) :id (System/identityHashCode this)))))

(defmethod print-method Track
  [this ^java.io.Writer w]
  (.write w (pr-str (assoc (info this) :id (System/identityHashCode this)))))

(defmethod print-method VoiceStatus
  [^VoiceStatus this ^java.io.Writer w]
  (.write w (pr-str (info this))))
