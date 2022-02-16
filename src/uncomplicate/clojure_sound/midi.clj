;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-sound.midi
  (:refer-clojure :exclude [sequence])
  (:require [uncomplicate.commons.core :refer [Releaseable Closeable close!] :as commons]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support SequenceSource set-sequence get-sequence
                               Load load-instruments unload-instruments simple-name key-name
                               ReceiverProvider get-receiver]]
             [core :refer [write! Info InfoProvider Open Timing Reset Broadcast Activity Type
                           Format active? InfoProvider connect! division resolution mytype]]])
  (:import [clojure.lang ILookup IFn]
           java.lang.reflect.Field
           java.net.URL
           [java.io File InputStream OutputStream]
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
  (message! [message status] [message arg1 arg2 arg3] [message command channel data1 data2]))

(defprotocol Tick
  (ticks [this]))

(defprotocol Event
  (event [this arg]))

;; ===================== Keyword coding ================================================

(def sync-mode
  {:internal Sequencer$SyncMode/INTERNAL_CLOCK
   :sync Sequencer$SyncMode/MIDI_SYNC
   :time Sequencer$SyncMode/MIDI_TIME_CODE
   :no-sync Sequencer$SyncMode/NO_SYNC})

(def timing-type
  {:ppq Sequence/PPQ
   :smpte24 Sequence/SMPTE_24
   :smpte25 Sequence/SMPTE_25
   :smpte30 Sequence/SMPTE_30
   :smpte30drop Sequence/SMPTE_30DROP})

(def timing-type-key
  {Sequence/PPQ :ppq
   Sequence/SMPTE_24 :smpte24
   Sequence/SMPTE_25 :smpte25
   Sequence/SMPTE_30 :smpte30
   Sequence/SMPTE_30DROP :smpte30drop})

(def message-status
  {:active-sensing ShortMessage/ACTIVE_SENSING
   :channel-pressure ShortMessage/CHANNEL_PRESSURE
   :continue ShortMessage/CONTINUE
   :control-change ShortMessage/CONTROL_CHANGE
   :exclusive-end ShortMessage/END_OF_EXCLUSIVE
   :time ShortMessage/MIDI_TIME_CODE
   :off ShortMessage/NOTE_OFF
   :on ShortMessage/NOTE_ON
   :bend ShortMessage/PITCH_BEND
   :poly-pressure ShortMessage/POLY_PRESSURE
   :program-change ShortMessage/PROGRAM_CHANGE
   :position ShortMessage/SONG_POSITION_POINTER
   :select ShortMessage/SONG_SELECT
   :start ShortMessage/START
   :stop ShortMessage/STOP
   :reset ShortMessage/SYSTEM_RESET
   :clock ShortMessage/TIMING_CLOCK
   :tune ShortMessage/TUNE_REQUEST
   :special-system-exclusive SysexMessage/SPECIAL_SYSTEM_EXCLUSIVE
   :system-exclusive SysexMessage/SYSTEM_EXCLUSIVE})

(def message-status-key
  {ShortMessage/ACTIVE_SENSING :active-sensing
   ShortMessage/CHANNEL_PRESSURE :channel-pressure
   ShortMessage/CONTINUE :continue
   ShortMessage/CONTROL_CHANGE :control-change
   ShortMessage/END_OF_EXCLUSIVE :exclusive-end
   ShortMessage/MIDI_TIME_CODE :time
   ShortMessage/NOTE_OFF :off
   ShortMessage/NOTE_ON :on
   ShortMessage/PITCH_BEND :bend
   ShortMessage/POLY_PRESSURE :poly-pressure
   ShortMessage/PROGRAM_CHANGE :program-change
   ShortMessage/SONG_POSITION_POINTER :position
   ShortMessage/SONG_SELECT :select
   ShortMessage/START :start
   ShortMessage/STOP :stop
   ShortMessage/SYSTEM_RESET :reset
   ShortMessage/TIMING_CLOCK :clock
   ShortMessage/TUNE_REQUEST :tune})

(def sysex-status-key
  {SysexMessage/SPECIAL_SYSTEM_EXCLUSIVE :special-system-exclusive
   SysexMessage/SYSTEM_EXCLUSIVE :system-exclusive})

(def meta-message-type
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

(def meta-message-type-key
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

;; =========================== MidiSystem ====================================

(defprotocol MidiSystemProcedures
  (file-format [this])
  (soundbank [this])
  (device [this]))

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
    (sequence (division sq) (resolution sq) (tracks sq))))

(extend-protocol MidiSystemProcedures
  File
  (file-format [file]
    (MidiSystem/getMidiFileFormat file))
  (soundbank [file]
    (MidiSystem/getSoundbank file))
  InputStream
  (file-format [stream]
    (MidiSystem/getMidiFileFormat stream))
  (soundbank [stream]
    (MidiSystem/getSoundbank stream))
  URL
  (file-format [url]
    (MidiSystem/getMidiFileFormat url))
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
  InfoProvider
  (info [info]
    info)
  Info
  (description [info]
    (.getDescription info))
  (myname [info]
    (.getName info))
  (vendor [info]
    (.getVendor info))
  (version [info]
    (.getVersion info)))

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
  Releaseable
  (release [this]
    (close! this)
    true)
  InfoProvider
  (info [device]
    (.getDeviceInfo device))
  ReceiverProvider
  (get-receiver [device]
    (.getReceiver device))
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
  (.noteOff channel! note velocity)
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

(defn controller ^long [^MidiChannel channel ^long controller]
  (.getController channel controller))

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
  ([^MidiChannel channel! ^long controller ^long val]
   (.controlChange channel! controller val)
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

;; ============================= Soundbank  ================================

(extend-type Soundbank
  Info
  (description [soundbank]
    (.getDescription soundbank))
  (myname [soundbank]
    (.getName soundbank))
  (vendor [soundbank]
    (.getVendor soundbank))
  (version [soundbank]
    (.getVersion soundbank))
  Instruments
  (instruments [soundbank]
    (.getInstruments soundbank))
  Load
  (load-instruments [soundbank synth!]
    (.loadAllInstruments ^Synthesizer synth! soundbank))
  (unload-instruments [soundbank synth!]
    (.unloadAllInstruments ^Synthesizer synth! soundbank))
  Support
  (supported [soundbank synth]
    (.isSoundbankSupported ^Synthesizer synth soundbank)))

(defn instrument
  ([^Soundbank soundbank patch]
   (.getInstrument soundbank patch)))

(defn resources [^Soundbank soundbank]
  (.getResources soundbank))

;; ============================= Sequencer ================================

(deftype MetaEventListenerFunction [f ^int meta-type]
  MetaEventListener
  (meta [_ message]
    (when (or (< meta-type 0) (= meta-type (.getType message)))
      (f message))))

(defn meta-listener
  ([meta-type f]
   (->MetaEventListenerFunction f (get meta-message-type meta-type meta-type)))
  ([f]
   (meta-listener f -1)))

(deftype ControllerEventListenerFunction [f]
  ControllerEventListener
  (controlChange [_ message]
    (f message)))

(defn ctrl-listener [f]
  (->ControllerEventListenerFunction f))

(extend-type Sequencer
  Broadcast
  (listen!
    ([sequencer! listener]
     (.addMetaEventListener sequencer!
                            (if (instance? MetaEventListener listener)
                              listener
                              (meta-listener listener))))
    ([sequencer! listener selection]
     (if (or (number? selection) (keyword? selection))
       (.addMetaEventListener sequencer! (meta-listener selection listener))
       (.addControllerEventListener sequencer!
                                    (if (instance? ControllerEventListener listener)
                                      listener
                                      (->ControllerEventListenerFunction listener))
                                    (if (sequential? selection)
                                      (int-array selection)
                                      selection)))))
  (ignore!
    ([sequencer! listener]
     (.removeMetaEventListener sequencer! listener)
     sequencer!)
    ([sequencer! listener controllers]
     (.removeControllerEventListener sequencer! listener
                                     (if (sequential? controllers)
                                       (int-array controllers)
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

(defn master-mode [^Sequencer sequencer]
  (.getMasterSyncMode sequencer))

(defn master-mode! [^Sequencer sequencer! sync]
  (.setMasterSyncMode sequencer! (get sync-mode sync sync))
  sequencer!)

(defn master-modes [^Sequencer sequencer]
  (.getMasterSyncModes sequencer))

(defn slave-mode [^Sequencer sequencer]
  (.getSlaveSyncMode sequencer))

(defn slave-mode! [^Sequencer sequencer! sync]
  (.setSlaveSyncMode sequencer! (get sync-mode sync sync))
  sequencer!)

(defn slave-modes [^Sequencer sequencer]
  (.getSlaveSyncModes sequencer))

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
    (.getLoadedInstruments synth)))

(defn available [^Synthesizer synth]
  (.getAvailableInstruments synth))

(defn channels [^Synthesizer synth]
  (.getChannels synth))

(defn latency ^long [^Synthesizer synth]
  (.getLatency synth))

(defn max-polyphony ^long [^Synthesizer synth]
  (.getMaxPolyphony synth))

(defn voice-status [^Synthesizer synth]
  (.getVoiceStatus synth))

(defn load!
  ([synth! source]
   (load-instruments source synth!))
  ([^Synthesizer synth! soundbank patches]
   (.loadInstruments synth! soundbank
                     (if (sequential? patches) (into-array Patch patches) patches))))

(defn unload! [synth! source]
  (unload-instruments source synth!)
  synth!)

(defn remap! [^Synthesizer synth! from to]
  (.remapInstrument synth! from to))

;; =================== SoundbankResource ==========================================

(extend-type SoundbankResource
  Info
  (myname [this]
    (.getName this))
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

;; =================== MidiFileFormat ==================================================

(extend-type MidiFileFormat
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
  (properties [mf]
    (.properties mf))
  (byte-length [mff]
    (.getByteLength mff))
  Type
  (mytype [mff]
    (.getType mff)))

(defn midi-file-format
  ([type division resolution bytes microseconds]
   (MidiFileFormat. type (get timing-type division division)
                    resolution bytes microseconds))
  ([type division resolution bytes microseconds properties]
   (MidiFileFormat. type (get timing-type division division)
                    resolution bytes microseconds properties)))

;; =================== MidiMessage =====================================================

(defn message-bytes [^MidiMessage message]
  (.getMessage message))

(defn message-length ^long [^MidiMessage message]
  (.getLength message))

(defn status [^MidiMessage message]
  (let [s (.getStatus message)]
    (get message-status-key s s)))

(extend-type MidiMessage
  commons/Info
  (commons/info
    ([message]
     {:status (message-status-key (status message))
      :length (message-length message)
      :bytes (message-bytes message)})
    ([message info-type]
     (case info-type
       :status (message-status-key (status message))
       :length (message-length message)
       :bytes (message-bytes message)
       nil)))
  Event
  (event [message tick]
    (MidiEvent. message tick)))

;; =================== MetaMessage =====================================================

(extend-type MetaMessage
  commons/Info
  (commons/info
    ([message]
     {:status (message-status-key (status message))
      :length (message-length message)
      :bytes (message-bytes message)
      :type (mytype message)
      :data (data message)})
    ([message info-type]
     (case info-type
       :status (message-status-key (status message))
       :length (message-length message)
       :bytes (message-bytes message)
       :type (mytype message)
       :data (data message)
       nil)))
  Type
  (mytype [message]
    (let [mt (.getType message)]
      (get meta-message-type-key mt mt)))
  Data
  (data [message]
    (.getData message))
  (message!! [mm! type data length]
    (.setMessage mm! type data length)
    mm!))

(defn meta-message
  ([]
   (MetaMessage.))
  ([type ^bytes data]
   (MetaMessage. type data (alength data)))
  ([type data length]
   (MetaMessage. type data length)))

;; =================== MidiEvent ======================================================

(defn message [^MidiEvent event]
  (.getMessage event))

(defn tick [^MidiEvent event]
  (.getTick event))

(defn tick! [^MidiEvent event! tick]
  (.setTick event! tick)
  event!)

;; =================== Patch ===========================================================

(extend-type Patch
  Program
  (program [patch]
    (.getProgram patch)))

(defn patch
  ([^Instrument instrument]
   (.getPatch instrument))
  ([bank program]
   (Patch. bank program)))

;; =================== Sequence ========================================================

(defn tracks [^Sequence sequence]
  (.getTracks sequence))

(defn sequence
  ([source]
   (get-sequence source))
  ([division ^long resolution]
   (Sequence. (get timing-type division division) resolution))
  ([division ^long resolution ^long num-tracks]
   (Sequence. (get timing-type division division) resolution num-tracks)))

(extend-type Sequence
  Timing
  (micro-length [s]
    (.getMicrosecondLength s))
  (division [s]
    (.getDivisionType s))
  (resolution [s]
    (.getResolution s))
  Tick
  (ticks [s]
    (.getTickLength s)))

(defn track! [^Sequence sequence!]
  (.createTrack sequence!))

(defn delete! [^Sequence sequence! track]
  (.deleteTrack sequence! track))

(defn patches [^Sequence sequence]
  (.getPatchList sequence))

;; =================== ShortMessage =====================================================

(defn channel [^ShortMessage message]
  (.getChannel message))

(defn command [^ShortMessage message]
  (.getCommand message))

(defn data1 ^long [^ShortMessage message]
  (.getData1 message))

(defn data2 ^long [^ShortMessage message]
  (.getData2 message))

(extend-type ShortMessage
  commons/Info
  (commons/info
    ([message]
     {:status (message-status-key (status message))
      :length (message-length message)
      :bytes (message-bytes message)
      :channel (channel message)
      :command (command message)
      :data1 (data1 message)
      :data2 (data2 message)})
    ([message info-type]
     (case info-type
       :status (message-status-key (status message))
       :length (message-length message)
       :bytes (message-bytes message)
       :channel (channel message)
       :command (command message)
       :data1 (data1 message)
       :data2 (data2 message)
       nil)))
  Data
  (message!
    ([sm! status]
     (.setMessage sm! (get message-status status status))
     sm!)
    ([sm! status data1 data2]
     (.setMessage sm! (get message-status status status) data1 data2)
     sm!)
    ([sm! command channel data1 data2]
     (.setMessage sm! (get message-status command command) channel data1 data2)
              sm!)))

(defn short-message
  ([]
   (ShortMessage.))
  ([status]
   (ShortMessage. (get message-status status status)))
  ([status data1 data2]
   (ShortMessage. (get message-status status status) data1 data2))
  ([command channel data1 data2]
   (ShortMessage. command channel data1 data2)))


;; =================== SysexMessage =====================================================

(extend-type SysexMessage
  Data
  (data [message]
    (.getData message))
  (message!
    ([sm! data length]
     (.setMessage sm! data length)
     sm!)
    ([sm! status data length]
     (.setMessage sm! (get message-status status status) data length)
     sm!)))

(defn sysex-message
  ([]
   (SysexMessage.))
  ([data length]
   (SysexMessage. data length))
  ([status data length]
   (SysexMessage. (get message-status status status) data length)))

;; =================== Track ==========================================

(extend-type Track
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

(extend-type VoiceStatus
  Activity
  (active? [status]
    (.active status)))

(defn voice-status-info
  ([^VoiceStatus status]
   (into {:class "VoiceStatus"}
         (if (active? status)
           (map (fn [^Field field]
                  (vector (name-key (.getName field)) (.get field status)))
                (.getDeclaredFields VoiceStatus))
           [[:active false]])))
  ([^VoiceStatus status key]
   (if (active? status)
     (case key
       :active (.active status)
       :bean (.bank status)
       :channel (.channel status)
       :note (.note status)
       :program (.program status)
       :volume (.volume status)
       (try
         (.get (.getDeclaredField (class status) (key-name key)) status)
         (catch NoSuchFieldException e nil)
         (catch IllegalAccessException e nil)))
     (if (= key :active) false nil))))

;; =================== User friendly printing ==========================================

(defmethod print-method MidiDevice$Info
  [info ^java.io.Writer w]
  (.write w (pr-str (update (bean info) :class simple-name))))

(defmethod print-method MidiDevice
  [device ^java.io.Writer w]
  (.write w (pr-str (assoc (bean (device-info device)) :class (simple-name (class device))))))

(defmethod print-method Receiver
  [receiver ^java.io.Writer w]
  (.write w (pr-str (bean receiver))))

(defmethod print-method Transmitter
  [transmitter ^java.io.Writer w]
  (.write w (pr-str (bean transmitter))))

(defmethod print-method Soundbank
  [soundbank ^java.io.Writer w]
  (.write w (pr-str (update (bean soundbank) :class simple-name))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiDevice$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.SoundbankResource;")
  [resources ^java.io.Writer w]
  (.write w (pr-str (seq resources))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Instrument;")
  [instruments ^java.io.Writer w]
  (.write w (pr-str (seq instruments))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Sequencer$SyncMode;")
  [modes ^java.io.Writer w]
  (.write w (pr-str (seq modes))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.MidiChannel;")
  [channel ^java.io.Writer w]
  (.write w (pr-str (seq channel))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.VoiceStatus;")
  [voices ^java.io.Writer w]
  (.write w (pr-str (seq voices))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Patch;")
  [patches ^java.io.Writer w]
  (.write w (pr-str (seq patches))))

(defmethod print-method (Class/forName "[Ljavax.sound.midi.Track;")
  [tracks ^java.io.Writer w]
  (.write w (pr-str (seq tracks))))

(defmethod print-method SoundbankResource
  [resource ^java.io.Writer w]
  (.write w (pr-str (update (bean resource) :class simple-name))))

(defmethod print-method MidiEvent
  [event ^java.io.Writer w]
  (.write w (pr-str (update (bean event) :class simple-name))))

(defmethod print-method MidiMessage
  [message ^java.io.Writer w]
  (.write w (pr-str (assoc (commons/info message) :class (simple-name (class message))))))

(defmethod print-method Patch
  [message ^java.io.Writer w]
  (.write w (pr-str (update (bean patch) :class simple-name))))

(defmethod print-method Sequence
  [s ^java.io.Writer w]
  (.write w (pr-str (update (bean s) :class simple-name))))

(defmethod print-method Track
  [track ^java.io.Writer w]
  (.write w (pr-str (update (bean track) :class simple-name))))

(defmethod print-method VoiceStatus
  [^VoiceStatus status ^java.io.Writer w]
  (.write w (pr-str (voice-status-info status))))
