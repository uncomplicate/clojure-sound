(ns uncomplicate.clojure-sound.midi
  (:refer-clojure :exclude [sequence])
  (:require [uncomplicate.commons.core :refer [Releaseable]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support SequenceSource set-sequence Load
                               load-instruments unload-instruments]]
             [core :refer [write! Info InfoProvider Open Timestamp Reset Broadcast Activity Type]]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.midi MidiSystem MidiDevice MidiDevice$Info MidiFileFormat MidiChannel
            Receiver MidiDeviceReceiver MidiDeviceTransmitter Sequencer Soundbank Synthesizer
            Transmitter ControllerEventListener MetaEventListener Instrument MetaMessage MidiEvent
            MidiMessage Patch Sequence Sequencer$SyncMode ShortMessage SoundbankResource
            SysexMessage Track VoiceStatus]))

(defprotocol Instruments
  (instruments [this]))

;; ===================== Keyword coding ================================================

(def sync-mode
  {:internal-clock Sequencer$SyncMode/INTERNAL_CLOCK
   :internal Sequencer$SyncMode/INTERNAL_CLOCK
   :midi-sync Sequencer$SyncMode/MIDI_SYNC
   :sync Sequencer$SyncMode/MIDI_SYNC
   :midi-time-code Sequencer$SyncMode/MIDI_TIME_CODE
   :time Sequencer$SyncMode/MIDI_TIME_CODE
   :midi-time Sequencer$SyncMode/MIDI_TIME_CODE
   :no-sync Sequencer$SyncMode/NO_SYNC})

;; =========================== MidiSystem ====================================

(defprotocol MidiSystemProcedures
  (file-format [this])
  (sequence [this])
  (soundbank [this])
  (device [this]))

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
    (MidiSystem/getSoundbank url))
  Sequencer
  (sequence [sequencer]
    (.getSequence sequencer))
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
    (.getDefaultSoundbank synth)))

(defn device-info []
  (MidiSystem/getMidiDeviceInfo))

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

(defn mute
  ([^MidiChannel channel]
   (.getMute channel))
  ([^Sequencer sequencer track]
   (.getTrackMute sequencer track)))

(defn mute!
  ([^MidiChannel channel!]
   (.setMute channel! true)
   channel!)
  ([^MidiChannel channel! mute]
   (.setMute channel! mute)
   channel!)
  ([^Sequencer sequencer! track mute]
   (.setTrackMute sequencer! track mute)
   sequencer!))

(defn omni [^MidiChannel channel]
  (.getOmni channel))

(defn omni!
  ([^MidiChannel channel!]
   (.setOmni channel! true)
   channel!)
  ([^MidiChannel channel! on]
   (.setOmni channel! on)
   channel!))

(defn bend ^long [^MidiChannel channel]
  (.getPitchBend channel))

(defn bend! [^MidiChannel channel! bend]
  (.setPitchBend channel! bend)
  channel!)

(defn program ^long [^MidiChannel channel]
  (.getProgram channel))

(defn solo
  ([^MidiChannel channel]
   (.getSolo channel))
  ([^Sequencer sequencer track]
   (.getTrackSolo sequencer track)))

(defn solo!
  ([^MidiChannel channel!]
   (.setSolo channel! true)
   channel!)
  ([^MidiChannel channel! on]
   (.setSolo channel! on)
   channel!)
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
    (.close this)
    true))

(defn send! [^Receiver receiver! ^MidiMessage message ^long timestamp]
  (.send receiver! message timestamp)
  receiver!)

;; ============================= Transmitter ================================

(extend-type Transmitter
  Releaseable
  (release [this]
    (.close this)
    true))

(defn receiver [^Transmitter transmitter]
  (.getReceiver transmitter))

(defn receiver! [^Transmitter transmitter! receiver]
  (.setReceiver transmitter! receiver)
  transmitter!)

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

(extend-type Sequencer
  Broadcast
  (listen! [sequencer! listener]
    (.addMetaEventListener sequencer! listener))
  (listen! [sequencer! listener controllers]
    (.addControllerEventListener sequencer! listener controllers))
  (ignore! [sequencer! listener]
    (.removeMetaEventListener sequencer! listener)
    sequencer!)
  (ignore! [sequencer! listener controllers]
    (.removeControllerEventListener sequencer! listener controllers))
  Timestamp
  (ms-length [sequencer]
    (.getMicrosecondLength sequencer))
  (ms-position [sequencer]
    (.getMicrosecondPosition sequencer))
  (ms-position! [sequencer! microseconds]
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
    sequencer!))

(extend-protocol SequenceSource
  InputStream
  (set-sequence [stream sequencer!]
    (.setSequence ^Sequencer sequencer! stream))
  Sequence
  (set-sequence [s sequencer!]
    (.setSequence ^Sequencer sequencer! s)))

(defn sequence! [sequencer! source]
  (set-sequence source sequencer!)
  sequencer!)

(defn loop-count ^long [^Sequencer sequencer]
  (.getLoopCount sequencer))

(defn loop-count! [^Sequencer sequencer! count]
  (.setLoopCount sequencer! count)
  sequencer!)

(defn end ^long [^Sequencer sequencer]
  (.getLoopEndPoint sequencer))

(defn end! [^Sequencer sequencer! tick]
  (.setLoopEndPoint sequencer! tick)
  sequencer!)

(defn start ^long [^Sequencer sequencer]
  (.getLoopStartPoint sequencer))

(defn start! [^Sequencer sequencer! tick]
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

(defn tick-length ^long [^Sequencer sequencer]
  (.getTickLength sequencer))

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

;; =================== Instrument ======================================================

(extend-type Instrument
  Load
  (load-instruments [instrument synth!]
    (.loadInstrument ^Synthesizer synth! instrument))
  (unload-instruments [instrument synth!]
    (.unloadInstrument ^Synthesizer synth! instrument)
    synth!))

(defn patch [^Instrument instrument]
  (.getPatch instrument))

;; =================== MetaMessage =====================================================

(extend-type MetaMessage
  Type
  (mytype [message]
    (.getType message)))

(defn meta-message
  ([]
   (MetaMessage.))
  ([type ^bytes data]
   (MetaMessage. type data (alength data)))
  ([type data length]
   (MetaMessage. type data length)))

(defn data [^MetaMessage message]
  (.getData message))

(defn set! [^MetaMessage message! type data length]
  (.setMessage message! type data length)
  message!)

;; =================== User friendly printing ==========================================

(defmethod print-method MidiDevice$Info
  [info ^java.io.Writer w]
  (.write w (pr-str (bean info))))

(defmethod print-method Soundbank
  [soundbank ^java.io.Writer w]
  (.write w (pr-str (bean soundbank))))

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

(defmethod print-method MidiMessage
  [message ^java.io.Writer w]
  (.write w (pr-str (dissoc (bean message) :class))))

(defmethod print-method MetaMessage
  [message ^java.io.Writer w]
  (.write w (pr-str (dissoc (bean message) :class))))
