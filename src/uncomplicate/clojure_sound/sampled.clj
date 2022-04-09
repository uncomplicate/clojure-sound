;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.clojure-sound.sampled
  (:require [clojure.walk :refer [stringify-keys]]
            [uncomplicate.commons.core :refer [Releaseable close! Info info]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support simple-name]]
             [core :refer [write! SoundInfoProvider Open Timing Reset Broadcast Activity Type
                           Format get-format SoundSystemProcedures file-format itype properties
                           sound-info]]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [java.security Permission]
           [javax.sound.sampled AudioSystem AudioFormat AudioInputStream AudioPermission
            AudioFormat AudioFormat$Encoding AudioFileFormat AudioFileFormat$Type Mixer Mixer$Info
            Line Line$Info DataLine DataLine$Info Port Port$Info SourceDataLine TargetDataLine Clip
            Control$Type Control BooleanControl BooleanControl$Type CompoundControl EnumControl
            EnumControl$Type FloatControl FloatControl$Type LineListener LineEvent LineEvent$Type
            ReverbType]
           [clojure.lang Sequential Keyword] ))

(defprotocol Match
  (matches? [this other]))

(defprotocol AudioSystemProcedures
  (audio-input-stream [this] [target-format source-stream])
  (encodings [this] [target-format source-stream])
  (convertible? [target source]))

(defprotocol Available
  (available [this]))

(defprotocol Frame
  (frame-length [this])
  (frame-position [this]))

(defprotocol Value
  (value [this])
  (value! [this val]))

;; ===================== Keyword coding ================================================

(def ^:const global-const
  {:not-specified AudioSystem/NOT_SPECIFIED})

(def port-info
  {:microphone Port$Info/MICROPHONE
   :mic Port$Info/MICROPHONE
   :headphone Port$Info/HEADPHONE
   :phones Port$Info/HEADPHONE
   :line-in Port$Info/LINE_IN
   :in Port$Info/LINE_IN
   :line-out Port$Info/LINE_OUT
   :out Port$Info/LINE_OUT
   :speaker Port$Info/SPEAKER
   :compact-disc Port$Info/COMPACT_DISC
   :cd Port$Info/COMPACT_DISC})

(def line-key-class
  {:target TargetDataLine
   :target-data-line TargetDataLine
   :source SourceDataLine
   :source-data-line SourceDataLine
   :clip Clip
   :port Port
   :mixer Mixer
   :line Line
   :data-line DataLine})

(def line-class-key
  {TargetDataLine :target
   SourceDataLine :source
   Clip :clip
   Port :port
   Mixer :mixer
   Line :line
   DataLine :data-line})

(def audio-encoding
  {:alaw AudioFormat$Encoding/ALAW
   :ulaw AudioFormat$Encoding/ULAW
   :pcm-signed AudioFormat$Encoding/PCM_SIGNED
   :pcm-unsigned AudioFormat$Encoding/PCM_UNSIGNED
   :pcm-float AudioFormat$Encoding/PCM_FLOAT
   :signed AudioFormat$Encoding/PCM_SIGNED
   :unsigned AudioFormat$Encoding/PCM_UNSIGNED
   :float AudioFormat$Encoding/PCM_FLOAT
   :byte AudioFormat$Encoding/PCM_SIGNED})

(def ^:const big-endian?
  {:big-endian true
   :big true
   true true
   :little-endian false
   :little false
   false false})

(def ^:const signed?
  {:signed true
   true true
   :unsigned false
   false false})

(def audio-file-format-type
  {:aifc AudioFileFormat$Type/AIFC
   :aiff AudioFileFormat$Type/AIFF
   :au AudioFileFormat$Type/AU
   :snd AudioFileFormat$Type/SND
   :wave AudioFileFormat$Type/WAVE})

(def audio-file-format-type-key
  {AudioFileFormat$Type/AIFC :aifc
   AudioFileFormat$Type/AIFF :aiff
   AudioFileFormat$Type/AU :au
   AudioFileFormat$Type/SND :snd
   AudioFileFormat$Type/WAVE :wave})

(def control-type
  {:apply-reverb BooleanControl$Type/APPLY_REVERB
   :reverb EnumControl$Type/REVERB
   :mute BooleanControl$Type/MUTE
   :aux-return FloatControl$Type/AUX_RETURN
   :aux-send FloatControl$Type/AUX_SEND
   :balance FloatControl$Type/BALANCE
   :master-gain FloatControl$Type/MASTER_GAIN
   :pan FloatControl$Type/PAN
   :reverb-return FloatControl$Type/REVERB_RETURN
   :reverb-send FloatControl$Type/REVERB_SEND
   :sample-rate FloatControl$Type/SAMPLE_RATE
   :volume FloatControl$Type/VOLUME
   :vol FloatControl$Type/VOLUME})

(def control-type-key
  {BooleanControl$Type/APPLY_REVERB :apply-reverb
   EnumControl$Type/REVERB :reverb
   BooleanControl$Type/MUTE :mute
   FloatControl$Type/AUX_RETURN :aux-return
   FloatControl$Type/AUX_SEND :aux-send
   FloatControl$Type/BALANCE :balance
   FloatControl$Type/MASTER_GAIN :master-gain
   FloatControl$Type/PAN :pan
   FloatControl$Type/REVERB_RETURN :reverb-return
   FloatControl$Type/REVERB_SEND :reverb-send
   FloatControl$Type/SAMPLE_RATE :sample-rate
   FloatControl$Type/VOLUME :volume})

(def ^:const line-event-type
  {:close LineEvent$Type/CLOSE
   :open LineEvent$Type/OPEN
   :start LineEvent$Type/START
   :stop LineEvent$Type/STOP})

(def ^:const line-event-type-key
  {LineEvent$Type/CLOSE :close
   LineEvent$Type/OPEN :open
   LineEvent$Type/START :start
   LineEvent$Type/STOP :stop})

;; =========================== AudioSystem ====================================

(extend-protocol SoundSystemProcedures
  File
  (file-format [file]
    (AudioSystem/getAudioFileFormat file))
  InputStream
  (file-format [stream]
    (AudioSystem/getAudioFileFormat stream))
  URL
  (afile-format [url]
    (AudioSystem/getAudioFileFormat url)))

(extend-protocol AudioSystemProcedures
  File
  (audio-input-stream [file]
    (AudioSystem/getAudioInputStream file))
  InputStream
  (audio-input-stream [stream]
    (AudioSystem/getAudioInputStream stream))
  URL
  (audio-input-stream [url]
    (AudioSystem/getAudioInputStream url))
  AudioFormat
  (audio-input-stream [target source]
    (AudioSystem/getAudioInputStream target ^AudioInputStream source))
  (encodings [source]
    (AudioSystem/getTargetEncodings source))
  (convertible? [target source]
    (AudioSystem/isConversionSupported target ^AudioFormat source))
  AudioFormat$Encoding
  (audio-input-stream [target source]
    (AudioSystem/getAudioInputStream target ^AudioInputStream source))
  (encodings [source]
    (AudioSystem/getTargetEncodings source))
  (convertible? [target source]
    (AudioSystem/isConversionSupported target ^AudioFormat source)))

(defn audio-file-types [^AudioInputStream stream]
  (AudioSystem/getAudioFileTypes stream))

(defn target-data-line
  ([^AudioFormat format]
   (AudioSystem/getTargetDataLine format))
  ([^AudioFormat format ^Mixer$Info mixer-info]
   (AudioSystem/getTargetDataLine format mixer-info)))

(defn target-formats [encoding source-format]
  (AudioSystem/getTargetFormats encoding source-format))

(defn target-line-info [^Line$Info info]
  (AudioSystem/getTargetLineInfo info))

;; =========================== Line ============================================

(deftype LineListenerFunction [f]
  Info
  (info [_]
    {:fn f})
  (info [_ info-type]
   (case info-type
     :fn f
     nil))
  LineListener
  (update [_ event]
    (f event)))

(defn line-listener [f]
  (->LineListenerFunction f))

(extend-type Line
  Info
  (info
    ([this]
     {:class (.getLineClass (.getLineInfo this))
      :status (if (.isOpen this) :open :closed)})
    ([this info-type]
     (case info-type
       :class (.getLineClass (.getLineInfo this))
       :status (if (.isOpen this) :open :closed))))
  Releaseable
  (release [this]
    (close! this)
    true)
  SoundInfoProvider
  (sound-info [line]
    (.getLineInfo this))
  Open
  (open! [line]
    (.open line)
    line)
  (open? [line]
    (.isOpen line))
  SoundInfoProvider
  (sound-info [this]
    (.getLineInfo this))
  Broadcast
  (listen! [line! listener]
    (let [listener (if (instance? LineListener listener)
                     listener
                     (line-listener listener))]
      (.addLineListener line! listener)
      listener))
  (ignore! [line! listener]
    (.removeLineListener line! listener)
    line!))

(extend-protocol SoundInfoProvider
  java.lang.Class
  (sound-info [c]
    (Line$Info. c))
  Keyword
  (sound-info [kw]
    (get port-info kw (ex-info "Unknown port info." {:type :sound-error
                                                     :requested kw
                                                     :supported (keys port-info)}))))

(extend-type Line$Info
  Info
  (info
    ([this]
     {:class (simple-name (.getLineClass this))})
    ([this info-type]
     (case info-type
       :class (simple-name (.getLineClass this))
       nil)))
  SoundInfoProvider
  (sound-info [this]
    this)
  Match
  (matches? [this other]
    (.matches this other))
  Support
  (supported [info]
    (AudioSystem/isLineSupported (get port-info info info))))

(extend-type Port$Info
  Info
  (info
    ([this]
     {:class (simple-name (.getLineClass this))
      :name (.getName this)})
    ([this info-type]
     (case info-type
       :class (simple-name (.getLineClass this))
       :name (.getName this)
       nil))))

(defn source? [^Port$Info port]
  (.isSource port))

(defn line-info
  ([this]
   (sound-info this))
  ([line-kind format]
   (DataLine$Info. (get line-key-class line-kind line-kind) format))
  ([line-kind format buffer-size]
   (if (number? buffer-size)
     (DataLine$Info. (get line-key-class line-kind line-kind) format buffer-size)
     (Port$Info. (get line-key-class line-kind line-kind) format buffer-size)))
  ([line-kind formats ^long min-buffer-size ^long max-buffer-size]
   (DataLine$Info. (get line-key-class line-kind line-kind)
                   (if (sequential? formats) (into-array AudioFormat formats) formats)
                   min-buffer-size max-buffer-size)))

(defn control
  ([^Line line]
   (.getControls line))
  ([^Line line ctrl-type]
   (.getControl line (get control-type ctrl-type ctrl-type))))

(defn line-class [info]
  (.getLineClass ^Line$Info (get port-info info info)))


(defn event [line event-type ^long position]
  (LineEvent. line event-type position))

(extend-type LineEvent
  Frame
  (frame-position [event]
    (.getFramePosition event))
  Type
  (itype [control]
    (.getType control)))

;; =================== DataLine ==========================================

(extend-type DataLine
  Info
  (info
    ([this]
     {:class (.getLineClass (.getLineInfo this))
      :status (if (.isOpen this) :open :closed)
      :level (.getLevel this)
      :active (.isActive this)
      :running (.isRunning this)})
    ([this info-type]
     (case info-type
       :class (.getLineClass (.getLineInfo this))
       :status (if (.isOpen this) :open :closed)
       :level (.getLevel this)
       :active (.isActive this)
       :running (.isRunning this))))
  Format
  (get-format [line]
    (.getFormat line))
  Frame
  (frame-position [line]
    (.getLongFramePosition line))
  Available
  (available [line]
    (.available line))
  Timing
  (micro-position [line]
    (.getMicrosecondPosition line))
  Activity
  (running? [line]
    (.isRunning line))
  (active? [line]
    (.isActive line))
  (start! [line!]
    (.start line!)
    line!)
  (stop! [line!]
    (.stop line!)
    line!))

(defn formats [^DataLine$Info info]
  (.getFormats info))

(defn max-buffer-size ^long [^DataLine$Info info]
  (.getMaxBufferSize info))

(defn min-buffer-size ^long [^DataLine$Info info]
  (.getMinBufferSize info))

(defn drain! [^DataLine line]
  (.drain line)
  line)

(defn flush! [^DataLine line]
  (.flush line)
  line)

(defn buffer-size ^long [^DataLine line]
  (.getBufferSize line))

(defn level ^double [^DataLine line]
  (.getLevel line))

;; ====================== Clip =================================================

(extend-type Clip
  Open
  (open!
    ([clip stream]
     (.open clip ^AudioInputStream stream)
     clip)
    ([clip format data offset buffer-size]
     (.open clip ^AudioFormat format data offset buffer-size)
     clip))
  (open? [clip]
    (.isOpen clip))
  Frame
  (frame-length [clip]
    (.getFrameLength clip))
  Timing
  (micro-length [clip]
    (.getMicrosecondLength clip))
  (micro-position [line]
    (.getMicrosecondPosition line))
  (micro-position! [clip microseconds]
    (.setMicrosecondPosition clip microseconds)))

(defn clip
  ([]
   (AudioSystem/getClip))
  ([^Mixer$Info mixer-info]
   (AudioSystem/getClip mixer-info)))

(defn frame-position! [^Clip clip ^long frames]
  (.setFramePosition clip frames)
  clip)

(defn loop-points! [^Clip clip ^long start ^long end]
  (.setLoopPoints clip start end)
  clip)

(defn loop!
  ([^Clip clip]
   (.loop clip Clip/LOOP_CONTINUOUSLY)
   clip)
  ([^Clip clip ^long count]
   (.loop clip count)
   clip))

;; ====================== SourceDataLine =================================================

(extend-type SourceDataLine
  Open
  (open!
    ([line format]
     (.open line ^AudioFormat format)
     line)
    ([line format buffer-size]
     (.open line ^AudioFormat format buffer-size)
     line))
  (open? [line]
    (.isOpen line)))

(defmethod write! [AudioInputStream File]
  [in out! file-type]
  (AudioSystem/write ^AudioInputStream in ^AudioFileFormat$Type file-type ^File out!))

(defmethod write! [AudioInputStream OutputStream]
  [in out! file-type]
  (AudioSystem/write ^AudioInputStream in ^AudioFileFormat$Type file-type ^OutputStream out!))

(defmethod write! [(Class/forName "[B") SourceDataLine]
  [byte-arr line! offset length]
  (.write ^SourceDataLine line! byte-arr offset length))

;; ====================== TargetDataLine =================================================

(extend-type TargetDataLine
  Open
  (open!
    ([line format]
     (.open line ^AudioFormat format)
     line)
    ([line format buffer-size]
     (.open line ^AudioFormat format buffer-size)
     line))
  (open? [line]
    (.isOpen line)))

;; =========================== Mixer ===========================================

(extend-type Mixer$Info
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

(extend-type Mixer
  Info
  (info
    ([this]
     (merge {:class (simple-name (class this))
             :status (if (.isOpen this) :open :closed)}
            (info (.getMixerInfo this))))
    ([this info-type]
     (case info-type
       :class (simple-name (class this))
       :status (if (.isOpen this) :open :closed)
       (info (.getMixerInfo this) info-type))))
  SoundInfoProvider
  (sound-info [mixer]
    (.getMixerInfo mixer))
  Support
  (supported [this info]
    (.isLineSupported this (get port-info info info))))

(defn mixer-info
  ([]
   (AudioSystem/getMixerInfo))
  ([^Mixer mixer]
   (.getMixerInfo mixer)))

(defn mixer
  ([]
   (map mixer (mixer-info)))
  ([^Mixer$Info info]
   (AudioSystem/getMixer info)))

(defn max-lines [^Mixer mixer info]
  (.getMaxLines mixer (get port-info info info)))

(defn line
  ([obj]
   (if (instance? LineEvent obj)
     (.getLine ^LineEvent obj)
     (AudioSystem/getLine (get port-info obj obj))))
  ([^Mixer mixer info]
   (.getLine mixer (get port-info info info))))

(defn source-info
  ([this]
   (if (instance? Line$Info this)
     (AudioSystem/getSourceLineInfo (get port-info this this))
     (.getSourceLineInfo ^Mixer this)))
  ([^Mixer mixer info]
   (.getSourceLineInfo mixer (get port-info info info))))

(defn source
  ([^Mixer mixer]
   (.getSourceLines mixer))
  ([^Mixer mixer info]
   (map (partial line mixer) (source-info mixer info))))

(defn target-info
  ([this]
   (if (instance? Line$Info this)
     (AudioSystem/getTargetLineInfo (get port-info this this))
     (.getTargetLineInfo ^Mixer this)))
  ([^Mixer mixer info]
   (.getTargetLineInfo mixer (get port-info info info))))

(defn target
  ([^Mixer mixer]
   (.getTargetLines mixer))
  ([^Mixer mixer info]
   (map (partial line mixer) (target-info mixer info))))

(defn sync-supported?
  ([mixer lines]
   (sync-supported? mixer lines true))
  ([^Mixer mixer lines maintain-sync?]
   (.isSynchronizationSupported mixer (if (sequential? lines) (into-array Line lines) lines)
                                maintain-sync?)))

(extend-protocol Support
  (Class/forName "[Ljavax.sound.sampled.Line;")
  (supported [lines mixer]
    (sync-supported? mixer lines true))
  Sequential
  (supported [lines mixer]
    (sync-supported? mixer (into-array Line lines) true)))

(defn sync! [^Mixer mixer! lines maintain-sync?]
  (.synchronize mixer! (if (sequential? lines) (into-array Line lines) lines) maintain-sync?))

(defn unsync! [^Mixer mixer! lines]
  (.unsynchronize mixer! (if (sequential? lines) (into-array Line lines) lines)))

;; ====================== AudioPermission ======================================

(extend-type AudioPermission
  Info
  (info
    ([this]
     {:name (.getName this)})
    ([this info-type]
     (case info-type
       :name (.getName this)
       nil))))

(defn audio-permission [permission]
  (if (instance? AudioPermission permission)
    permission
    (try (AudioPermission. (#{"play" "record" "*"} (name permission)))
         (catch NullPointerException e
           (throw (ex-info (format "Unsupported permission: %s." permission)
                           {:type :permission-error
                            :requested permission :supported #{:play :record "play" "record"}}))))))

(defn implies [^Permission this other]
  (.implies this (audio-permission other)))

;; ================== AudioFormat ======================================

(extend-type AudioFormat$Encoding
  Info
  (info
    ([this]
     {:name (.toString this)})
    ([this info-type]
     (case info-type
       :name (.toString this)))))

(extend-type AudioFormat
  Info
  (info
    ([this]
     (into {:encoding (.toString (.getEncoding this))}
           (map (fn [[k v]] [(name-key k) v]) (properties this))))
    ([this info-type]
     (case info-type
       :encoding (.toString (.getEncoding this))
       (map (fn [[k v]] [(name-key k) v]) (properties this)))))
  Support
  (supported [af line]
    (.isFormatSupported ^DataLine$Info line af))
  Match
  (matches? [af other]
    (.matches af other))
  Format
  (property [af key]
    (.getProperty af (name key)))
  (properties [af]
    (.properties af)))

(defn audio-format
  ([from]
   (if (map? from)
     (let [{:keys [encoding sample-rate sample-size-bits channels
                   frame-size frame-rate signed endian properties]
            :or {channels 1 sample-size-bits 16 endian :little-endian signed :signed}} from]
       (if encoding
         (if properties
           (audio-format encoding sample-rate sample-size-bits channels frame-size
                         frame-rate endian properties)
           (audio-format encoding sample-rate sample-size-bits channels frame-size frame-rate))
         (audio-format sample-rate sample-size-bits channels signed endian)))
     (get-format from)))
  ([sample-rate sample-size-bits]
   (audio-format sample-rate sample-size-bits 1 :signed :little-endian))
  ([sample-rate sample-size-bits channels]
   (audio-format sample-rate sample-size-bits channels :signed :little-endian))
  ([sample-rate sample-size-bits channels signed endian]
   (AudioFormat. sample-rate sample-size-bits channels
                 (signed? signed) (big-endian? endian)))
  ([encoding sample-rate sample-size-bits channels frame-size frame-rate endian]
   (AudioFormat. (get audio-encoding encoding encoding)
                 sample-rate sample-size-bits channels frame-size frame-rate
                 (big-endian? endian)))
  ([encoding sample-rate sample-size-bits channels frame-size frame-rate endian properties]
   (AudioFormat. (get audio-encoding encoding encoding)
                 sample-rate sample-size-bits channels frame-size frame-rate
                 (big-endian? endian) (stringify-keys properties))))

(defn encoding [this]
  (if (instance? AudioFormat this)
    (.getEncoding ^AudioFormat this)
    (get audio-encoding this (AudioFormat$Encoding. (name this)))))

(defn channels [^AudioFormat format]
  (.getChannels format))

(defn frame-rate ^double [^AudioFormat format]
  (.getFrameRate format))

(defn frame-size ^long  [^AudioFormat format]
  (.getFrameSize format))

(defn sample-rate ^double [^AudioFormat format]
  (.getSampleRate format))

(defn sample-size-bits ^long  [^AudioFormat format]
  (.getSampleSizeInBits format))

(defn big-endian? [^AudioFormat format]
  (.isBigEndian format))

;; =================== AudioFileFormat =================================================

(extend-type AudioFileFormat$Type
  Info
  (info
    ([this]
     {:extension (.getExtension this)
      :name (.toString this)})
    ([this info-type]
     (case info-type
       :extension (.getExtension this)
       :name (.toString this)
       nil)))
  Support
  (supported [feature stream]
    (AudioSystem/isFileTypeSupported (get audio-file-format-type feature feature) stream)))

(defn extension [^AudioFileFormat$Type t]
  (.getExtension t))

(defn file-format-type
  ([aff]
   (if (instance? AudioFileFormat aff)
     (.getType ^AudioFileFormat aff)
     (get audio-file-format-type aff (file-format-type (name aff) aff))))
  ([name extension]
   (AudioFileFormat$Type. (str name) (name extension))))

(extend-type AudioFileFormat
  Info
  (info
    ([this]
     (into {:type (extension (itype this))}
           (map (fn [[k v]] [(name-key k) v]) (properties this))))
    ([this info-type]
     (case info-type
       :type (extension (itype this))
       (map (fn [[k v]] [(name-key k) v]) (properties this)))))
  Frame
  (frame-length [aff]
    (.getFrameLength aff))
  Type
  (itype [aff]
    (.getType aff))
  Format
  (get-format [aff]
    (.getFormat aff))
  (property [aff key]
    (.getProperty aff (name key)))
  (properties [aff]
    (.properties aff))
  (byte-length [aff]
    (.getByteLength aff)))

(defn audio-file-format
  ([this]
   (file-format this))
  ([type ^long byte-length format ^long frame-length]
   (AudioFileFormat. (file-format-type type) byte-length format frame-length))
  ([type format ^long frame-length]
   (AudioFileFormat. (file-format-type type) format frame-length))
  ([type args]
   (AudioFileFormat. (file-format-type type)
                     (:format args) (:frame-length args)
                     (stringify-keys (dissoc args :format :frame-length)))))

;; ========================== InputStream ================================================

(extend-type InputStream
  Available
  (available [stream]
    (.available stream))
  Support
  (supported [stream]
    (.markSupported stream))
  Reset
  (re-set! [stream!]
    (.reset stream!)
    stream!))

(defn mark! [^InputStream stream! ^long read-limit]
  (.mark stream! read-limit))

(defn read!
  (^long [^InputStream stream]
   (.read stream))
  (^long [^InputStream stream ^bytes array!]
   (.read stream array!))
  (^long [^InputStream stream ^bytes array! ^long offset, ^long length]
   (.read stream array! offset length)))

(defn skip! [^InputStream stream! long n]
  (.skip stream! n))

;; =================== AudioInputStream ================================================

(extend-type AudioInputStream
  Releaseable
  (release [this]
    (.close this)
    true)
  Format
  (get-format [this]
    (.getFormat this))
  Frame
  (frame-length [clip]
    (.getFrameLength clip)))

(defn audio-input
  ([stream format ^long length]
   (AudioInputStream. stream format length))
  ([line]
   (AudioInputStream. line)))

;; =================== Control ==================================================

(extend-type Control$Type
  Info
  (info
    ([this]
     {:name (.toString this)})
    ([this info-type]
     (case info-type
       :name (.toString this)
       nil)))
  Support
  (supported [this line]
    (.isControlSupported ^Line line this)))

(extend-type Control
  Info
  (info
    ([this]
     {:type (.toString (.getType this))})
    ([this info-type]
     (case info-type
       :type (.toString (.getType this))
       nil)))
  Type
  (itype [control]
    (.getType control)))

;; =================== BooleanControl ==================================================

(extend-type BooleanControl
  Value
  (value [bc]
    (.getValue bc))
  (value! [bc! val]
    (.setValue bc! val)))

(defn state-label [^BooleanControl control state]
  (.getStateLabel control state))

;; =================== CompoundControl ==================================================

(defn controls [^CompoundControl control]
  (.getMemberControls control))

;; =================== EnumControl ==================================================

(extend-type EnumControl
  Value
  (value [control]
    (.getValue control))
  (value! [control! val]
    (.setValue control! val)))

(defn values [^EnumControl control]
  (.getValues control))

;; =================== FloatControl ==================================================

(extend-type FloatControl
  Value
  (value [control]
    (.getValue control))
  (value! [control! val]
    (.setValue control! val)))

(defn maximum ^double [^FloatControl control]
  (.getMaximum control))

(defn max-label [^FloatControl control]
  (.getMaxLabel control))

(defn mid-label [^FloatControl control]
  (.getMidLabel control))

(defn minimum ^double [^FloatControl control]
  (.getMaximum control))

(defn min-label [^FloatControl control]
  (.getMinLabel control))

(defn precision ^double [^FloatControl control]
  (.getPrecision control))

(defn units [^FloatControl control]
  (.getUnits control))

(defn update-period ^long [^FloatControl control]
  (.getUpdatePeriod control))

(defn shift! [^FloatControl control ^double from ^double to ^long microseconds]
  (.shift control from to microseconds))

;; =================== ReverbType  =====================================================

(defn decay-time ^long [^ReverbType reverb]
  (.getDecayTime reverb))

(defn early-delay ^long [^ReverbType reverb]
  (.getEarlyReflectionDelay reverb))

(defn early-intensity ^double [^ReverbType reverb]
  (.getEarlyReflectionIntensity reverb))

(defn late-delay ^long [^ReverbType reverb]
  (.getLateReflectionDelay reverb))

(defn late-intensity ^double [^ReverbType reverb]
  (.getLateReflectionIntensity reverb))

(extend-type ReverbType
  Info
  (info
    ([this]
     {:name (.getName this)
      :decay-time (decay-time this)
      :early-delay (early-delay this)
      :early-intensity (early-intensity this)
      :late-delay (late-delay this)
      :late-intensity (late-intensity this)})
    ([this info-type]
     (case info-type
       :name (.getName this)
       :decay-time (decay-time this)
       :early-delay (early-delay this)
       :early-intensity (early-intensity this)
       :late-delay (late-delay this)
       :late-intensity (late-intensity this)
       nil))))

;; =================== User friendly printing ==========================================

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Mixer$Info;")
  [this ^java.io.Writer w]
  (.write w (pr-str (seq this))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Line$Info;")
  [this ^java.io.Writer w]
  (.write w (pr-str (seq this))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.DataLine$Info;")
  [this ^java.io.Writer w]
  (.write w (pr-str (seq this))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Port$Info;")
  [this ^java.io.Writer w]
  (.write w (pr-str (seq this))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Line;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.DataLine;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Clip;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.SourceDataLine;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.TargetDataLine;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Mixer;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.AudioFormat;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.AudioFormat$Encoding;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.AudioFileFormat;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.AudioFileFormat$Type;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Control;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.BooleanControl;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.CompoundControl;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.EnumControl;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.FloatControl;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljava.lang.Object;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method LineListener
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Line
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Line$Info
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method AudioFormat
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method AudioFormat$Encoding
  [this ^java.io.Writer w]
  (.write w (pr-str (name-key this))))

(defmethod print-method AudioFileFormat
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method AudioFileFormat$Type
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Control$Type
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method AudioPermission
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))




(defmethod print-method AudioInputStream
  [stream ^java.io.Writer w]
  (.write w (pr-str (bean stream))))

(defmethod print-method Control
  [control ^java.io.Writer w]
  (.write w (pr-str (bean control))))

(defmethod print-method Control$Type
  [type ^java.io.Writer w]
  (.write w (pr-str (name-key type))))

(defmethod print-method LineEvent
  [event ^java.io.Writer w]
  (.write w (pr-str (bean event))))

(defmethod print-method LineEvent$Type
  [type ^java.io.Writer w]
  (.write w (pr-str (name-key type))))

(defmethod print-method ReverbType
  [type ^java.io.Writer w]
  (.write w (pr-str (bean type))))
