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
             [internal :refer [name-key key-name Support supported simple-name GetFormat get-format
                               extend-array-info]]
             [core :refer [write! read! SoundInfoProvider Open Timing Reset Broadcast Activity Type
                           Format SoundSystemProcedures file-format itype properties
                           sound-info Available Channels]]])
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
  (encodings [this] [target-format source-stream]))

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

(def port-info-key
  {Port$Info/MICROPHONE :microphone
   Port$Info/HEADPHONE :headphone
   Port$Info/LINE_IN :line-in
   Port$Info/LINE_OUT :line-out
   Port$Info/SPEAKER :speaker
   Port$Info/COMPACT_DISC :compact-disc})

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

(def boolean-control
  {:apply-reverb BooleanControl$Type/APPLY_REVERB
   :mute BooleanControl$Type/MUTE})

(def enum-control
  {:reverb EnumControl$Type/REVERB})

(def float-control
  {:aux-return FloatControl$Type/AUX_RETURN
   :aux-send FloatControl$Type/AUX_SEND
   :balance FloatControl$Type/BALANCE
   :master-gain FloatControl$Type/MASTER_GAIN
   :pan FloatControl$Type/PAN
   :reverb-return FloatControl$Type/REVERB_RETURN
   :reverb-send FloatControl$Type/REVERB_SEND
   :sample-rate FloatControl$Type/SAMPLE_RATE
   :volume FloatControl$Type/VOLUME
   :vol FloatControl$Type/VOLUME})

(def control-type
  (merge boolean-control enum-control float-control))

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

(def control-class
  {:boolean BooleanControl$Type
   :enum EnumControl$Type
   :float FloatControl$Type})

(def control-class-key
  {BooleanControl$Type :boolean
   EnumControl$Type :enum
   FloatControl$Type :float})

(def line-event-type
  {:close LineEvent$Type/CLOSE
   :open LineEvent$Type/OPEN
   :start LineEvent$Type/START
   :stop LineEvent$Type/STOP})

(def line-event-type-key
  {LineEvent$Type/CLOSE :close
   LineEvent$Type/OPEN :open
   LineEvent$Type/START :start
   LineEvent$Type/STOP :stop})

(def sampled-type
  (merge port-info line-key-class audio-encoding audio-file-format-type
         control-type control-class line-event-type))

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
  AudioFormat$Encoding
  (audio-input-stream [target source]
    (AudioSystem/getAudioInputStream target ^AudioInputStream source))
  (encodings [source]
    (AudioSystem/getTargetEncodings source)))

(defn audio-file-types
  ([^AudioInputStream stream]
   (AudioSystem/getAudioFileTypes stream))
  ([]
   (AudioSystem/getAudioFileTypes)))

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

(deftype LineListenerFunction [f match-event?]
  Info
  (info [_]
    {:fn f})
  (info [_ info-type]
   (case info-type
     :fn f
     nil))
  LineListener
  (update [_ event]
    (when (match-event? (.getType event))
      (f event))))

(deftype LineListenerWrapper [^LineListener f match-event?]
  Info
  (info [_]
    {:fn f})
  (info [_ info-type]
    (case info-type
      :fn f
      nil))
  LineListener
  (update [_ event]
    (when (match-event? (.getType event))
      (.update f event))))

(defn line-listener
  ([selection f]
   (let [selection (cond (seqable? selection)
                         (apply hash-set (map #(get line-event-type % %) selection))
                         selection (partial = (get line-event-type selection selection))
                         :default (constantly true))]
     (if (instance? LineListener f)
       (->LineListenerWrapper f selection)
       (->LineListenerFunction f selection))))
  ([f]
   (if (instance? LineListener f)
     f
     (->LineListenerFunction f (constantly true)))))

(extend-type Line
  Info
  (info
    ([this]
     {:class (simple-name (.getLineClass (.getLineInfo this)))
      :status (if (.isOpen this) :open :closed)})
    ([this info-type]
     (case info-type
       :class (simple-name (.getLineClass (.getLineInfo this)))
       :status (if (.isOpen this) :open :closed))))
  Releaseable
  (release [this]
    (close! this)
    true)
  SoundInfoProvider
  (sound-info [this]
    (.getLineInfo this))
  Open
  (open! [line!]
    (.open line!)
    line!)
  (open? [line]
    (.isOpen line))
  Broadcast
  (listen!
    ([line! listener]
     (let [listener (line-listener listener)]
       (.addLineListener line! (line-listener listener))
       listener))
    ([line! listener selection]
     (let [listener (line-listener selection listener)]
       (.addLineListener line! (line-listener listener))
       listener)))
  (ignore! [line! listener]
    (.removeLineListener line! listener)
    line!))

(extend-type java.lang.Class
  SoundInfoProvider
  (sound-info [c]
    (Line$Info. c)))

(extend-type Keyword
  SoundInfoProvider
  (sound-info [kw]
    (get sampled-type kw (ex-info "Unknown port info." {:type :sound-error
                                                        :requested kw
                                                        :supported (keys sampled-type)})))
  Support
  (supported
    ([kw]
     (AudioSystem/isLineSupported (sound-info kw)))
    ([kw obj]
     (supported (get sampled-type kw kw) obj))))

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
  (supported
    ([info]
     (AudioSystem/isLineSupported info))
    ([info mxr]
     (.isLineSupported ^Mixer mxr info))))

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
   (sound-info (sound-info this)))
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
  (.getLineClass ^Line$Info (sound-info info)))

(extend-type LineEvent$Type
  Info
  (info
    ([this]
     {:name (.toString this)})
    ([this info-type]
     (case info-type
       :name (.toString this)
       nil))))

(extend-type LineEvent
  Info
  (info
    ([this]
     {:type (itype this)
      :position (frame-position this)})
    ([this info-type]
     (case info-type
       :type (itype this)
       :position (frame-position this)
       nil)))
  Frame
  (frame-position [event]
    (.getFramePosition event))
  Type
  (itype [event]
    (let [event-type (.getType event)]
      (get line-event-type-key event-type event-type))))

(defn line-event [line event-type ^long position]
  (LineEvent. line (line-event-type event-type event-type) position))

;; =================== DataLine ==========================================

(extend-type DataLine
  Info
  (info
    ([this]
     {:class (simple-name (.getLineClass (.getLineInfo this)))
      :status (if (.isOpen this) :open :closed)
      :level (.getLevel this)
      :active (.isActive this)
      :running (.isRunning this)})
    ([this info-type]
     (case info-type
       :class (simple-name (.getLineClass (.getLineInfo this)))
       :status (if (.isOpen this) :open :closed)
       :level (.getLevel this)
       :active (.isActive this)
       :running (.isRunning this))))
  GetFormat
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
    ([clip!]
     (try
       (.open clip!)
       (catch IllegalArgumentException e
         (ex-info "Clip does not support 1-argument open!."
                  {:type :sound-error
                   :cause e})))
     clip!)
    ([clip! stream]
     (.open clip! ^AudioInputStream stream)
     clip!)
    ([clip! format data offset buffer-size]
     (.open clip! ^AudioFormat format data offset buffer-size)
     clip!))
  (open? [clip]
    (.isOpen clip))
  Frame
  (frame-length [clip]
    (.getFrameLength clip))
  (frame-position [clip]
    (.getFramePosition clip))
  Timing
  (micro-length [clip]
    (.getMicrosecondLength clip))
  (micro-position [line]
    (.getMicrosecondPosition line))
  (micro-position! [clip microseconds]
    (.setMicrosecondPosition clip microseconds)
    clip))

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
    ([line!]
     (.open line!)
     line!)
    ([line! format-or-buffer-size]
     (if (number? format-or-buffer-size)
       (.open line! ^AudioFormat (get-format line!) format-or-buffer-size)
       (.open line! ^AudioFormat format-or-buffer-size))
     line!)
    ([line! format buffer-size]
     (.open line! ^AudioFormat format buffer-size)
     line!))
  (open? [line]
    (.isOpen line)))

(defmethod write! [AudioInputStream File]
  [^AudioInputStream in ^File out! ^AudioFileFormat$Type file-type]
  (AudioSystem/write in file-type out!))

(defmethod write! [AudioInputStream OutputStream]
  [^AudioInputStream in ^OutputStream out! ^AudioFileFormat$Type file-type]
  (AudioSystem/write in file-type out!))

(defmethod write! [(Class/forName "[B") SourceDataLine Number Number]
  [byte-arr ^SourceDataLine line! offset length]
  (.write line! byte-arr offset length))

(defmethod write! [(Class/forName "[B") SourceDataLine Number]
  [^bytes byte-arr ^SourceDataLine line! ^long offset]
 (.write line! byte-arr offset (- (long (alength byte-arr)) offset)))

(defmethod write! [(Class/forName "[B") SourceDataLine]
  [^bytes byte-arr ^SourceDataLine line!]
  (.write line! byte-arr 0 (alength byte-arr)))

;; ====================== TargetDataLine =================================================

(extend-type TargetDataLine
  Open
  (open!
    ([line!]
     (.open line!)
     line!)
    ([line! format-or-buffer-size]
     (if (number? format-or-buffer-size)
       (.open line! ^AudioFormat (get-format line!) format-or-buffer-size)
       (.open line! ^AudioFormat format-or-buffer-size))
     line!)
    ([line! format buffer-size]
     (.open line! ^AudioFormat format buffer-size)
     line!))
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
    (.isLineSupported this (sound-info info))))

(defn mixer-info
  ([]
   (AudioSystem/getMixerInfo))
  ([^Mixer mixer]
   (.getMixerInfo mixer)))

(defn mixer
  ([]
   (AudioSystem/getMixer nil))
  ([^Mixer$Info info]
   (AudioSystem/getMixer info)))

(defn max-lines [^Mixer mixer info]
  (.getMaxLines mixer (sound-info info)))

(defn line
  ([obj]
   (if (instance? LineEvent obj)
     (.getLine ^LineEvent obj)
     (AudioSystem/getLine (sound-info obj))))
  ([^Mixer mixer info]
   (.getLine mixer (sound-info info))))

(defn source-info
  ([this]
   (if (instance? Mixer this)
     (.getSourceLineInfo ^Mixer this)
     (AudioSystem/getSourceLineInfo (sound-info this))))
  ([^Mixer mixer info]
   (.getSourceLineInfo mixer (sound-info info))))

(defn source
  ([this]
   (if (instance? Mixer this)
     (map (partial line this) (.getSourceLineInfo ^Mixer this))
     (map line (AudioSystem/getSourceLineInfo this))))
  ([mixer info]
   (map (partial line mixer) (source-info mixer info))))

(defn open-sources [^Mixer mixer]
  (.getSourceLines mixer))

(defn target-info
  ([this]
   (if (instance? Line$Info this)
     (AudioSystem/getTargetLineInfo (sound-info this))
     (.getTargetLineInfo ^Mixer this)))
  ([^Mixer mixer info]
   (.getTargetLineInfo mixer (sound-info info))))

(defn target
  ([this]
   (if (instance? Mixer this)
     (map (partial line this) (.getTargetLineInfo ^Mixer this))
     (map line (AudioSystem/getTargetLineInfo this))))
  ([mixer info]
   (map (partial line mixer) (target-info mixer info))))

(defn open-targets [^Mixer mixer]
  (.getTargetLines mixer))

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

(defn sync!
  ([^Mixer mixer! lines maintain-sync?]
   (.synchronize mixer! (if (sequential? lines) (into-array Line lines) lines) maintain-sync?))
  ([mixer! lines]
   (sync! mixer! lines true)))

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
     {:name (name-key (.toString this))})
    ([this info-type]
     (case info-type
       :name (name-key (.toString this)))))
  Support
  (supported [target source]
    (AudioSystem/isConversionSupported target ^AudioFormat source)))

(extend-type AudioFormat
  Info
  (info
    ([this]
     (into {:encoding (name-key (.toString (.getEncoding this)))}
           (map (fn [[k v]] [(name-key k) v]) (properties this))))
    ([this info-type]
     (case info-type
       :encoding (name-key (.toString (.getEncoding this)))
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
    (.properties af))
  GetFormat
  (get-format [this]
    this)
  Support
  (support [target source]
    (AudioSystem/isConversionSupported target ^AudioFormat source))
  Channels
  (channels [format]
    (.getChannels format)))

(defn audio-format
  ([from]
   (get-format from))
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

(extend-type java.util.Map
  Format
  (property [this key]
    (.get this (name key)))
  (properties [this]
    this)
  GetFormat
  (get-format [from]
    (let [{:keys [encoding sample-rate sample-size-bits channels
                  frame-size frame-rate signed endian properties]
           :or {channels 1 sample-size-bits 16 endian :little-endian signed :signed}} from]
      (if encoding
        (if properties
          (audio-format encoding sample-rate sample-size-bits channels frame-size
                        frame-rate endian properties)
          (audio-format encoding sample-rate sample-size-bits channels frame-size frame-rate))
        (audio-format sample-rate sample-size-bits channels signed endian)))))

(defn encoding [this]
  (if (instance? AudioFormat this)
    (.getEncoding ^AudioFormat this)
    (get audio-encoding this (AudioFormat$Encoding. (name this)))))

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
  GetFormat
  (get-format [aff]
    (.getFormat aff))
  Format
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

(defmethod read! [InputStream]
  [^InputStream stream]
  (.read stream))

(defmethod read! [InputStream (Class/forName "[B")]
  [^InputStream stream array!]
  (.read stream array!))

(defmethod read! [InputStream (Class/forName "[B") Number Number]
  [^InputStream stream array! offset length]
  (.read stream array! offset length))

(defmethod read! [InputStream (Class/forName "[B") Number]
  [^InputStream stream ^bytes array! ^long offset]
  (.read stream array! offset (- (long (alength array!)) offset)))

(defmethod read! [(Class/forName "[B") InputStream]
  [array! ^InputStream stream]
  (.read stream array!))

(defmethod read! [(Class/forName "[B") InputStream Number Number]
  [array! ^InputStream stream offset length]
  (.read stream array! offset length))

(defmethod read! [(Class/forName "[B") InputStream Number]
  [^bytes array! ^InputStream stream ^long offset]
  (.read stream array! offset (- (long (alength array!)) offset)))

(defmethod read! [TargetDataLine (Class/forName "[B") Number Number]
  [^TargetDataLine line ^bytes array! offset length]
  (.read line array! offset length))

(defmethod read! [TargetDataLine (Class/forName "[B") Number]
  [^TargetDataLine line ^bytes array! ^long offset]
  (.read line array! offset (- (long (alength array!)) offset)))

(defmethod read! [TargetDataLine (Class/forName "[B")]
  [^TargetDataLine line ^bytes array!]
  (.read line array! 0 (long (alength array!))))

(defmethod read! [(Class/forName "[B") TargetDataLine Number Number]
  [^bytes array! ^TargetDataLine line offset length]
  (.read line array! offset length))

(defmethod read! [(Class/forName "[B") TargetDataLine Number]
  [^bytes array! ^TargetDataLine line ^long offset]
  (.read line array! offset (- (long (alength array!)) offset)))

(defmethod read! [(Class/forName "[B") TargetDataLine]
  [^bytes array! ^TargetDataLine line]
  (.read line array! 0 (long (alength array!))))


(defn skip! [^InputStream stream! ^long n]
  (.skip stream! n))

;; =================== AudioInputStream ================================================

(extend-type AudioInputStream
  Info
  (info
    ([this]
     {:format (info (.getFormat this))
      :length (.getFrameLength this)})
    ([this info-type]
     (case info-type
       :format (info (.getFormat this))
       :length (.getFrameLength this)
       nil)))
  Releaseable
  (release [this]
    (.close this)
    true)
  GetFormat
  (get-format [this]
    (.getFormat this))
  Frame
  (frame-length [this]
    (.getFrameLength this)))

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
     {:name (get control-type-key this (.toString this))
      :type (let [clazz (class this)]
               (get control-class-key clazz (simple-name clazz)))})
    ([this info-type]
     (case info-type
       :name (get control-type-key this (.toString this))
       :type (let [clazz (class this)]
                (get control-class-key clazz (simple-name clazz)))
       nil)))
  Support
  (supported [this line]
    (.isControlSupported ^Line line this))
  Type
  (itype [this]
    (class this)))

(extend-type Control
  Info
  (info
    ([this]
     {:type (let [ctrl-type (.getType this)]
              (get control-type-key ctrl-type ctrl-type))
      :value (value this)
      :kind (info (.getType this) :type)})
    ([this info-type]
     (let [t (itype this)]
       (case info-type
         :type (let [ctrl-type (.getType this)]
                 (get control-type-key ctrl-type ctrl-type))
         :value (value this)
         :kind (info (.getType this) :type)
         nil))))
  Type
  (itype [control]
    (.getType control)))

;; =================== BooleanControl ==================================================

(extend-type BooleanControl
  Value
  (value [this]
    (.getValue this))
  (value! [this! val]
    (.setValue this! val)
    this!))

(defn state-label [^BooleanControl control state]
  (.getStateLabel control state))

;; =================== CompoundControl ==================================================

(defn controls [^CompoundControl control]
  (.getMemberControls control))

;; =================== EnumControl ==================================================

(extend-type EnumControl
  Value
  (value [this]
    (.getValue this))
  (value! [this! val]
    (.setValue this! val)
    this!))

(defn values [^EnumControl control]
  (.getValues control))

;; =================== FloatControl ==================================================

(extend-type FloatControl
  Value
  (value [this]
    (.getValue this))
  (value! [this! val]
    (.setValue this! val)
    this!))

(defn maximum ^double [^FloatControl control]
  (.getMaximum control))

(defn max-label [^FloatControl control]
  (.getMaxLabel control))

(defn mid-label [^FloatControl control]
  (.getMidLabel control))

(defn minimum ^double [^FloatControl control]
  (.getMinimum control))

(defn min-label [^FloatControl control]
  (.getMinLabel control))

(defn precision ^double [^FloatControl control]
  (.getPrecision control))

(defn units [^FloatControl control]
  (.getUnits control))

(defn update-period ^long [^FloatControl control]
  (.getUpdatePeriod control))

(defn shift? [^FloatControl control]
  (< -1 (.getUpdatePeriod control)))

(defn shift!
  ([control ^long microseconds]
   (shift! control (minimum control) (maximum control) microseconds))
  ([control ^long to ^long microseconds]
   (shift! control (value control) to microseconds))
  ([^FloatControl control ^double from ^double to ^long microseconds]
   (.shift control from to microseconds)))

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

;; =================== Sequential info for sampled arrays ==============================

(extend-array-info (Class/forName "[Ljavax.sound.sampled.Mixer$Info;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Line$Info;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.DataLine$Info;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Port$Info;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Line;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.DataLine;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Clip;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Clip;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.SourceDataLine;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.TargetDataLine;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Mixer;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.AudioFormat;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.AudioFormat$Encoding;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.AudioFileFormat;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.AudioFileFormat$Type;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.Control;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.BooleanControl;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.CompoundControl;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.EnumControl;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.FloatControl;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.LineEvent;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.LineEvent$Type;"))
(extend-array-info (Class/forName "[Ljavax.sound.sampled.ReverbType;"))
(extend-array-info (Class/forName "[Ljava.lang.Object;"))

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

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.LineEvent;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.LineEvent$Type;")
  [this w]
  (print-method (seq this) w))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.ReverbType;")
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

(defmethod print-method Mixer$Info
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method AudioPermission
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

(defmethod print-method AudioInputStream
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Control$Type
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method Control
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method LineEvent
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method LineEvent$Type
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))

(defmethod print-method ReverbType
  [this ^java.io.Writer w]
  (.write w (pr-str (info this))))
