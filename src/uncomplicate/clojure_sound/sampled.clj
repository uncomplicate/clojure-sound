(ns uncomplicate.clojure-sound.sampled
  (:require [clojure.walk :refer [stringify-keys]]
            [uncomplicate.commons.core :refer [Releaseable]]
            [uncomplicate.clojure-sound
             [internal :refer [name-key Support]]
             [core :refer [write! Info InfoProvider Open Timestamp Reset Broadcast Activity Type
                           Format]]])
  (:import java.net.URL
           [java.io File InputStream OutputStream]
           [javax.sound.sampled AudioSystem AudioFormat AudioInputStream  AudioPermission
            AudioFormat AudioFormat$Encoding AudioFileFormat AudioFileFormat$Type Mixer Mixer$Info
            Line Line$Info DataLine DataLine$Info Port Port$Info SourceDataLine TargetDataLine Clip
            Control$Type Control BooleanControl BooleanControl$Type CompoundControl EnumControl
            EnumControl$Type FloatControl FloatControl$Type LineListener LineEvent LineEvent$Type
            ReverbType]))

(defprotocol Matches
  (matches? [this other]))

(defprotocol AudioSystemProcedures
  (afile-format [this])
  (audio-input-stream [this] [target-format source-stream])
  (target-encodings [this] [target-format source-stream])
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

(def global-const
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

(def big-endian?
  {:big-endian true
   :big true
   true true
   :little-endian false
   :little false
   false false})

(def signed?
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


(def line-event-type
  {:close LineEvent$Type/CLOSE
   :open LineEvent$Type/OPEN
   :start LineEvent$Type/START
   :stop LineEvent$Type/STOP})

;; =========================== AudioSystem ====================================

(extend-protocol AudioSystemProcedures
  File
  (afile-format [file]
    (AudioSystem/getAudioFileFormat file))
  (audio-input-stream [file]
    (AudioSystem/getAudioInputStream file))
  InputStream
  (afile-format [stream]
    (AudioSystem/getAudioFileFormat stream))
  (audio-input-stream [stream]
    (AudioSystem/getAudioInputStream stream))
  URL
  (afile-format [url]
    (AudioSystem/getAudioFileFormat url))
  (audio-input-stream [url]
    (AudioSystem/getAudioInputStream url))
  AudioFormat
  (audio-input-stream [target source]
    (AudioSystem/getAudioInputStream target ^AudioInputStream source))
  (target-encodings [source]
    (AudioSystem/getTargetEncodings source))
  (convertible? [target source]
    (AudioSystem/isConversionSupported target ^AudioFormat source))
  AudioFormat$Encoding
  (audio-input-stream [target source]
    (AudioSystem/getAudioInputStream target ^AudioInputStream source))
  (target-encodings [source]
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

(extend-type Line
  Releaseable
  (release [this]
    (.close this)
    true)
  Open
  (open [line]
    (.open line)
    line)
  (open? [line]
    (.isOpen line))
  InfoProvider
  (info [this]
    (.getLineInfo this))
  Broadcast
  (listen! [line! listener]
    (.addLineListener line! listener)
    line!)
  (ignore! [line! listener]
    (.removeLineListener line! listener)
    line!))

(extend-type Line$Info
  Matches
  (matches? [this other]
    (.matches this other))
  Support
  (supported [info]
    (AudioSystem/isLineSupported (get port-info info info))))

(defn control
  ([^Line line]
   (.getControls line))
  ([^Line line ^Control$Type control-type]
   (.getControl line control-type)))

(defn line-info
  ([this]
   (if (keyword? this)
     (port-info this)
     (.getLineInfo ^Line this)))
  ([line-kind format]
   (DataLine$Info. (get line-key-class line-kind line-kind) format))
  ([line-kind format buffer-size]
   (DataLine$Info. (get line-key-class line-kind line-kind) format buffer-size))
  ([line-kind formats min-buffer-size max-buffer-size]
   (DataLine$Info. (get line-key-class line-kind line-kind)
                   (if (sequential? formats) (into-array AudioFormat formats) formats)
                   min-buffer-size max-buffer-size)))

(defn line-class [info]
  (.getLineClass ^Line$Info (get port-info info info)))


(defn line-event [line event-type ^long position]
  (LineEvent. line event-type position))

(extend-type LineEvent
  Frame
  (frame-position [event]
    (.getFramePosition event))
  Type
  (mytype [control]
    (.getType control)))

;; =================== DataLine ==========================================

(extend-type DataLine
  Format
  (get-format [line]
    (.getFormat line))
  Frame
  (frame-position [line]
    (.getLongFramePosition line))
  Available
  (available [line]
    (.available line))
  Timestamp
  (ms-position [line]
    (.getMicrosecondPosition line))
  Activity
  (running? [line]
    (.isRunning line))
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

(defn active? [^DataLine line]
  (.isActive line))

;; ====================== Clip =================================================

(extend-type Clip
  Open
  (open [clip stream]
    (.open clip ^AudioInputStream stream)
    clip)
  (open [clip format data offset buffer-size]
    (.open clip ^AudioFormat format data offset buffer-size)
    clip)
  Frame
  (frame-length [clip]
    (.getFrameLength clip))
  Timestamp
  (ms-length [clip]
    (.getMicrosecondLength clip))
  (ms-position [line]
    (.getMicrosecondPosition line))
  (ms-position! [clip microseconds]
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
  (open [line format]
    (.open line ^AudioFormat format)
    line)
  (open [line format buffer-size]
    (.open line ^AudioFormat format buffer-size)
    line))

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
  (open [line format]
    (.open line ^AudioFormat format)
    line)
  (open [line format buffer-size]
    (.open line ^AudioFormat format buffer-size)
    line))

;; =========================== Port ============================================

(extend-type Port$Info
  Info
  (myname [info]
    (.getName info)))

(defn source? [^Port$Info port]
  (.isSource port))

;; =========================== Mixer ===========================================

(extend-type Mixer$Info
  Info
  (description [info]
    (.getDescription info))
  (myname [info]
    (.getName info))
  (vendor [info]
    (.getVendor info))
  (version [info]
    (.getVersion info)))

(extend-type Mixer
  Support
  (supported [this info]
    (.isLineSupported this (get port-info info info)))
  InfoProvider
  (info [mixer]
    (.getMixerInfo mixer))
  Info
  (description [mixer]
    (.getDescription (.getMixerInfo mixer)))
  (myname [mixer]
    (.getName (.getMixerInfo mixer)))
  (vendor [mixer]
    (.getVendor (.getMixerInfo mixer)))
  (version [mixer]
    (.getVersion (.getMixerInfo mixer))))

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

(extend-type (Class/forName "[Ljavax.sound.sampled.Line;")
  Support
  (supported [lines mixer]
    (sync-supported? mixer lines true)))

(defn sync! [^Mixer mixer! lines maintain-sync?]
  (.synchronize mixer! (if (sequential? lines) (into-array Line lines) lines) maintain-sync?))

(defn unsync! [^Mixer mixer! lines]
  (.unsynchronize mixer! (if (sequential? lines) (into-array Line lines) lines)))

;; ====================== AudioPermission ======================================

(defn audio-permission [permission]
  (try (AudioPermission. (#{"play" "record"} (name permission)))
       (catch NullPointerException e
         (throw (ex-info (format "Unsupported permission: %s." permission)
                         {:type :permission-error
                          :requested permission :supported #{:play :record "play" "record"}})))))

;; ================== AudioFormat ======================================

(extend-type AudioFormat
  Support
  (supported [af line]
    (.isFormatSupported ^DataLine$Info line af))
  Matches
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
     (let [{:keys [encoding sample-rate sample-size-bits channels frame-size frame-rate signed endian properties]
            :or {channels 1 endian :little-endian signed :signed}} from]
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

(defn channels [^AudioFormat format]
  (.getChannels format))

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

(extend-type AudioFileFormat
  Frame
  (frame-length [aff]
    (.getFrameLength aff))
  Type
  (mytype [aff]
    (.getType aff))
  Format
  (get-format [aff]
    (.getFormat aff))
  (property [aff key]
    (.getProperty aff (name key)))
  (byte-length [aff]
    (.getByteLength aff)))

(extend-type AudioFileFormat$Type
  Support
  (supported [feature stream]
    (AudioSystem/isFileTypeSupported (get audio-file-format-type feature feature) stream)))

(defn extension [^AudioFileFormat$Type t]
  (.getExtension t))

(defn aff-type
  ([aff]
   (if (instance? AudioFileFormat aff)
     (.getType ^AudioFileFormat aff)
     (audio-file-format-type aff)))
  ([name extension]
   (AudioFileFormat$Type. name extension)))

(defn audio-file-format
  ([this]
   (afile-format this))
  ([type ^long byte-length format ^long frame-length]
   (AudioFileFormat. (get audio-file-format-type type type) byte-length format frame-length))
  ([type format ^long frame-length]
   (AudioFileFormat. (get audio-file-format-type type type) format frame-length))
  ([type args]
   (AudioFileFormat. (get audio-file-format-type type type)
                     (:format args) (:frame-length args)
                     (stringify-keys (dissoc properties :format :frame-length)))))

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
  Support
  (supported [this line]
    (.isControlSupported ^Line line this)))

(extend-type Control
  Type
  (mytype [control]
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

(extend-type ReverbType
  Info
  (myname [reverb]
    (.getName reverb)))

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

;; =================== User friendly printing ==========================================

(defmethod print-method Line$Info
  [info ^java.io.Writer w]
  (.write w (pr-str (-> (bean info) (dissoc :class)
                        (update :lineClass line-class-key)))))

(defmethod print-method Mixer$Info
  [info ^java.io.Writer w]
  (.write w (pr-str (bean info))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Mixer$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Line$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.DataLine$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Port$Info;")
  [info ^java.io.Writer w]
  (.write w (pr-str (seq info))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Line;")
  [lines ^java.io.Writer w]
  (.write w (pr-str (seq lines))))

(defmethod print-method (Class/forName "[Ljavax.sound.sampled.Mixer;")
  [mixers ^java.io.Writer w]
  (.write w (pr-str (seq mixers))))

(defmethod print-method AudioFormat
  [af ^java.io.Writer w]
  (.write w (pr-str (bean af))))

(defmethod print-method AudioFileFormat
  [aff ^java.io.Writer w]
  (.write w (pr-str (bean aff))))

(defmethod print-method AudioFormat$Encoding
  [enc ^java.io.Writer w]
  (.write w (pr-str (name-key enc))))

(defmethod print-method AudioFileFormat$Type
  [type ^java.io.Writer w]
  (.write w (pr-str (name-key type))))

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
