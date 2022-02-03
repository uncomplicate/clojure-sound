(ns clojure.uncomplicate-sound.sampled
  (:require [clojure.string :as str]
            [uncomplicate.commons.core :refer [Releaseable]])
  (:import [javax.sound.sampled AudioSystem AudioFormat AudioInputStream Mixer Mixer$Info Line
            Line$Info DataLine DataLine$Info LineListener Port Port$Info SourceDataLine TargetDataLine Clip DataLine$Info
            Control$Type AudioPermission AudioFormat AudioFormat$Encoding]))

(defprotocol Open
  (open [line] [line buffer-size] [line format data offset buffer-size]))

(defprotocol Supported
  (supported [feature line]))

(defprotocol Matches
  (matches? [this other]))

(defn name-key [s]
  (-> (str/trim s)
      (str/replace " " "")
      str/lower-case
      (str/replace "_" "-")
      keyword))

;; ===================== Keyword coding ================================================

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

;; ===========================

(extend-type Control$Type
  Supported
  (supported [this line]
    (.isControlSupported ^Line line this)))

(defn supported?
  ([info]
   (AudioSystem/isLineSupported (get port-info info info)))
  ([line feature]
   (supported feature line)))

;; =========================== Line ============================================

(defn add-listener! [^Line line! listener]
  (.addLineListener line! listener))

(defn remove-listener! [^Line line! listener]
  (.removeLineListener line! listener))

(extend-type Line
  Releaseable
  (release [this]
    (.close this)
    true)
  Open
  (open [line]
    (.open line)
    line))

(extend-type Line$Info
  Matches
  (matches? [this other]
    (.matches this other)))

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

(defn open? [^Line line]
  (.isOpen line))

(defn line-class [info]
  (.getLineClass ^Line$Info (get port-info info info)))

;; =================== DataLine ==========================================

(defn formats [^DataLine$Info info]
  (.getFormats info))

(defn max-buffer-size ^long [^DataLine$Info info]
  (.getMaxBufferSize info))

(defn min-buffer-size ^long [^DataLine$Info info]
  (.getMinBufferSize info))

(defn available ^long [^DataLine line]
  (.available line))

(defn drain! [^DataLine line]
  (.drain line)
  line)

(defn flush! [^DataLine line]
  (.flush line)
  line)

(defn buffer-size ^long [^DataLine line]
  (.getBufferSize line))

(defn frame-position ^long [^DataLine line]
  (.getLongFramePosition line))

(defn microsecond-position ^long [^DataLine line]
  (.getMicrosecondPosition line))

(defn level ^double [^DataLine line]
  (.getLevel line))

(defn active? [^DataLine line]
  (.isActive line))

(defn running? [^DataLine line]
  (.isRunning line))

(defn start! [^DataLine line]
  (.start line)
  line)

(defn stop! [^DataLine line]
  (.stop line)
  line)

;; ====================== Clip =================================================

(extend-type Clip
  Open
  (open [clip stream]
    (.open clip ^AudioInputStream stream)
    clip)
  (open [clip format data offset buffer-size]
    (.open clip ^AudioFormat format data offset buffer-size)
    clip))

(defn frame-length ^long [^Clip clip]
  (.getFrameLength clip))

(defn microsecond-length ^long [^Clip clip]
  (.getMicrosecondLength clip))

(defn frame-position! [^Clip clip ^long frames]
  (.setFramePosition clip frames)
  clip)

(defn microsecond-position! [^Clip clip ^long microseconds]
  (.setMicrosecondPosition clip microseconds)
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

(defn write! ^long [^SourceDataLine line! ^bytes array ^long offset, ^long length]
  (.write line! array offset length))

;; ====================== TargetDataLine =================================================

(extend-type TargetDataLine
  Open
  (open [line format]
    (.open line ^AudioFormat format)
    line)
  (open [line format buffer-size]
    (.open line ^AudioFormat format buffer-size)
    line))

(defn read! ^long [^TargetDataLine line  ^bytes array! ^long offset, ^long length]
  (.read line array! offset length))

;; =========================== Port ============================================

(defn port-name [^Port$Info port]
  (.getName port))

(defn source? [^Port$Info port]
  (.isSource port))

;; =========================== Mixer ===========================================

(extend-type Mixer
  Supported
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
  ([info]
   (AudioSystem/getLine (get port-info info info)))
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
  Supported
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
  Supported
  (supported [this line]
    (.isFormatSupported ^DataLine$Info line this))
  Matches
  (matches? [this other]
    (.matches this other)))

(defmethod print-method AudioFormat
  [format ^java.io.Writer w]
  (.write w (pr-str (bean format))))

(defn audio-format
  ([from]
   (if (instance? DataLine from)
     (.getFormat ^DataLine from)
     (let [{:keys [encoding sample-rate sample-size-bits channels frame-size frame-rate signed endian properties]
            :or {channels 1 endian :little-endian signed :signed}} from]
       (if encoding
         (if properties
           (audio-format encoding sample-rate sample-size-bits channels frame-size
                         frame-rate endian properties)
           (audio-format encoding sample-rate sample-size-bits channels frame-size frame-rate))
         (audio-format sample-rate sample-size-bits channels signed endian)))))
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
                 (big-endian? endian) properties)))

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

(defn property [^AudioFormat format key]
  (.getProperty format (name key)))

(defn sample-rate ^double [^AudioFormat format]
  (.getSampleRate format))

(defn sample-size-bits ^long  [^AudioFormat format]
  (.getSampleSizeInBits format))

(defn big-endian? [^AudioFormat format]
  (.isBigEndian format))

(defn properties [^AudioFormat format]
  (.properties format))

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
