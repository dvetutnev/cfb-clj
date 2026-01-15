(ns cfb.parser
  (:import (java.nio.file Paths StandardOpenOption OpenOption)
           (java.nio.channels FileChannel)
           (java.nio ByteBuffer ByteOrder))
  (:require [cfb.constants :refer :all]
            [clojure.string :as string]))

(defn read-u8! [^ByteBuffer buffer]
  (-> buffer
      .get
      (bit-and 0xFF)))

(defn read-u16! [^ByteBuffer buffer]
  (-> buffer
      .getShort
      (bit-and 0xFFFF)))

(defn read-u32! [^ByteBuffer buffer]
  (-> buffer
      .getInt
      (bit-and 0xFFFFFFFF)))

(defn shift-position! [^ByteBuffer buffer n]
  (.position buffer (+ (.position buffer) n)))

(defn read-header! [^FileChannel f]
  (let [buffer (ByteBuffer/allocate HeaderSize)
        signature (byte-array 8)]
    (.read f buffer)
    (doto buffer
      (.order ByteOrder/LITTLE_ENDIAN)
      (.rewind)
      (.get signature))
    (assert (java.util.Arrays/equals signature
                                     (byte-array [0xD0 0xCF 0x11 0xE0 0xA1 0xB1 0x1A 0xE1])))
    (shift-position! buffer 16) ; Skip CLSID
    (let [header (apply hash-map [:minor-version (read-u16! buffer)
                                  :major-version (read-u16! buffer)
                                  :byte-order (read-u16! buffer)
                                  :sector-shift (read-u16! buffer)
                                  :mini-sector-shift (read-u16! buffer)
                                  :num-fat-sector (do ; skip reserved and numdirectory sector
                                                    (shift-position! buffer 10)
                                                    (read-u32! buffer))
                                  :start-directory-sector (read-u32! buffer)
                                  :mini-stream-cutoff (do ; skip transaction signature
                                                        (shift-position! buffer 4)
                                                        (read-u32! buffer))
                                  :start-minifat (read-u32! buffer)
                                  :num-minifat-sector (read-u32! buffer)
                                  :start-difat-sector (read-u32! buffer)
                                  :num-difat-sector (read-u32! buffer)])

          difat (let [difat (transient [])]
                  (doseq [_ (range (min (:num-fat-sector header) 109))]
                    (conj! difat (read-u32! buffer)))
                  (persistent! difat))]

      (assert (= (:minor-version header) 0x003E))
      (assert (= (:major-version header) 0x0003))
      (assert (= (:byte-order header) 0xFFFE))
      (assert (= (:sector-shift header) 0x0009))
      (assoc header :difat difat))))

(defn sector->offset [n]
  (* (+ n 1) SectorSize))

(defn read-difat-tail [start-difat]
  [])

(defn read-fat [^FileChannel f difat]
  (let [buffer (ByteBuffer/allocate SectorSize)
        fat (transient [])]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (doseq [n difat]
      (.position f (sector->offset n))
      (.clear buffer)
      (.read f buffer)
      (.rewind buffer)
      (doseq [_ (range (/ SectorSize u32size))]
        (conj! fat (read-u32! buffer))))
    (persistent! fat)))

(defn read-directory-entry-name! [^ByteBuffer buffer]
  (let [name (byte-array 64 (byte 0x00))]
    (.position buffer 0)
    (.get buffer name)
    (let [len (read-u16! buffer)]
      (if (> len 0)
        (String. name 0 (- len 2) "UTF-16LE")
        (String.)))))

(defn read-directory-entry-type! [^ByteBuffer buffer]
  (let [type (read-u8! buffer)]
    (case type
      1 :storage
      2 :stream
      5 :root
      :unknown)))

(defn read-directory-sector! [^FileChannel f sector]
  (let [buffer (ByteBuffer/allocate 128)
        entries (transient [])]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (.position f (sector->offset sector))
    (doseq [_ (range DirectoryEntryPeerSector)]
      (.clear buffer)
      (.read f buffer)
      (.rewind buffer)
      (let [entry (apply hash-map [:name (read-directory-entry-name! buffer)
                                   :type (read-directory-entry-type! buffer)
                                   :color (read-u8! buffer)
                                   :left (read-u32! buffer)
                                   :right (read-u32! buffer)
                                   :child (read-u32! buffer)
                                   :start (do (shift-position! buffer (+ 16 ; CLSID
                                                                         4 ; State bits
                                                                         8 ; Creation time
                                                                         8)) ; Modified time
                                              (read-u32! buffer))
                                   :size (read-u32! buffer)])]
        (conj! entries entry)))
    (persistent! entries)))

(defn read-directory-stream! [^FileChannel file fat start]
  (let [entries (transient [])]
    (loop [sector start]
      (if (= sector ENDOFCHAIN)
        (persistent! entries)
        (do
          (doseq [entry (read-directory-sector! file sector)]
            (conj! entries entry))
          (recur (nth fat sector)))))))

(defn parse-directory-stream
  ([directory-stream] (parse-directory-stream directory-stream 0))
  ([directory-stream root-id]
   (if (= root-id NOSTREAM)
     {}
     (let [obj (nth directory-stream root-id)
           entry (if (or (= (:type obj) :storage)
                         (= (:type obj) :root))
                   (merge {:type :storage} (parse-directory-stream directory-stream (:child obj)))
                   (select-keys obj [:type :start :size]))]
       (merge {(:name obj) entry}
              (parse-directory-stream directory-stream (:left obj))
              (parse-directory-stream directory-stream (:right obj)))))))

(defn locate-first-sector [fat start offset]
  (loop [start start
         offset offset]
    (if (>= offset SectorSize)
      (recur (nth fat start)
             (- offset SectorSize))
      start)))

(defprotocol CFBStreamProtocol
  (read-stream [this] [this length] [this offset length]))

(deftype CFBStream [file fat start stream-size]
  CFBStreamProtocol
  (read-stream [this] (read-stream this 0 stream-size))
  (read-stream [this length] (read-stream this 0 length))
  (read-stream [this offset length]
    (let [result-buffer (byte-array length (byte 0))]
      (loop [result-buffer-pos 0
             remaing length
             sector (locate-first-sector fat start offset)
             offset (mod offset SectorSize)]
        (when (> remaing 0)
          (let [read-length (- (min remaing SectorSize) offset)
                fbuf (ByteBuffer/allocate read-length)]
            (.position file (+ (sector->offset sector) offset))
            (.read file fbuf)
            (.rewind fbuf)
            (.get fbuf result-buffer result-buffer-pos read-length)
            (recur (+ result-buffer-pos read-length)
                   (- remaing read-length)
                   (nth fat sector)
                   0))))
      result-buffer)))

(defprotocol CFBProtocol
  (open-stream [this path]))

(deftype CFB [file header fat directory]
  CFBProtocol
  (open-stream [this path]
    (let [p (string/split (str "Root Entry/" path) #"/")
          {:keys [start size]} (get-in directory p)]
      (CFBStream. file fat start size))))

(defn open-cfb [^String path]
  (let [p (Paths/get path (into-array String []))
        file (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))
        header (read-header! file)
        difat (concat (:difat header) (read-difat-tail (:start-difat-sector header)))
        fat (read-fat file difat)
        directory-stream (read-directory-stream! file fat (:start-directory-sector header))
        directory (parse-directory-stream directory-stream)]
    (CFB. file header fat directory)))

(defn dump-header [^String path]
  (let [p (Paths/get path (into-array String []))
        file (FileChannel/open p (into-array OpenOption [StandardOpenOption/READ]))]
    (read-header! file)))
