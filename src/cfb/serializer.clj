(ns cfb.serializer
  (:require [clojure.math :as math]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [cfb.constants :refer :all])
  (:import (java.nio ByteBuffer ByteOrder)))

(defn calc-num-sector
  ([length] (calc-num-sector length 1))
  ([length entry-size]
   (let [total-size (* length entry-size)
         num-full-sector (math/floor-div total-size SectorSize)]
     (if (zero? (mod total-size SectorSize))
       num-full-sector
       (inc num-full-sector)))))

(defn make-fat-chain [start length]
  (let [start (inc start)
        end (+ start (dec length))]
    (conj (vec (range start end)) ENDOFCHAIN)))

(defn make-proto-fat [sizes]
  (reduce (fn [[starts fat] size]
            (let [starts (conj starts (count fat))
                  chain (make-fat-chain (count fat) (calc-num-sector size))
                  fat (concat fat chain)]
              [starts fat]))
          [[] []] sizes))

(def difat-entry-in-header 109)

(defn calc-num-difat-sector [num-fat-sector]
  (if (<= num-fat-sector difat-entry-in-header)
    0
    (let [num-full-sector (math/floor-div (- num-fat-sector difat-entry-in-header) 127)]
      (if (zero? (mod (- num-fat-sector difat-entry-in-header) 127))
        num-full-sector
        (inc num-full-sector)))))

(def fat-entry-peer-sector (/ SectorSize u32size))

(defn make-fat [proto-fat]
  (loop [num-fat-sector (calc-num-sector (count proto-fat) u32size)]
    (if (> (+ num-fat-sector (count proto-fat))
           (* num-fat-sector fat-entry-peer-sector))
      (recur (inc num-fat-sector))
      (let [num-total-fat-entry (* num-fat-sector fat-entry-peer-sector)
            num-used-fat-entry (+ num-fat-sector (count proto-fat))
            num-pad-entry (- num-total-fat-entry num-used-fat-entry)
            start (+ (count proto-fat) num-pad-entry)]
        (println "count proto-fat: " (count proto-fat))
        (println "num-fat-sector: " num-fat-sector)
        (println "num-pad-entry: " num-pad-entry)
        (println "start (fat): " start)
        [(concat proto-fat
                 (long-array num-pad-entry FREESEC)
                 (long-array num-fat-sector FATSEC))
         start num-fat-sector num-pad-entry]))))

(defn calc-padding
  ([length] (calc-padding length SectorSize))
  ([length alignment]
   (let [m (mod length alignment)]
     (if (zero? m)
       0
       (- alignment m)))))

(defn make-difat-head [start length]
  {:pre [(<= length difat-entry-in-header)]}
  (concat (range start (+ start length))
          (long-array (- difat-entry-in-header length) FREESEC)))

(defn make-difat-tail [start-fat length start-difat]
  (let [arr (range start-fat (+ start-fat length))
        pad (long-array (calc-padding length 127) FREESEC)
        num-difat-sector (calc-num-difat-sector (+ length difat-entry-in-header))
        [res _ _] (->> (concat arr pad)
                       (partition 127)
                       (reduce (fn [[res current-difat-sector remaing] part]
                                 (let [next-difat-sector (if (zero? remaing)
                                                           FREESEC
                                                           (inc current-difat-sector))]
                                   [(concat res part [next-difat-sector])
                                    (inc current-difat-sector)
                                    (dec remaing)]))
                               [[] start-difat (dec num-difat-sector)]))]
    res))

(defn make-difat [start-fat length start-difat]
  (let [head-length (min length difat-entry-in-header)
        tail-length (if (< length difat-entry-in-header)
                      0
                      (- length difat-entry-in-header))]
    [(make-difat-head start-fat head-length)
     (make-difat-tail (+ start-fat difat-entry-in-header) tail-length start-difat)]))

(defn serialize-header [header]
  (let [^ByteBuffer buffer (ByteBuffer/allocate SectorSize)]
    (doto buffer
      (.order ByteOrder/LITTLE_ENDIAN)
      (.put (byte-array [0xD0 0xCF 0x11 0xE0 0xA1 0xB1 0x1A 0xE1])) ; Signature
      (.put (byte-array 16 (byte 0)))      ; CLSID
      (.putShort 0x003E)                   ; Minor version
      (.putShort 0x0003)                   ; Major version
      (.putShort (unchecked-short 0xFFFE)) ; Byte order
      (.putShort 0x0009)                   ; Sector size
      (.putShort 0x0006)                   ; Mini stream sector size
      (.putShort 0)                        ; Reserved
      (.putInt 0)                          ; Reserved
      (.putInt 0) ; Number of directory sector (not used for version 3)
      (.putInt (:num-fat-sector header)) ; Number of FAT sector
      (.putInt (:start-directory header)) ; Directory starting sector location
      (.putInt 0)                         ; Transaction signature
      (.putInt 0)                         ; Mini stream cutoff
      (.putInt (unchecked-int ENDOFCHAIN)) ; Mini FAT start sector location
      (.putInt 0)                          ; Number of mini FAT sector
      (.putInt (:start-difat-sector header)) ; DIFAT start sector location
      (.putInt (:num-difat-sector header)))  ; Number of DIFAT sector
    (doseq [entry (:difat-head header)]
      (.putInt buffer (unchecked-int entry)))
    (.array buffer)))

(defrecord Node [name child left right type size start])

(defn nil->default [default value]
  (if (nil? value) default value))

(def nil->0xFFFFFFFF (partial nil->default 0xFFFFFFFF))
(def nil->0 (partial nil->default 0))

(defn serialize-directory-entry [entry]
  (let [^ByteBuffer buffer (ByteBuffer/allocate DirectoryEntrySize)
        name (.getBytes (:name entry) "UTF-16LE")
        name-size (if (empty? name) 0 (+ (count name) 2))]
    (doto buffer
      (.order ByteOrder/LITTLE_ENDIAN)
      (.put name)
      (.putShort 0)                   ; Entry name terminator
      (.put (byte-array (- 64 (+ (count name) 2)) (byte 0))) ; Entry name padding
      (.putShort name-size) ; Entry name length with terminator
      (.put (:type entry))
      (.put (byte 0x01))             ; Color flag - black
      (.putInt (unchecked-int (nil->0xFFFFFFFF (:left entry))))
      (.putInt (unchecked-int (nil->0xFFFFFFFF (:right entry))))
      (.putInt (unchecked-int (nil->0xFFFFFFFF (:child entry))))
      (.put (byte-array 16 (byte 0x00))) ; CLSID
      (.putInt 0)                        ; State bits
      (.putLong 0)                       ; Creation time
      (.putLong 0)                       ; Modified time
      (.putInt (unchecked-int (nil->0 (:start entry))))
      (.putLong (unchecked-long (nil->0 (:size entry)))))
    (.array buffer)))

(defn serialize-fat [fat]
  (let [^ByteBuffer buffer (ByteBuffer/allocate (* (count fat) u32size))]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (doseq [entry fat]
      (.putInt buffer (unchecked-int entry)))
    (.array buffer)))

(declare make-directory)

(defn make-cfb [output-path streams]
  (let [[starts strm-proto-fat] (make-proto-fat (map (comp count last) streams))
        directory (make-directory (map (fn [[path stream] start]
                                         [path (count stream) start]) streams starts))
        start-directory (count strm-proto-fat)
        num-directory-sector (calc-num-sector (count directory) DirectoryEntrySize)
        proto-fat (concat strm-proto-fat
                          (make-fat-chain start-directory num-directory-sector))
        [fat start-fat num-fat-sector num-pad-sector] (make-fat proto-fat)
        difat-head (make-difat-head start-fat num-fat-sector)
        header {:num-fat-sector num-fat-sector
                :start-directory start-directory
                :start-difat-sector (unchecked-int ENDOFCHAIN)
                :num-difat-sector 0
                :difat-head difat-head}]
    (with-open [out (io/output-stream output-path)]
      (.write out (serialize-header header))
      (doseq [[_ content] streams]
        (.write out content)
        (.write out (byte-array (calc-padding (count content)) (byte 0))))
      (doseq [entry directory]
        (.write out (serialize-directory-entry entry)))
      (doseq [_ (range (calc-padding (count directory) DirectoryEntryPeerSector))]
        (.write out (serialize-directory-entry (map->Node {:name "" :type (byte 0x00)}))))
      (doseq [_ (range num-pad-sector)]
        (.write out (byte-array SectorSize (byte 0))))
      (.write out (serialize-fat fat)))))

(defn add-node [directory parent-id direction node]
  (let [new-id (count directory)
        parent-node (nth directory parent-id)
        upd-parent-node (assoc parent-node direction new-id)]
    [(conj (assoc directory parent-id upd-parent-node) node)
     new-id]))

(defn insert-in-tree [directory root-id node]
  (loop [root-id root-id
         parent-id nil
         direction nil]
    (if (nil? root-id)
      (add-node directory parent-id direction node)
      (let [{:keys [name left right]} (directory root-id)
            cmp-res (compare (:name node) name)]
        (cond
          (< cmp-res 0) (recur left root-id :left)
          (> cmp-res 0) (recur right root-id :right)
          :else [directory root-id])))))

(defn insert-in-storage [directory storage-id node]
  (let [storage (nth directory storage-id)
        root-id (:child storage)]
    (if (nil? root-id)
      (add-node directory storage-id :child node)
      (insert-in-tree directory root-id node))))

(defn add-nodes-path [directory path]
  (let [[directory _] (reduce (fn [[directory storage-id] node]
                                (insert-in-storage directory storage-id node))
                              [directory 0] path)]
    directory))

(defn make-nodes-path [path size start]
  (let [path* (string/split path #"/")
        head (map #(map->Node {:name % :type StorageObject}) (drop-last path*))
        tail (map->Node {:name (last path*) :type StreamObject :size size :start start})]
    (concat head (list tail))))

(defn make-directory [items]
  (reduce (fn [directory [path size start]]
            (let [path* (make-nodes-path path size start)]
              (add-nodes-path directory path*)))
          [(map->Node {:name "Root Entry" :type RootStorageObject})] items))

(def strm1 (byte-array (+ 1 (* 8 SectorSize)) (byte \A)))
(def strm2 (byte-array (* 8 SectorSize) (byte \B)))
(def strm3 (byte-array (* 128 SectorSize) (byte \C)))

(defn test-cfb []
  (make-cfb "test.bin" [["a/b" strm1]
                        ["a/h" strm2]
                        ["c/a" strm2]
                        ["c/d" strm1]
                        ["e/f" strm3]]))
