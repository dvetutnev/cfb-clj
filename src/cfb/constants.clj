(ns cfb.constants)

(def u32size 4)

(def ENDOFCHAIN 0xFFFFFFFE)
(def FREESEC 0xFFFFFFFF)
(def FATSEC 0xFFFFFFFD)
(def DIFATSEC 0xFFFFFFFC)
(def NOSTREAM 0xFFFFFFFF)

(def SectorSize 512)
(def HeaderSize 512)
(def DirectoryEntrySize 128)
(def DirectoryEntryPeerSector (/ SectorSize DirectoryEntrySize))

(def StorageObject (byte 0x01))
(def StreamObject (byte 0x02))
(def RootStorageObject (byte 0x05))

