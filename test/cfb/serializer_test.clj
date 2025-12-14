(ns cfb.serializer-test
  (:require [clojure.test :refer :all]
            [cfb.serializer :refer :all]
            [cfb.constants :refer :all]))

(deftest test-make-directory
  (let [expected [(map->Node {:name "Root Entry" :type RootStorageObject :child 1})
                  (map->Node {:name "B" :type StorageObject :child 2 :left 6})
                  (map->Node {:name "C" :type StorageObject :child 3 :right 4})
                  (map->Node {:name "Stream" :type StreamObject :size 4095 :start 0})
                  (map->Node {:name "D" :type StorageObject :child 5})
                  (map->Node {:name "Stream" :type StreamObject :size 4096 :start 8})
                  (map->Node {:name "AStream" :type StreamObject :size 4097 :start 16})]
        directory (make-directory [["B/C/Stream" 4095 0]
                                   ["B/D/Stream" 4096 8]
                                   ["AStream" 4097 16]])]
    (is (= expected directory))))

(deftest test-make-proto-fat
  (let [[starts fat] (make-proto-fat [SectorSize (* 2 SectorSize) (* 3 SectorSize)])]
    (is (= [0 1 3] starts))
    (is (= [ENDOFCHAIN 2 ENDOFCHAIN 4 5 ENDOFCHAIN] fat))))

(deftest test-calc-num-sector
  (testing "one byte"
    (is (= 1 (calc-num-sector (- SectorSize 1))))
    (is (= 1 (calc-num-sector SectorSize)))
    (is (= 2 (calc-num-sector (+ SectorSize 1)))))
  (testing "128"
    (is (= 1 (calc-num-sector 3 128)))
    (is (= 1 (calc-num-sector 4 128)))
    (is (= 2 (calc-num-sector 5 128)))))

(deftest test-make-fat
  (testing "Simple"
    (let [[fat start length] (make-fat (range 127))
          tail (last fat)]
      (is (= 128 (count fat)))
      (is (= FATSEC tail))
      (is (= 127 start))
      (is (= 1 length))))
  (testing "Expand"
    (let [[fat start length] (make-fat (range 128))
          padsec (subvec (vec fat) 128 254)
          fatsec (subvec (vec fat) 254)]
      (is (= 256 (count fat)))
      (is (= (vec (long-array 126 FREESEC)) padsec))
      (is (= (vec (long-array 2 FATSEC)) fatsec))
      (is (= 254 start))
      (is (= 2 length))))
  (testing "300"
    (let [[fat start length] (make-fat (range 300))
          padsec (subvec (vec fat) 300 381)
          fatsec (subvec (vec fat) 381)]
      (is (= 384 (count fat)))
      (is (= (vec (long-array 81 FREESEC)) padsec))
      (is (= (vec (long-array 3 FATSEC)) fatsec))
      (is (= 381 start))
      (is (= 3 length)))))

(deftest test-calc-padding
  (testing "SectorSize"
    (is (= 12 (calc-padding 500)))
    (is (= 0 (calc-padding 1024)))
    (is (= 24 (calc-padding 1000))))
  (testing "explicit"
    (is (= 1 (calc-padding 3 4)))
    (is (= 0 (calc-padding 8 4)))
    (is (= 2 (calc-padding 6 4)))))
