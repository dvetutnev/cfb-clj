(ns cfb.parser-test
  (:require [clojure.test :refer :all]
            [cfb.parser :refer :all]))

(deftest test-sector->offset
  (is (= SectorSize (sector->offset 0)))
  (is (= (* SectorSize 3) (sector->offset 2))))

(deftest test-locate-start-sector
  (testing "Flat FAT"
    (let [fat [1 2 3 ENDOFCHAIN]]
      (let [start 0 offset 0]
        (is (= 0 (locate-first-sector fat start offset))))
      (let [start 0 offset SectorSize]
        (is (= 1 (locate-first-sector fat start offset))))
      (let [start 0 offset (+ 1 SectorSize)]
        (is (= 1 (locate-first-sector fat start offset))))))
  (testing "Chop FAT"
    (let [fat [2 ENDOFCHAIN 4 ENDOFCHAIN ENDOFCHAIN]
          start 0 offset (* 2 SectorSize)]
      (is (= 4 (locate-first-sector fat start offset))))))
