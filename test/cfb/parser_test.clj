(ns cfb.parser-test
  (:require [clojure.test :refer :all]
            [cfb.parser :refer :all]))

(deftest test-sector->offset
  (is (= SectorSize (sector->offset 0)))
  (is (= (* SectorSize 3) (sector->offset 2))))

(deftest test-locate-start-sector
  (let [fat [1 2 3 ENDOFCHAIN]]
    (let [start 0
          offset 0]
      (is (= 0 (locate-start-sector fat start offset))))))
