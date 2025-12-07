(ns cfb.parser-test
  (:require [clojure.test :refer :all]
            [cfb.parser :refer :all]))

(deftest test-sector->offset
  (is (= SectorSize (sector->offset 0)))
  (is (= (* SectorSize 3) (sector->offset 2))))
