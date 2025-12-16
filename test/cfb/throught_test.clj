(ns cfb.throught-test
  (:require [clojure.test :refer :all]
            [cfb.serializer :refer [make-cfb]]
            [cfb.parser :refer [open-cfb open-stream read-stream]]
            [cfb.constants :refer [SectorSize]]))

(deftest test-read-stream
  (let [data (byte-array (concat
                          (byte-array SectorSize (byte \A))
                          (byte-array SectorSize (byte \B))
                          (byte-array SectorSize (byte \C))))
        _  (make-cfb "test.cfb" [["stream" data]])
        cfb (open-cfb "test.cfb")
        stream (open-stream cfb "stream")]

    (testing "full"
      (let [result (read-stream stream)]
        (is (java.util.Arrays/equals data result))))

    (testing "length"
      (let [result (read-stream stream (+ SectorSize (/ SectorSize 2)))
            expected (byte-array (concat
                                  (byte-array SectorSize (byte \A))
                                  (byte-array (/ SectorSize 2) (byte \B))))]
        (is (java.util.Arrays/equals result expected))))

    (testing "offset"
      (let [result (read-stream stream (+ SectorSize (/ SectorSize 2))
                                (+ SectorSize (/ SectorSize 2)))
            expected (byte-array (concat
                                  (byte-array (/ SectorSize 2) (byte \B))
                                  (byte-array SectorSize (byte \C))))]
        (is (java.util.Arrays/equals result expected))))))
