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

(deftest test-directory
  (let [strm1 (byte-array 10 (byte 10))
        strm2 (byte-array 20 (byte 20))
        strm3 (byte-array 30 (byte 30))
        _ (make-cfb "test.cfb" [["a/b" strm1]
                                ["a/a" strm2]
                                ["a/c" strm3]])
        cfb (open-cfb "test.cfb")]

    (testing "a/b"
      (let [stream (open-stream cfb "a/b")
            result (read-stream stream)]
        (is (java.util.Arrays/equals result strm1))))

    (testing "a/a"
      (let [stream (open-stream cfb "a/a")
            result (read-stream stream)]
        (is (java.util.Arrays/equals result strm2))))

    (testing "a/c"
      (let [stream (open-stream cfb "a/c")
            result (read-stream stream)]
        (is (java.util.Arrays/equals result strm3))))))

(deftest test-difat-tail
  (let [data (byte-array (* (+ 109 127 42) 128 SectorSize) (byte \D))
        _ (make-cfb "test.cfb" [["stream" data]])
        cfb (open-cfb "test.cfb")
        stream (open-stream cfb "stream")
        result (read-stream stream)]
    (is (java.util.Arrays/equals result data))))
