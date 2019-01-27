(ns mazecodes.core-test
  (:require [mazecodes.core :as m]
            [clojure.test :refer [is]]
            [kaocha.test :refer [deftest]]))

(deftest code->segments-test
  (is (= ["TRR4" "6R7W" "UR4F" "V23X"
          "U8MJ" "Y9SP" "WR5F" "UR3F"
          "UYTP" "CIQI" "JNO" "A6"]

         (m/code->segments "TRR4 6R7W UR4F V23X
U8MJ Y9SP WR5F UR3F
UYTP CIQI JNOA 6"))))

(deftest segments->code-test
  (is (= "TRR4 6R7W UR4F V23X
U8MJ Y9SP WR5F UR3F
UYTP CIQI JNOA 6"
         (m/segments->code
          ["TRR4" "6R7W" "UR4F" "V23X"
           "U8MJ" "Y9SP" "WR5F" "UR3F"
           "UYTP" "CIQI" "JNO" "A6"]))))

(deftest decode-segment-test
  (is (= [0 0 0 0]
         (m/decode-segment "UR3F")))

  (is (= [0 0 0 0]
         (m/decode-segment "UR3F")))

  (is (= [0 0 0 16]
         (m/decode-segment "UR3Q"))))

(deftest encode-segment-test
  (is (= "UR3F"
         (m/encode-segment [0 0 0 0]))))

(deftest checksum-test
  (is (= "48"
         (m/checksum "UR3FUR3FUR4F423RUR3FUR3FUR3FUR3FUR3FUR3FURS"))))

(deftest bits->num-test
  (is (= 0 (m/bits->num [0 0 0 0])))
  (is (= 1 (m/bits->num [0 0 0 1])))
  (is (= 2 (m/bits->num [0 0 1 0])))
  (is (= 3 (m/bits->num [0 0 1 1])))
  (is (= 4 (m/bits->num [0 1 0 0])))
  (is (= 5 (m/bits->num [0 1 0 1]))))

(deftest decode-test
  (is (= [14 0 1 7 18 0 3 24 0 0 6 0 16 3 0 8 0 26 22 26 30 11 29 15 21 0 8 0 0 0 0 0 0 31 16 15 31 29 15 31 23 10 21]
         (m/decode "TRR4 6R7W UR4F V23X
U8MJ Y9SP WR5F UR3F
UYTP CIQI JNOA 6"))))

(deftest encode-test
  (is (= (m/encode [14 0 1 7 18 0 3 24 0 0 6 0 16 3 0 8 0 26 22 26 30 11 29 15 21 0 8 0 0 0 0 0 0 31 16 15 31 29 15 31 23 10 21])
         "TRR4 6R7W UR4F V23X\nU8MJ Y9SP WR5F UR3F\nUYTP CIQI JNOA 6")))

#_
(encode
 (decode "UR3F UR3F UR4F 423R
UR3F UR3F UR3F UR3F
UR3F UR3F URS"))
