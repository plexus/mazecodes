(ns mazecodes.core
  (:require [clojure.string :as str]))

(def cipher* "F3RU72G4X5BKN90PQTHVE6OMWAJ18DSILYCZ    !")

(def enc-cipher (into {} (map-indexed (fn [idx ch] [(- idx 3) ch]) cipher*)))
(def dec-cipher (into {} (map-indexed (fn [idx ch] [ch (- idx 3)]) cipher*)))

(def checksum-cipher
  {\0 1 \1 2 \2 3 \3 4 \4 5 \5 6 \6 7 \7 8 \8 9 \9 10 \A 11 \B 12 \C 13 \D 14 \E 15 \F 16 \G 17 \H 18 \I 19 \J 20 \K 21 \L 22 \M 23 \N 24 \O 25 \P 26 \Q 27 \R 28 \S 29 \T 30 \U 31 \V 32 \W 33 \X 34 \Y 35 \Z 36})

(def item-names
  [:arrows
   :ceramic-arrows
   :rolling-fire
   :fire
   :mines
   :magnifying-glass
   :cross
   :great-key
   :necklace
   :crown
   :helmet
   :oar
   :shoes
   :doll
   :robe
   :bell
   :halo
   :candle
   :armor
   :carpet
   :helm
   :lamp
   :vase
   :pendant
   :earrings
   :bracelet
   :ring
   :bible
   :harp
   :triangle
   :trumpet-shell
   :pitcher
   :saber
   :dagger
   :feather
   :bronze-shield
   :bread-and-water
   :salt
   :silver-shield
   :golden-shield])

(defn code->segments [code]
  (reduce
   (fn [[res code] i]
     (cond
       (< i 10)
       [(conj res (subs code 0 4))
        (subs code 5)]

       (= i 10)
       [(conj res (subs code 0 3))
        (subs code 3)]

       (= i 11)
       (conj res (str (first code) (last code)))))
   [[] code]
   (range 12)))

(defn segments->code [segments]
  (reduce
   (fn [s [seg idx]]
     (if (= idx 11)
       (str s (first seg) " " (last seg))
       (let [s (str s seg)]
         (cond
           (> idx 10)
           s

           (= 3 (mod idx 4))
           (str s "\n")

           (< idx 10)
           (str s " ")

           :else
           s))))
   ""
   (map vector segments (range))))

(defn decode-segment [seg]
  (map-indexed (fn [idx ch] (+ (dec-cipher ch) idx)) seg))

(defn encode-segment [seg]
  (apply str (map-indexed (fn [idx i] (enc-cipher (- i idx))) seg)))

(defn checksum [chars]
  (let [hex (Long/toHexString (apply + (map checksum-cipher chars)))]
    (str/upper-case (subs hex (- (count hex) 2)))))

(defn decode [code]
  (let [segments (take 11 (code->segments code))]
    (mapcat decode-segment segments)))

(defn encode [nums]
  (let [segs (map encode-segment (partition-all 4 nums))
        [c1 c2] (checksum (mapcat identity segs))]
    (str (str/join "\n"
                   (map (fn [segs]
                          (str/join " " segs))
                        (partition-all 4 segs)))
         c1 " " c2)))

(defn bits->num [bits]
  (reduce (fn [n b] (+ (bit-shift-left n 1) b)) 0 bits))

(defn num->bits
  ([num]
   (num->bits 5))
  ([num digits]
   (second
    (reduce (fn [[n bs] _]
              (let [b (mod n 2)]
                [(bit-shift-right (- n b) 1) (cons b bs)]))
            [num []] (range digits)))))

(defn digit->bcd [d]
  (mapv #(if (= 0 (bit-and % d)) 0 1) [8 4 2 1]))

(defn number->bcd [n]
  (let [u (mod n 10)
        t (mod (/ (- n u) 10) 10)
        h (mod (/ (- n t u) 100) 10)]
    (mapv digit->bcd [(long u) (long t) (long h)])))

(def world-state
  {:locked [0 0 0]
   :closed [0 1 0]
   :open   [0 0 1]
   :done   [1 0 1]})

(defn world-bits [{:keys [items state]}]
  (concat
   (map (fn [i]
          (if (contains? items i) 1 0))
        [:rod :cape :water :map])
   (world-state state (world-state :locked))))

(defn edn->code [edn]
  (let [{:keys [arrows keys coins items aphrodite popolon active]} edn
        [[au0 au1 au2 au3] [at0 at1 at2 at3] [ah0 ah1 ah2 ah3]] (number->bcd arrows)
        [[ku0 ku1 ku2 ku3] [kt0 kt1 kt2 kt3] [kh0 kh1 kh2 kh3]] (number->bcd keys)
        [[cu0 cu1 cu2 cu3] [ct0 ct1 ct2 ct3] [ch0 ch1 ch2 ch3]] (number->bcd coins)
        ea (num->bits (:exp aphrodite) 8)
        va (num->bits (:vit aphrodite) 8)
        ep (num->bits (:exp popolon) 8)
        vp (num->bits (:vit popolon) 8)
        [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10] (map (fn [i]
                                                (world-bits (get edn (keyword (str "world-" (inc i))))))
                                              (range 10))
        items (map (fn [i] (if (contains? items i) 1 0)) item-names)]
    (encode
     (concat
      (map bits->num
           (concat [[at0 at1 at2 at3 au0]
                    [au1 au2 au3 0 0]
                    [0 0 ah0 ah1 ah2]
                    [ah3 ct0 ct1 ct2 ct3]

                    [cu0 cu1 cu2 cu3 0]
                    [0 0 0 ch0 ch1]
                    [ch2 ch3 kt0 kt1 kt2]
                    [kt3 ku0 ku1 ku2 ku3]

                    [0 0 0 0 kh0]
                    [kh1 kh2 kh3 0 0]
                    (take 5 ea)
                    (concat (drop 5 ea) (take 2 va))

                    (take 5 (drop 2 va))
                    (cons (last va) (take 4 ep))
                    (concat (drop 4 ep) (take 1 vp))
                    (take 5 (drop 1 vp))
                    ;;----------------------------------------------
                    (concat (drop 6 vp) [0 0 0])
                    (concat (take 4 w1) [0])
                    (concat (drop 4 w1) (take 2 w2))
                    (concat (drop 2 (take 2 w2)) [0] (take 2 (drop 4 w2)))

                    (cons (last w2) (take 4 w3))
                    (concat [0] (drop 4 w3) (take 1 w4))
                    (concat (take 3 (drop 1 w4)) [0] (take 1 (drop 4 w4)))
                    (concat (drop 5 w4) (take 3 w5))

                    (concat (take 1 (drop 3 w5)) [0] (drop 4 w5))
                    (concat [0] (take 4 w6))
                    (concat (drop 4 w6) (take 2 w7))
                    (concat (take 2 (drop 2 w7)) [0] (take 2 (drop 4 w7)))

                    (concat (drop 6 w7) (take 4 w8))
                    (concat [0] (drop 4 w8) (take 1 w9))
                    (concat (take 3 (drop 1 w9)) [0] (take 1 (drop 4 w9)))
                    (concat (drop 5 w9) (take 3 w10))
                    ;;----------------------------------------------

                    (concat (drop 3 w10) [0])
                    (take 5 items)
                    [(nth items 5) 0 0 0 0]]

                   (partition 5 (drop 6 items))
                   [(concat (drop (- (count items) 4) items) [0])]

                   [[(if (= :active :popolon) 1 0)
                     (if (:revived? popolon) 0 1)
                     (if (:revived? aphrodite) 0 1)
                     (if (:alive? popolon) 1 0)
                     (if (:alive? aphrodite) 1 0)]]))))))

(println
 "--------------------------------\n"
 (edn->code {:keys 123
             :arrows 987
             :coins 543
             :active :aphrodite
             :aphrodite {:exp 8
                         :vit 32
                         :alive? true
                         :revived? false}
             :popolon {:exp 8
                       :vit 40
                       :alive? true
                       :revived? false}
             :items #{:arrows :vase :fire :mines :magnifying-glass :cross :great-key
                      :necklace :crown :helmet :oar :shoes :doll :robe :bell :halo :candle :armor}
             :world-1 {:items #{:rod :cape}
                       :state :open}
             :world-2 {:items #{:map :holy-water}
                       :state :open}
             :world-3 {:state :closed}}))
