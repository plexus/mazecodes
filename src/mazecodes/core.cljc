(ns mazecodes.core
  "Encode/decode Maze of Galious 'passwords' (save codes)"
  (:require [clojure.string :as str]))

(def cipher* "F3RU72G4X5BKN90PQTHVE6OMWAJ18DSILYCZ    !")

(def enc-cipher (into {} (map-indexed (fn [idx ch] [(- idx 3) ch]) cipher*)))
(def dec-cipher (into {} (map (juxt val key)) enc-cipher))

(def checksum-cipher
  {\0 1 \1 2 \2 3 \3 4 \4 5 \5 6 \6 7 \7 8 \8 9 \9 10
   \A 11 \B 12 \C 13 \D 14 \E 15 \F 16 \G 17 \H 18
   \I 19 \J 20 \K 21 \L 22 \M 23 \N 24 \O 25 \P 26
   \Q 27 \R 28 \S 29 \T 30 \U 31 \V 32 \W 33 \X 34
   \Y 35 \Z 36})

(def item-names
  [:arrows :ceramic-arrows :rolling-fire :fire :mines :magnifying-glass
   ;; These are placeholder for the world-items, setting these bits in the code
   ;; will make them show up in your inventory even when in the castle.
   :_1 :_2 :_3 :_4
   :cross :great-key
   :necklace :crown :helmet :oar :shoes :doll :robe :bell :halo :candle
   :armor :carpet :helm :lamp :vase :pendant :earrings :bracelet :ring :bible
   :harp :triangle :trumpet-shell :pitcher :saber :dagger :feather
   :bronze-shield :bread-and-water :salt :silver-shield :golden-shield])

(def world-item-names
  [:rod :cape :water :map])

(defn code->segments [code]
  (let [code (str/replace code #"\s" "")]
    (conj
     ;; 4-character segments
     (mapv #(subs code (* 4 %) (* 4 (inc %))) (range 10))
     ;; one 3-char segment
     (subs code 40 43)
     ;; 2 char checksum
     (subs code 43))))

(defn partition-str-all [n ^String str]
  (let [len (.length str)]
    (loop [idx 0
           res []]
      (if (< (* (inc idx) n) len)
        (recur (inc idx)
               (conj res (subs str (* idx n) (* (inc idx) n))))
        (conj res (subs str (* idx n)))))))

(defn decode-segment [seg]
  (map-indexed (fn [idx ch] (+ (dec-cipher ch) idx)) seg))

(defn encode-segment [seg]
  (apply str (map-indexed (fn [idx i] (enc-cipher (- i idx))) seg)))

(defn to-hex [i]
  #?(:clj (Long/toHexString i)
     :cljs (.toString i 16)))

(defn checksum [chars]
  (let [hex (to-hex (apply + (map checksum-cipher chars)))]
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

(defn num->bits [num digits]
  (second
   (reduce (fn [[n bs] _]
             (let [b (mod n 2)]
               [(bit-shift-right (- n b) 1) (cons b bs)]))
           [num []] (range digits))))

(defn number->bcd [n]
  (let [u (mod n 10)
        t (mod (/ (- n u) 10) 10)
        h (mod (/ (- n t u) 100) 10)]
    (mapv #(num->bits % 4) [(long u) (long t) (long h)])))

(defn bcd->number [u t h]
  (+ (bits->num u) (* (bits->num t) 10) (* (bits->num h) 100)))

(def world->state
  {:locked [0 0 0]
   :closed [0 1 0]
   :open   [0 0 1]
   :done   [1 0 1]})

(def state->world (into {} (map (juxt val key)) world->state))

(defn world-bits [{:keys [items state]}]
  (concat
   (map (fn [i]
          (if (contains? items i) 1 0))
        world-item-names)
   (world->state (or state :locked))))

(defn edn->nums [edn]
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

                   (concat (drop 3 w10) [0])]
                  (partition 5 items)
                  [(concat (drop (- (count items) 4) items) [0])]

                  [[(if (= active :popolon) 1 0)
                    (if (:revived? popolon) 0 1)
                    (if (:revived? aphrodite) 0 1)
                    (if (:alive? popolon) 1 0)
                    (if (:alive? aphrodite) 1 0)]])))))

(defn edn->code [edn]
  (encode (edn->nums edn)))

(def code-spec
  `[[:arrows1 :int 4]
    [:arrows0 :int 4]
    [:_ :pad 4]
    [:arrows2 :int 4]
    [:coins1 :int 4]
    [:coins0 :int 4]
    [:_ :pad 4]
    [:coins2 :int 4]
    [:keys1 :int 4]
    [:keys0 :int 4]
    [:_ :pad 4]
    [:keys2 :int 4]
    [:_ :pad 2]
    [:exp-aphrodite :int 8]
    [:vit-aphrodite :int 8]
    [:exp-popolon :int 8]
    [:vit-popolon :int 8]
    [:_ :pad 3]
    ~@(for [i (range 1 11)]
        [[:world-state i] :bitmap 8])
    [:items :bitmap 45]
    [:active :bitmap 1]
    [:popolon-revived :bitmap 1]
    [:aphrodite-revived :bitmap 1]
    [:popolon-alive :bitmap 1]
    [:aphrodite-alive :bitmap 1]])

(defn item-set [bits item-names]
  (into #{} (filter some?) (map (fn [bit n] (when (= 1 bit) n)) bits item-names)))

#_(defn code->edn [code]
    (let [nums (decode code)
          [[at0 at1 at2 at3 au0]
           [au1 au2 au3 _ _]
           [_ _ ah0 ah1 ah2]
           [ah3 ct0 ct1 ct2 ct3]

           [cu0 cu1 cu2 cu3 _]
           [_ _ _ ch0 ch1]
           [ch2 ch3 kt0 kt1 kt2]
           [kt3 ku0 ku1 ku2 ku3]

           [_ _ _ _ kh0]
           [kh1 kh2 kh3 _ _]
           & bitgroups] (map num->bits nums)
          bits (into [] cat bitgroups)]
      {:keys (bcd->number [ku0 ku1 ku2 ku3]
                          [kt0 kt1 kt2 kt3]
                          [kh0 kh1 kh2 kh3])
       :arrows (bcd->number [au0 au1 au2 au3]
                            [at0 at1 at2 at3]
                            [ah0 ah1 ah2 ah3])
       :coins (bcd->number [cu0 cu1 cu2 cu3]
                           [ct0 ct1 ct2 ct3]
                           [ch0 ch1 ch2 ch3])
       :aphrodite {:exp (bits->num (take 8 bits))
                   :vit (bits->num (subvec bits 8 16))}
       :popolon {:exp (bits->num (subvec bits 16 24))
                 :vit (bits->num (subvec bits 24 32))}}))

(defn code->edn [code]
  (let [bits (mapcat #(num->bits % 5) (decode code))]
    (let [m (second
             (reduce
              (fn [[bits res :as acc] [name type len]]
                (case type
                  :pad    [(drop len bits) res]
                  :bitmap [(drop len bits) (assoc res name (take len bits))]
                  :int    [(drop len bits) (assoc res name (bits->num (take len bits)))]))
              [bits {}]
              code-spec))]
      (into
       {:arrows    (+ (* 100 (:arrows2 m)) (* 10 (:arrows1 m)) (:arrows0 m))
        :coins     (+ (* 100 (:coins2 m)) (* 10 (:coins1 m)) (:coins0 m))
        :keys      (+ (* 100 (:keys2 m)) (* 10 (:keys1 m)) (:keys0 m))
        :active    (if (= [1] (:active m)) :popolon :aphrodite)
        :aphrodite {:exp      (:exp-aphrodite m)
                    :vit      (:vit-aphrodite m)
                    :alive?   (= [1] (:aphrodite-alive m))
                    :revived? (= [0] (:aphrodite-revived m))}
        :popolon   {:exp      (:exp-popolon m)
                    :vit      (:vit-popolon m)
                    :alive?   (= [1] (:popolon-alive m))
                    :revived? (= [0] (:popolon-revived m))}
        :items     (item-set (:items m) item-names)}
       (keep (fn [wnum]
               (let [wbits (get m [:world-state wnum])]
                 (when (some #{1} wbits)
                   [(keyword (str "world-" wnum))
                    {:state (state->world (drop 5 wbits))
                     :items (item-set wbits world-item-names)}]))))
       (range 1 11)))))
