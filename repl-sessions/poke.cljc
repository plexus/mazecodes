(ns poke
  (:require [mazecodes.core :refer :all]))



(comment
  (println
   (str
    "--------------------------------\n"
    (edn->code {:arrows 459
                :coins 32
                :keys 23
                :active :popolon
                :aphrodite {:exp 0
                            :vit 8
                            :alive? true
                            :revived? false}
                :popolon {:exp 0
                          :vit 16 ;; you start with 8, each key grants +8
                          :alive? true
                          :revived? false}
                :items #{:arrows :ceramic-arrows :mines :magnifying-glass
                         :necklace :helmet :shoes :halo
                         :pendant :earrings :bible
                         :harp :bronze-shield}
                })))



  (code->segments "UR3F UR3F UR4F 423R
UR3F UR3F UR3F UR3F
UR3F UH3F URS3 E")

  (decode-segment "423R")


  (edn->code
   (code->edn
    "0GUU 4RRU UR3F 4R37
UR2F UR3F UR3F UR3F
U1T2 KH4N 7RL2 3")))

(println
 (edn->code {:arrows 123
             :coins 456
             :keys 789
             :active :popolon
             :aphrodite {:exp 0
                         :vit 8
                         :alive? true
                         :revived? false}
             :popolon {:exp 0
                       :vit 16 ;; you start with 8, each key grants +8
                       :alive? true
                       :revived? false}
             :items #{
                      :arrows :ceramic-arrows :rolling-fire :fire :mines :magnifying-glass
                      ;; These are placeholder for the world-items, setting these bits in the code
                      ;; will make them show up in your inventory even when in the castle.
                      :_1 :_2 :_3 :_4
                      :cross :great-key
                      :necklace :crown :helmet :oar :shoes :doll :robe :bell :halo :candle
                      :armor :carpet :helm :lamp :vase :pendant :earrings :bracelet :ring :bible
                      :harp :triangle :trumpet-shell :pitcher :saber :dagger :feather
                      :bronze-shield :bread-and-water :salt :silver-shield :golden-shield
                      }}))

(edn->code
 (code->edn
  "K032 TR32 UR4F 4237
UR5F UR3F UR3F UR3F
UH3X UH3F URSF 4"))
