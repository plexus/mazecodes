# mazecodes

Convert Maze of Galious passwords to EDN and vice versa.

```clojure
(require 'mazecodes.core :refer [edn->code code->edn])

(println 
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
                      :harp :bronze-shield}}))
;; 0GUU 4RRU UR3F 4R37
;; UR3F UR3F UR3F UR3F
;; U1T2 KH4N 7RL2 4

(code->edn "0GUU 4RRU UR3F 4R37
UR2F UR3F UR3F UR3F
U1T2 KH4N 7RL2 3") ;;=> {:arrows 459, ...}
```

## UI

```
➜ bb
Babashka v1.12.218
Type :repl/help for help
user=> (require '[sci.nrepl.browser-server :as nrepl])
  #_=> (nrepl/start! {:nrepl-port 1339 :websocket-port 1340}) 
```

```
cd public
python3 -m http.server
```

## Links

http://fcfantasy.cn/maps/msx/the_maze_of_galious_knightmare_2.png

## License

Copyright &copy; 2019-2026 Arne Brasseur

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
