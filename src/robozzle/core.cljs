(ns robozzle.core
  (:require-macros [devcards.core :as dc :refer [defcard defcard-doc edn]]
                   [cljs.test :refer [is testing]])
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(defcard-doc
  "# Robozzle

  Robozzle is an addictive robot-programming puzzle game. The rules are easy to understand and allow for a wide variety of interesting puzzles.

  I suggest you [play Robozzle](http://robozzle.com) before going any futher. It's fun and you'll get a better idea of where this is all leading to. Otherwise, you can just check out this video:

  <iframe width=\"480\" height=\"320\" src=\"https://www.youtube-nocookie.com/embed/MmqBVWi_Pc0?rel=0\" frameborder=\"0\" allowfullscreen></iframe>


  ## Goals

  It was originally built by [Igor Ostrovsky](http://igoro.com). This version doesn't aim to be official, or feature complete, it's meant primarily as a playground so I can learn ClojureScript and Om Next.

  A secondary goal is to use this project as a live testbed for ClojureScript hosted on the JVM. A more detailed design for that system will be described elsewhere, but it's worth noting here to explain that this game isn't mean't to fully functional.


  ## Approach

  I'm building this game using a bottom up approach. I want to start with data, built up to small UI components and then compose everything together to make the full game. Each card descriped either a UI component or a system. Cards are ordered by their dependencies with components with fewer dependencies coming first.

  One interesting thing is that I'm opting not to use external CSS - everything in the game is styled by inline styles. I'm doing this to encourage sytlistically pure components and also to avoid the burdon of any CSS build-system.")


(defn- grid
  "This is a defcard helper."
  ([items] (grid {:space 0} items))
  ([{:keys [space]} items]
   (dom/div
    #js {:style #js {:display "flex"
                     :flexWrap "wrap"
                     :margin (- 0 space)}}
    (map-indexed
     (fn [i item]
       (dom/div #js {:style #js {:padding space}
                     :key i}
                item))
     items))))

(defcard Grid
  "TODO")


(defn- cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))






; Emojis

(defn emoji [{:keys [char size scale]}]
  (let [scale (or scale 0.75)
        padding (/ (* (- 1 scale) size) 2)
        scaled-size (* scale size)]
    (dom/div #js {:style #js {:padding padding
                              :width scaled-size}}
             (dom/img #js {:src (str "http://twemoji.maxcdn.com/svg/" char ".svg")
                           :style #js {:display "block"}
                           :width scaled-size
                           :height scaled-size}))))

(defcard Emoji
  "I'm using emoji as the basic icons in the game. They're simple and cute. This component makes it easier to pull in those emoji and format them in a square tile. The `:char` property is the character code for any emoji that [Twemoji](https://twitter.github.io/twemoji/preview.html) supports.

  ```
  (emoji {:char \"1f355\" :size 60}) ; pizza
  (emoji {:char \"2b50\" :size 40}) ; star
  (emoji {:char \"1f4a9\" :size 20}) ; poo
  ```
  "
  (grid [(dom/div #js {:style #js {:textAlign "center"
                                   :padding "1rem"}}
                  "Pizza"
                  (emoji {:char "1f355" :size 60}))
         (dom/div #js {:style #js {:textAlign "center"
                                   :padding "1rem"}}
                  "Star"
                  (emoji {:char "2b50" :size 40}))
         (dom/div #js {:style #js {:textAlign "center"
                                   :padding "1rem"}}
                  "Poo"
                  (emoji {:char "1f4a9" :size 20}))]))


; Tiles

(defn tile-color [tile]
  (clojure.string/lower-case tile))

(defn star? [tile]
  (and tile (= (clojure.string/upper-case tile) tile)))

(defn tile-hex-color [tile]
  (get {"b" "#0074D9"
        "g" "#2ECC40"
        "r" "#FF4136"}
      (tile-color tile)))

(defui Tile
  Object
  (render [this]
    (let [{:keys [size tile has-star]} (om/props this)
          style #js {:width size
                     :height size
                     :background "linear-gradient(135deg, rgba(255, 255, 255, .4), rgba(0, 0, 0, .1))"
                     :backgroundColor (tile-hex-color tile)
                     :boxShadow "inset -1px -1px 0 0 rgba(0, 0, 0, .5), inset 1px 1px 0 0 rgba(255, 255, 255, .5)"}]
      (dom/div
        #js {:style style}
        (when (= tile (clojure.string/upper-case tile))
          (emoji {:char "1f538" :size size}))))))

(def tile (om/factory Tile))

(defcard Tile
  "A tile is either red, green or blue and can optionally contain a star. This leads to six possible tiles. A tile is encoded as a single character. The character represents the color. If the character is uppercase, the tile has a star.

  ```
  (tile {:tile \"r\" :size 40})
  (tile {:tile \"R\" :size 40})
  (tile {:tile \"g\" :size 40})
  (tile {:tile \"G\" :size 40})
  (tile {:tile \"b\" :size 40})
  (tile {:tile \"B\" :size 40})
  ```
  "
  (grid
   (map
    (fn [t] (tile {:tile t :size 40 :key t}))
    ["r" "R" "g" "G" "b" "B"])))


; Board

(defui Board
  Object
  (render [this]
    (let [{:keys [width height tiles dir pos]} (om/props this)
          tile-size 40
          style #js {:width (* width tile-size)
                     :height (* height tile-size)
                     :backgroundColor "#1A2033"
                     :position "relative"}]

      (dom/div #js {:style style}
               (flatten
                (map (fn [y]
                       (map (fn [x]
                              (dom/div #js {:key (str x "," y)
                                            :style #js {:position "absolute"
                                                        :top (* y tile-size)
                                                        :left (* x tile-size)}}
                                       (when-let [t (get-in tiles [y x])]
                                         (tile {:tile t
                                                :size tile-size}))))
                            (range width)))
                     (range height)))
               (dom/div #js {:style #js {:position "absolute"
                                         :top (* (first pos) tile-size)
                                         :left (* (second pos) tile-size)
                                         :transform (str "rotate(" (* (dec dir)  90) "deg)")}}
                        (emoji {:char "1f355" :size tile-size}))))))

(def board (om/factory Board))

(defcard Board
  "A board is the initial configuration of the game. It has the property that it is both the intial state, and the way of observing the game.

  The `:width` and `:height` are passed in because the board can be larger than the tiles provided.

  `:dir` and `:pos` both describe the robot. `dir` is in the domain {0 1 2 3} where 0 is the robot facing east. The direction increments clockwise. `pos` is a `[y x]` (note the switch) Integer tuple describing the position of the robot.

  `:tiles` is a 2D vector of tile characters.

  ```
  (board {:width 3 :height 4 :dir 0 :pos [0 0] :tiles [[\"b\"]]})
  ```"
  (fn [data-atom owner]
    (let [{:keys [dir pos tiles]} @data-atom]
      (board {:width 5
              :height 5
              :dir dir
              :pos pos
              :tiles tiles})))
  {:dir 0
   :pos [2 1]
   :tiles [[nil nil nil nil nil]
           [nil \r \b \G nil]
           [nil \b nil \B nil]
           [nil \g \b \R nil]
           [nil nil nil nil nil]]}
  {:inspect-data true})


; Editor

(defn- fn-cell [n]
)

(def cmd-emoji {:move "2b06"
                :left "2b05"
                :right "27a1"
                :red "2764"
                :green "1f49a"
                :blue "1f499"
                :f1 "31-20e3"
                :f2 "32-20e3"
                :f3 "33-20e3"
                :f4 "34-20e3"
                :f5 "35-20e3"})

(def pred-color {"b" "#CDE4F8"
                 "g" "#D6F5DA"
                 "r" "#FFDAD8"})

(def foo {:red "r"
          :green "g"
          :blue "b"})

(defn operation
  [{[cmd pred] :operation}]
  (dom/div
   #js {:style #js {:width 40
                    :height 40
                    :backgroundColor (pred-color pred)}}
   (cond
     (contains? #{:red :green :blue} cmd)
     (dom/div #js {:style #js {:padding 10}}
              (dom/div #js {:style #js {:width 20
                                        :height 20
                                        :backgroundColor (tile-hex-color (foo cmd))
                                        :borderRadius 2}})
              )

     cmd (emoji {:char (cmd-emoji cmd) :size 40 :scale 0.5}))))



(defcard Operation
  "Yay an operation."
  (grid
   {:space 1}
   (map
    (fn [o]
      (operation {:operation o}))
    (cartesian-product [nil
                        :move
                        :left
                        :right
                        :red
                        :green
                        :blue
                        :f1
                        :f2
                        :f3
                        :f4
                        :f5]
                       [nil "r" "g" "b"]))))


(defui Program
  Object
  (render
   [this]
   (let [{:keys [program layout ip]} (om/props this)]
     (dom/div
      #js {:style #js {:marginTop "-1rem"}}
      (map-indexed
       (fn [i n]
         (dom/div
          #js {:style #js {:paddingTop "1rem"
                           :display "flex"}}
          (dom/div
           #js {:style #js {:textAlign "center"
                            :alignSelf "center"
                            :minWidth 40}}
           (str "F" (inc i)))
          (grid
           {:space 2}
           (map
            (fn [j]
              (let [ref [i j]
                    op (or (get-in program ref) [nil nil])

                    bar                 (dom/div #js {:style #js {:backgroundColor "rgba(0, 0, 0, .05)"}}
                         (operation {:operation op}))]
                (if (= ip ref)
                  (dom/div #js {:style #js {:boxShadow "0 0 6px 1px rgba(0, 0, 0, .4), 0 0 0 1px rgba(0, 0, 0, .1)"
                                            :WebkitTransform "scale(1.2)"}}
                           bar)
                  bar
                  )
))
           (range n)))
))
       layout)))))

(def program (om/factory Program))

(defcard ProgramCard
  "Each command is a one or two element vector. The first element is aways the command, and the second (optional element) is the color predicate that the command can be executed upon. These are arranged into functions (a vector). A program is a vector containing at minimum one function and at most 5 functions.

  The possible commands are:

  * `:move`
  * `:left` & `:right`
  * `:red`, `:green` & `:blue`
  * `:f1`, `:f2`, `:f3`, `:f4` & `:f5`

  As an example, here is a program that (would theoretically) move forward and turn right when it moves onto a green tile:"
  (fn [atom _]
    (dom/div #js {:style #js {:paddingTop "1rem"
                              :paddingBottom "1rem"}}
             (program @atom)))

  {:layout [5 4 3 2 1]
   :program [[[:left] [:right "b"] [:move "g"] [:red] [:f2 "r"]]
             [[:right] [:right]]]
   :ip [1 1]}
  {:inspect-data true})





; Virtual Machine

(def directions {0 [ 0  1]
                 1 [ 1  0]
                 2 [ 0 -1]
                 3 [-1  0]})

(def f->idx {:f1 0
             :f2 1
             :f3 3
             :f4 4
             :f5 5})

(defn- move [dir pos]
  (mapv + pos (directions dir)))

(defn- turn-left [dir]
  (mod (dec dir) (count directions)))

(defn- turn-right [dir]
  (mod (inc dir) (count directions)))

(defn new-vm [{:keys [dir pos tiles]} program]
  {:dir dir
   :pos pos
   :tiles tiles
   :program program
   :ip [0 0]
   :stack []
   :steps 0
   :stars (count (filter star? (apply concat tiles)))})

(defn step [{:keys [dir pos program ip stack] :as state}]
  (let [tile-path (apply vector :tiles pos)
        tile (get-in state tile-path)
        star? (star? tile)
        tile (tile-color tile)
        [op-code op-color] (get-in program ip)
        state (-> state
                  ; moves the instruction pointer
                  (update-in [:ip 1] inc)

                  ; increments the number of sets taken
                  (update :steps inc)

                  ; clears any stars on the current tile
                  (assoc-in tile-path tile)

                  ; decrements the number of stars remaining if on a star
                  (update :stars (if star? dec identity)))]

    ; only execute op if there's a pred and it matches
    (if (and op-color (not= tile op-color))
      state
      (case op-code
        ; move robot forward
        :move (update state :pos (partial move dir))

        ; turn robot
        :left (update state :dir turn-left)
        :right (update state :dir turn-right)

        ; set tile colors
        :red (assoc-in state tile-path \r)
        :green (assoc-in state tile-path \g)
        :blue (assoc-in state tile-path \b)

        ; function has finished executing, reset ip from stack
        nil (step (assoc state :ip (first stack) :stack (rest stack)))

        ; function call
        (assoc state
               ; jump ip to new function
               :ip [(f->idx op-code) 0]
               ; store current ip in stack
               :stack (cons (:ip state) stack))))))

(defcard VirtualMachine
  "The virtual machine (VM) is the meat and bones of the game. Firstly, you might ask \"Robozzle is just a game, why does it have a virtual machine?\" Well, part of the beauty of Robozzle is becuase your programs can have functions and can modify a board, for any non-empty board the programs are turing complete. They are capable of executing an arbitarily complex calculus.

  The VM is implemented with a very simple architecture. All it has is memory (the board), an instruction pointer and a function call stack. The VM enforces code and data separation by storing them in separate sections of memory. Only the data (tiles) are writeable, your program code is immutable. Unlike regular CPU architectures, your program can't re-write it's own code.

  A VM is constructed from the initial board and a program. To actually run the VM, there is a `step` function, that takes the VM, and returns a new VM after the instruction at the current instruction pointer has been executed. The `step` function handles executing the operations and updating the tiles on the game board.

  ```
  (def my-board {:dir 0
                 :pos [0 0]
                 :tiles [[\"b\" \"b\" \"B\"]]})
  (def my-program [[[:move] [:f1]]])

  (def vm (new-vm my-board my-program))

  (step vm)
  ```

  Using the compoents we've constructed previously, we can visualize the VM stepping through a simple program. The VM's stack size is unbounded here to the program will execute indefinitely."
  (fn [data-atom _]
    (let [{:keys [dir pos tiles ip]} @data-atom]
      (dom/div nil
               (dom/div
                #js {:style #js {:display "flex"
                                 :alignItems "flex-start"}}
                (board {:width 3
                        :height 3
                        :dir dir
                        :pos pos
                        :tiles tiles})
                (program {:layout [3] :program (:program @data-atom) :ip ip}))
               (dom/button #js {:onClick (fn [] (swap! data-atom step))} "Step"))))

  (new-vm {:dir 0
             :pos [0 0]
             :tiles [["g" "B" "g"]
                     ["B" nil "B"]
                     ["g" "B" "g"]]}
            [[[:move] [:right \g] [:f1]]])
  {:inspect-data true :history true})

(dc/deftest step-test
  "The `step` function itself is a very simple, pure function. However, its behavior is complex and therefore should be tested. The pleasant side-effect of these tests is that they strongly document the exact behavior of the game."
  (testing "The steps counter is incremented each step."
    (let [board {:dir 0 :pos [0 0] :tiles [["b"]]}
          program [[[:right]]]
          vm (new-vm board program)]
      (is (= (:steps vm) 0))
      (is (= (:steps (step vm)) (inc (:steps vm))))))

  (testing "The star counter is decremented when robot is on a star."
    (let [vm-without-star (new-vm {:dir 0 :pos [0 0] :tiles [["b"]]} [[]])
          vm-with-star (new-vm {:dir 0 :pos [0 0] :tiles [["B"]]} [[]])]
      (is (= 0 (:stars vm-without-star)))
      (is (= 1 (:stars vm-with-star)))
      (is (= (dec (:stars vm-with-star)) (:stars (step vm-with-star))))))

  (testing "A robot can move (in all for directions)."
    (let [move-right-vm (new-vm {:dir 0
                                 :pos [0 0]
                                 :tiles [["b" "b"]]}
                                [[[:move]]])
          move-down-vm (new-vm {:dir 1
                                :pos [0 0]
                                :tiles [["b"]
                                        ["b"]]}
                               [[[:move]]])
          move-left-vm (new-vm {:dir 2
                                :pos [0 1]
                                :tiles [["b" "b"]]}
                               [[[:move]]])
          move-up-vm (new-vm {:dir 3
                              :pos [1 0]
                              :tiles [["b"]
                                      ["b"]]}
                             [[[:move]]])]
      (is (= [0 1] (:pos (step move-right-vm))))
      (is (= [1 0] (:pos (step move-down-vm))))
      (is (= [0 0] (:pos (step move-left-vm))))
      (is (= [0 0] (:pos (step move-up-vm))))))

  (testing "The robot's direction can turned cw and ccw."
    (let [turn-right-vm (new-vm {:dir 0
                                 :pos [0 0]
                                 :tiles [["b"]]}
                                [[[:right]]])
          turn-left-vm (new-vm {:dir 0
                                :pos [0 0]
                                :tiles [["b"]]}
                               [[[:left]]])]
      (is (= 1 (:dir (step turn-right-vm))))
      (is (= 3 (:dir (step turn-left-vm))))))

  (testing "Tiles can be changed."
    (let [red-vm (new-vm {:dir 0
                          :pos [0 0]
                          :tiles [["b"]]}
                         [[[:red]]])
          green-vm (new-vm {:dir 0
                            :pos [0 0]
                            :tiles [["b"]]}
                           [[[:green]]])
          blue-vm (new-vm {:dir 0
                           :pos [0 0]
                           :tiles [["g"]]}
                          [[[:blue]]])]
      (is (= [["r"]] (:tiles (step red-vm))))
      (is (= [["g"]] (:tiles (step green-vm))))
      (is (= [["b"]] (:tiles (step blue-vm))))))

  (testing "Programs can call functions."
    (let [turning-vm (new-vm {:dir 0
                              :pos [0 0]
                              :tiles [["b"]]}
                             [[[:f2] [:right]]
                              [[:right]]])
          recursive-vm (new-vm {:dir 0
                                :pos [0 0]
                                :tiles [["b"]]}
                               [[[:right] [:f1]]])]
      (is (= 2 (:dir (-> turning-vm step step step))))
      (is (= 2 (:dir (-> recursive-vm step step step))))))

  (testing "Operations that have predicates only execute when the robot is on a tile of the same color."
    (let [board {:dir 0 :pos [0 0] :tiles [["b"]]}
          right-on-blue-vm (new-vm board [[[:right "b"]]])
          right-on-red-vm (new-vm board [[[:right "r"]]])]
      (is (= 0 (:dir (step right-on-red-vm))))
      (is (= 1 (:dir (step right-on-blue-vm)))))))


(comment
  (defn mutate
    [{:keys [state] :as env} key params]
    (if (= 'step key)
      {:value {:keys [:dir :pos :board :ip :stack :steps :stars]}
       :action #(swap! state step)}
      {:value :not-found})))
