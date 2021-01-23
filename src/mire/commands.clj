(ns mire.commands
  (:use [mire.rooms :only [rooms room-contains? room-contains-gold?]]
        [mire.player])
  (:use [clojure.string :only [join]]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- move-delete
  [obj from]
  (alter from disj obj))




;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @*current-room*)
       "\r\nExits: " (keys @(:exits @*current-room*)) "\r\n"
       (join "\r\n" (map #(str "There " % "is here.\r\n")
                           @(:items @*current-room*)))
       (join "\r\n" (map #(str "Player " % " is here.\r\n")
                           @(:inhabitants @*current-room*)))
       (join (str "GOLD " @(:gold @*current-room*) " here.\r\n"))
  ))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @*current-room*) (keyword direction))      
         target (@rooms target-name)]                                    
     (if (not= @( :lock target) #{(some @( :lock target) @*inventory*)}) 
        (if (not= @( :lock target) #{})                                  
           ( str "LOCK!!! Find an " @( :lock target) " to pass " )       
        (if target                                                          
           (do
             (move-between-refs *name*
                                (:inhabitants @*current-room*)
                                (:inhabitants target))
             (ref-set *current-room* target)
             (look))
        "You can't go that way."))
    (if target                                                            
       (do
         (move-between-refs *name*
                            (:inhabitants @*current-room*)
                            (:inhabitants target))
         (ref-set *current-room* target)
         (look))
    "You can't go that way.")))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
    (if (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
      (if (room-contains-gold? @*current-room* thing)
        (do
          (case thing
            "coin" (alter *money* inc)
            "bagmoney" (alter *money* + 7)
            "treasuregold" (alter *money* + 15)
          )
          (if (= ((keyword thing) @(:gold @*current-room*)) 1)
            (alter (:gold @*current-room*) dissoc (keyword thing))
            (do
              (def temp-gold ((keyword thing) @(:gold @*current-room*)))
              (alter (:gold @*current-room*) dissoc (keyword thing))
              (alter (:gold @*current-room*) assoc (keyword thing) (- temp-gold 1))
            )
          )
          (str "You picked up the " thing ".")
        )
        (str "There isn't any " thing " here.")
      )
      (if (room-contains? @*current-room* thing)
        (do
          (move-between-refs (keyword thing)
                             (:items @*current-room*)
                             *inventory*)
          (str "You picked up the " thing ".")
        )
        (str "There isn't any " thing " here.")
      )
    )
  )
)

(defn seemoney
  "See your money"
  []
  (str (join "\r\n" (map #(str "Money is " % " .\r\n") [(str @*money*)])))
)

(defn discard
  "Put something down that you're carrying."
  [thing]
  (if (= #{(keyword thing)} @( :lock @*current-room*))                              
   (str "Here you cannot throw " @( :lock @*current-room*))                         
  (dosync
   (if (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
        (case thing
          "coin" (if (> @*money* 0)
                    (do
                      (alter *money* dec)
                      (if (room-contains-gold? @*current-room* thing)
                        (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                        (def temp-gold 0)
                      )
                      (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                      (str "You dropped the " (keyword thing) ".")
                    )
                    (str "Not enough money!")
                  )
          "bagmoney" (if (> @*money* 7)
                        (do
                          (alter *money* - 5)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "You dropped the " (keyword thing) ".")
                        )
                        (str "Not enough money!")
                      )
          "treasuregold" (if (> @*money* 15)
                        (do
                          (alter *money* - 15)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "You dropped the " (keyword thing) ".")
                        )
                        (str "Not enough money!")
                      )
        )
        (if (carrying? thing)
          (do (move-between-refs (keyword thing)
                                 *inventory*
                                 (:items @*current-room*))
              (str "You dropped the " thing ".")
          )
          (str "You're not carrying a " thing ".")
        )
      )
    )
  )
)

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\r\n"
       (join "\r\n" (seq @*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @*current-room*) *name*)]
      (binding [*out* (streams inhabitant)]
        (println *name* " : " message)
        (println prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (join "\r\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn attack
  "Show available commands and what they do."
  []
  (str "your hp: " *healthpoints* "\r\n"
  					  (join "\r\n" (map #(str "Player " %1 " is here. And has " %2 " hp-s \r\n")
                           @(:inhabitants @*current-room*)
                           @(:inhabitants @*current-room*)))

  					 )

  )

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "seemoney" seemoney
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help
               "attack" attack})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))