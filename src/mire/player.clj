(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *healthpoints* 5)
(def ^:dynamic *money*)

(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))
