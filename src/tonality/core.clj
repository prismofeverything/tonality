(ns tonality.core
  (:use overtone.core)
  (:require [tonality.keyboard :as keyboard]
            [tonality.organ :as organ]))

(defn midi-keyboard-handler
  [organ-born]
  (let [organ (atom organ-born)
        keyboard (atom (keyboard/blank-keyboard))]
    (fn [event]
      (if (:command event)
        (let [[new-organ new-keyboard] (keyboard/trigger @keyboard event @organ)]
          (dosync
           (reset! organ new-organ)
           (reset! keyboard new-keyboard)))))))

(def handle-midi-keyboard
  (midi-keyboard-handler organ/formantax-organ))

(defn boot-keyboard
  [search]
  (let [keyboard (midi-in search)]
    (midi-handle-events keyboard #'handle-midi-keyboard)))

(defn boot-radium
  []
  (boot-keyboard "Port 1"))

(defn boot-emu
  []
  (boot-keyboard "E-MU"))
