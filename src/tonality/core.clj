(ns tonality.core
  (:use overtone.core)
  (:require [tonality.keyboard :as keyboard]
            [tonality.sound :as sound]
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
  (midi-keyboard-handler organ/pad-organ))

(defn boot-radium
  []
  (let [radium49 (midi-in "Port 1")]
    (midi-handle-events radium49 #'handle-midi-keyboard)))
