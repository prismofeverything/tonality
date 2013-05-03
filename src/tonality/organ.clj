(ns tonality.organ
  (:use overtone.core)
  (:require [tonality.tonality :as tonality]
            [tonality.keyboard :as keyboard]
            [tonality.sound :as sound]))

(defn control-parameters
  [program keyboard]
  (reduce
   (fn [parameters [channel level]]
     (if-let [[label range] (get program channel)]
       (assoc parameters label (* level range))
       parameters))
   {} (:controls keyboard)))

(defn generate-tone
  [program tonality instrument note velocity keyboard]
  (let [tone (tonality note)
        amp (* velocity keyboard/midi-normal)
        control (control-parameters program keyboard)
        parameters (merge-with * {:freq tone :amp amp} control)]
    (println "CONTROL" control)
    (println "PROGRAM" program)
    (println "PARAMETERS" parameters)
    (apply instrument (mapcat identity parameters))))

(defn stop-tone
  [synth]
  (ctl synth :gate 0))

(defrecord TonalityOrgan [tonality instrument program playing]
  keyboard/KeyboardResponse
  (on [this keyboard note velocity]
    (let [synth (generate-tone program tonality instrument note velocity keyboard)]
      (assoc-in this [:playing note] synth)))
  (off [this keyboard note]
    (stop-tone (get playing note))
    (update-in this [:playing] #(dissoc % note)))
  (control [this keyboard channel level]
    (let [[signal scale] (get program channel)]
      (println "SIGNAL" signal scale level channel)
      (ctl instrument signal (* level scale))
      this))
  (wheel [this keyboard base detail] this))

(def radium-controls
  [1 7
   82 83 28 29 16 80 18 19
   74 71 81 91 2 10 5 21])

(def organ
  (atom
   (TonalityOrgan.
    tonality/nineteen sound/j8
    {7 [:amp keyboard/midi-normal]}
    {})))

(def pad-organ
  (TonalityOrgan.
   tonality/nineteen sound/j8
   {7 [:amp keyboard/midi-normal]
    1 [:amt keyboard/midi-normal]
    82 [:a (* 3.0 keyboard/midi-normal)]
    83 [:d (* 3.0 keyboard/midi-normal)]
    28 [:s (* 3.0 keyboard/midi-normal)]
    29 [:r (* 3.0 keyboard/midi-normal)]
    2 [:t (* 20.0 keyboard/midi-normal)]
    }
   {}))

(def b3-organ
  (TonalityOrgan.
   tonality/nineteen sound/b3
   {7 [:amp keyboard/midi-normal]
    82 [:a (* 3.0 keyboard/midi-normal)]
    83 [:d (* 3.0 keyboard/midi-normal)]
    28 [:s (* 3.0 keyboard/midi-normal)]
    29 [:r (* 3.0 keyboard/midi-normal)]
    }
   {}))

(defn keyboard-organ
  []
  (atom []))

