(ns tonality.keyboard)

(def midi-normal (/ 1 127))

(defn blank-keyboard
  []
  {:keys {} :controls {} :wheel {:base 64 :detail 0} :state {}})

(defprotocol KeyboardResponse
  (on [this keyboard note velocity])
  (off [this keyboard note])
  (control [this keyboard channel level])
  (wheel [this keyboard base detail]))

(defn note-on-event
  [response keyboard note velocity]
  (println "note on" (str note ":" velocity))
  (let [new-keyboard (assoc-in keyboard [:keys note] velocity)
        back (on response new-keyboard note velocity)]
    [back new-keyboard]))

(defn note-off-event
  [response keyboard note]
  (println "note off" note)
  (let [new-keyboard (update-in keyboard [:keys] #(dissoc % note))
        back (off response new-keyboard note)]
    [back new-keyboard]))

(defn control-event
  [response keyboard channel level]
  (println "control" channel level)
  (let [new-keyboard (assoc-in keyboard [:controls channel] level)
        back (control response new-keyboard channel level)]
    [back new-keyboard]))

(defn wheel-event
  [response keyboard base detail]
  (println "wheel" base detail)
  (let [new-keyboard (assoc keyboard :wheel {:base base :detail detail})
        back (wheel response new-keyboard base detail)]
    [back new-keyboard]))

(defn trigger
  [keyboard event response]
  (condp = (:command event)
   :note-on (note-on-event response keyboard (:note event) (:velocity event))
   :note-off (note-off-event response keyboard (:note event))
   :control-change (control-event response keyboard (:note event) (:velocity event))
   :pitch-bend (wheel-event response keyboard (:velocity event) (:note event))))