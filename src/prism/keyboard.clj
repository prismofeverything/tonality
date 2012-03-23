(ns prism.keyboard
  (:use overtone.core))

(defn blank-keyboard
  []
  {:keys {} :controls {} :wheel {:high 64 :low 0}})

(defn key-event
  [keyboard instrument tonality note vel]
  (println (str note ":" vel))
  (println (str keyboard))
  (if (zero? vel)
    (do
      (ctl ((keyboard :keys) note) :gate 0)
      (update-in keyboard [:keys] #(dissoc % note)))
    (let [tone (tonality note)
          amp (/ vel 127.0)
          synth (instrument :freq tone :amp amp :vel vel)]
      (println synth)
      (assoc-in keyboard [:keys note] synth))))

(defn control-event
  [keyboard instrument channel level]
  (assoc-in keyboard [:controls channel] level))

(defn wheel-event
  [keyboard instrument high low]
  (assoc keyboard :wheel {:high high :low low}))

(defn keyboard-event
  [keyboard instrument tonality event]
  (cond
   (= (event :cmd) 144) (key-event keyboard instrument tonality (event :note) (event :vel))
   (= (event :cmd) 176) (control-event keyboard instrument (event :note) (event :vel))
   (= (event :cmd) 224) (wheel-event keyboard instrument (event :vel) (event :note))))
