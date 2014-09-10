(ns tonality.sound
  (:require [overtone.core :as o]
            [tonality.tonality :as tonality]))

(def ^:dynamic *external-server* false)

(if (o/server-disconnected?)
  (if *external-server*
    (o/connect-external-server 2345)
    (o/boot-internal-server)))

(defn play-tone
  [tonality instrument note velocity]
  (let [tone (tonality note)
        amp (* velocity (/ 1 127))
        synth (instrument :freq tone :amp amp :vel velocity)]
    synth))

(def dull-partials
  [0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])

(def partials
  [0.5
   1
   3
   4.2
   5.4
   6.8])

(o/defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (o/env-gen (o/perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (o/sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))


(o/definst dull-bell [freq 220 dur 1.0 amp 1.0]
  (let [snd (* amp (bell-partials freq dur dull-partials))]
    (o/detect-silence snd :action o/FREE)
    snd))

(o/definst pretty-bell [freq 220 dur 1.0 amp 1.0]
  (let [snd (* amp (bell-partials freq dur partials))]
    (o/detect-silence snd :action o/FREE)
    snd))

(o/definst b3
  [freq 60 amp 0.8 gate 1.0
   a 0.01 d 3 s 1 r 0.01]
  (let [waves (o/sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 (/ 7 4))
                        (* freq 2 2 2)])
        snd (apply + waves)
        env (o/env-gen (o/adsr a d s r) gate :action o/FREE)]
    (* amp env snd 0.1)))

(o/definst j8
  [freq 60 amp 0.8 gate 1.0
   a 0.4 d 0.5 s 0.8 r 2
   amt 0.3 t 10]
  (let [lfo        (+ 2 (* 0.01 (o/sin-osc:kr 5 (rand 1.5))))
        src        (apply + (o/saw [freq (* freq lfo)]))
        env        (o/env-gen (o/adsr a d s r) gate :action o/FREE)
        f-env      (o/x-line:kr 0.001 4 t)
        src        (* env src)
        signal     (o/rlpf src (+ (* 0.3 freq) (* f-env 2 freq)) 0.5)
        k          (/ (* 4 amt) (- 1 amt))
        dist       (o/clip2 (/ (* (+ 1 k) signal) (+ 1 (* k (o/abs signal)))) 0.03)
        snd        (* amp dist (o/line:kr 1 0 t))]
    snd))

(o/definst yellow
  "additive band-pass filters of varying widths"
  [amp {:default 0.8 :min 0.01 :max 0.99}
   width {:default 0.02 :min 0.01 :max 1}]
  (let [base (* 0.8 (o/white-noise))
        b-filter (o/bpf base (o/mouse-y 40 5000 o/EXP) width)
        a-filter (o/bpf base (o/mouse-x 40 5000 o/EXP) width)]
    (* amp (+ a-filter b-filter))))

(o/definst orange
  "using layered square waves to modulate frequency"
  [amp       {:default 0.8 :min 0.01 :max 0.99}
   step-base {:default 200 :min 40 :max 5000}
   c-step-freq {:default 0.8 :min 0.001 :max 20}
   c-step-interval {:default 1.125 :min 0.5 :max 2}
   b-step-freq {:default 0.5 :min 0.001 :max 20}
   b-step-interval {:default 1.25 :min 0.5 :max 2}
   a-step-freq {:default 0.3 :min 0.001 :max 20}
   a-step-interval {:default 1.5 :min 0.5 :max 2}]
  (let [a-step (+ 1 (* (- a-step-interval 1) (o/lf-pulse a-step-freq 0 0.5)))
        b-step (+ 1 (* (- b-step-interval 1) (o/lf-pulse b-step-freq 0 0.5)))
        c-step (+ 1 (* (- c-step-interval 1) (o/lf-pulse c-step-freq 0 0.5)))
        scaled (* step-base a-step b-step c-step)
        base (o/sin-osc :freq scaled)]
    (* amp base)))

(o/definst pink
  "basic pulse width modulation"
  [amp {:default 0.8 :min 0.01 :max 0.99}]
   ;; width {:default 0.5 :min 0.01 :max 0.99}
   ;; freq {:default 200 :min 20 :max 50000}]
  (let [mod (* 0.4 (+ 1.1 (o/sin-osc (o/mouse-y 0.01 200 o/EXP))))
        pulse (o/pulse (o/mouse-x 40 5000 o/EXP) mod)]
    (* amp pulse)))

(o/definst green
  "amplitude modulated saw tooth wave"
  [amp {:default 0.8 :min 0.01 :max 0.99}]
  (let [modulator (* 0.5 (+ 1 (o/sin-osc (o/mouse-y 1 5000 o/EXP))))
        carrier (o/saw (o/mouse-x 40 5000 o/EXP))]
    (* amp modulator carrier)))

(o/definst red
  "frequency modulated sin wave"
  [amp {:default 0.8 :min 0.01 :max 0.99}
   freq {:default 400 :min 40 :max 5000}]
  (let [modulator (o/sin-osc (o/mouse-y 1 5000 o/EXP))
        carrier (o/sin-osc (+ freq (* (o/mouse-x 1 5000 o/EXP) modulator)))]
    (* amp carrier)))

(o/definst chameleon
  [amp {:default 0.8 :min 0.01 :max 0.99}]
  (let [saws (o/mix (o/saw [80 (o/mouse-y 50 2000 o/EXP) 101 100.5]))
        wobble (o/lin-lin (o/lf-tri (o/mouse-x 0.1 20 o/EXP)) -1 1 400 4000)]
    (* amp (o/lpf saws wobble))))

(o/definst formantax
  [amp {:default 0.8 :min 0.01 :max 0.99}
   formant {:default 400 :min 20 :max 50000}
   freq {:default 30 :min 1 :max 1000}
   window {:default 0.05 :min 0 :max 1.0}
   mass {:default 0.03 :min 0 :max 1.0}
   gate 1.0]
  (let [env (o/env-gen (o/adsr 0.1 1 1 0.1) gate :action o/FREE)
        threshhold (* formant window)
        smear (* threshhold (o/brown-noise))
        trigger (o/impulse:ar (+ formant smear))
        grains (o/grain-sin:ar 1 trigger mass freq 0 -1 512)]
    (* amp env grains)))

