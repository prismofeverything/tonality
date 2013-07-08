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
  [amp {:default 0.8 :min 0.01 :max 0.99}
   width {:default 0.02 :min 0.01 :max 1}]
  (let [base (* 0.8 (o/white-noise))
        b-filter (o/bpf base (o/mouse-y 40 5000 o/EXP) width)
        a-filter (o/bpf base (o/mouse-x 40 5000 o/EXP) width)]
    (* amp (+ a-filter b-filter))))

(o/definst orange
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

