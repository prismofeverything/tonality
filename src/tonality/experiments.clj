(ns tonality.experiments
  (:use overtone.core)
  (:require [tonality.tonality :as tonality]
            [tonality.keyboard :as keyboard]))

(if (server-disconnected?)
  (boot-internal-server))

;; (connect-external-server 2345)

(definst ks-stringer
  [freq 440 rate 6]
  (let [noize (* 0.8 (white-noise))
        trig  (dust rate)
        coef  (mouse-x -0.999 0.999)
        delay (/ 1.0 (* (mouse-y 0.001 0.999) freq))
        plk   (pluck noize trig (/ 1.0 freq) delay 10 coef)
        filt (rlpf plk (* 12 freq) 0.6)]
    (* 0.8 filt)))

(definst dusty
  [freq {:default 60.0 :min 0.0}
   amp  {:default 0.8 :min 0.01 :max 0.99}
   rate {:default 6}
   dur  {:default 2}
   decay {:default 10}
   coef {:default 0.01 :min 0.01 :max 2 :step 0.01}
   gate 1 a 0.1 d 1.0 s 1.0 r 1.0]
  (let [noize (* 0.8 (white-noise))
        trig (dust rate)
        dly (/ 1.0 freq)
        plk (pluck noize trig (/ 1.0 freq) dly
                   decay
                   coef)
        dist (distort plk)
        filt (rlpf dist (* 12 freq) 0.6)
        clp (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (adsr a d s r) gate :action FREE) reverb)))

;; (definst karplus
;;   [freq {:default 60.0 :min 0.0}
;;    amp  {:default 0.8 :min 0.01 :max 0.99}
;;    rate {:default 6}
;;    dur  {:default 2}
;;    decay {:default 30}
;;    coef {:default 0.001}
;;    gate 1 a 0.1 d 1.0 s 1.0 r 1.0]
;;   (let [noize (* 0.8 (white-noise))
;;         trig (* 0.8 (pink-noise))
;;         dly (/ 1.0 freq)
;;         plk (pluck noize 1 (/ 1.0 freq) dly
;;                    decay
;;                    coef)
;;         dist (distort plk)
;;         filt (rlpf dist (* 12 freq) 0.6)
;;         clp (clip2 filt 0.8)
;;         reverb (free-verb clp 0.4 0.8 0.2)]
;;     (* amp (env-gen (adsr a d s r) gate :action FREE) reverb)))

;; (definst orbit
;;   [freq {:default 60.0 :min 0.0}
;;    amp  {:default 0.8 :min 0.01 :max 0.99}
;;    gate 1 a 0.01 d 0.6 s 0.8 r 0.7
;;    divergence 1.1
;;    harmonic-span 50
;;    harmonic-base 5]
;;   (let [series (take 9 (map #(vec [(* % (* freq 0.5)) (/ 1.0 %)]) (iterate #(+ 1.0 %) divergence)))
;;         deathstar
;;         (* 0.03
;;            (env-gen (adsr a d s r) gate :action FREE)
;;            (apply + (map (fn [[f m]] (sin-osc (+ f (* 10 (sin-osc (* 5 (+ 1 (pink-noise)))))) (rlpf (lf-noise2 (+ (* (+ 1 (pink-noise)) harmonic-span) harmonic-base) m) 50 0.4))) series)))]
;;     (* amp deathstar)))

;; (definst hybrid
;;   [freq {:default 60.0 :min 0.0}
;;    amp  {:default 0.8 :min 0.01 :max 0.99}
;;    rate {:default 6}
;;    dur  {:default 2}
;;    decay {:default 30}
;;    coef {:default 0.01}
;;    divergence {:default 1.00}
;;    gate 1 a 1 d 0.6 s 0.8 r 0.7]
;;   (let [noize (* 0.8 (white-noise))
;;         trig (* 0.8 (pink-noise))
;;         unfreq (* 0.5 freq)
;;         dly (/ 1.0 unfreq)
;;         plk (pluck noize 1 (/ 1.0 unfreq) dly
;;                    decay
;;                    coef)
;;         dist (distort plk)
;;         filt (rlpf dist (* 12 unfreq) 0.4)
;;         clp (clip2 filt 0.8)
;;         reverb (free-verb clp 0.4 0.8 0.2)
;;         prc (* reverb (env-gen (perc 0.001 1)))
;;         series (take 9 (map #(vec [(* % unfreq) (/ 1.0 %)]) (iterate #(+ 1.0 %) divergence)))
;;         raw
;; ;        (free-verb
;;          (* 0.03
;;             (env-gen (adsr a d s r) gate :action FREE)
;;             ;;(apply + (map (fn [[f m]] (sin-osc f (rlpf (lf-noise2 (+ (* (rand) 15) 5) m) 20 0.4))) series)))
;;             (apply + (map (fn [[f m]] (sin-osc f (rlpf (pink-noise m) 20 0.4))) series)))
;;                                         ;        0.4 0.4 0.7)
;;         ]
;;     ;;(* amp (+ prc raw))))
;;     (* amp raw)))

(definst flute
  [freq 200 amp 0.8 gate 1 
   a 0.01 d 1.0 s 0.8 r 0.8]
  (* amp 127
     (env-gen (adsr a d s r) gate :action FREE)
     ;;;(stk-bowed freq 1.0 0.5 0.3 0.7 0.5 gate 0.1 3)))
     (stk-flute freq 0.2 0.5 0.3)))

(definst plunk
  [freq 200 vel 0.8 gate 1 a 0.1 d 1.0 s 0.8 r 1.0]
  (* vel 0.005
     (env-gen (adsr a d s r) gate :action FREE)
     ;; (env-gen (perc 0.1 0.3) 1 1 0 gate :action FREE)
     (+ (sin-osc (/ freq 2))
        (rlpf (saw freq) (* 1.1 freq) 0.4)
        (rlpf (sin-osc freq) (* 2.02 freq) 0.7)
        (rlpf (sin-osc freq) (* 3.2 freq) 0.9))))

(definst sawing [freq 200.0 vel 0.8]
  (* vel
     (saw freq)))

(definst sines [freq 200.0 vel 0.8]
  (* vel
     (sin-osc freq)))

(definst mooger
  "Choose 0, 1, or 2 for saw, sin, or pulse"
  [freq {:default 60 :min 0}
   amp  {:default 0.3 :min 0 :max 1}
   osc1 {:default 0 :min 0 :max 2 :step 1}
   osc2 {:default 1 :min 0 :max 2 :step 1}
   osc1-level {:default 0.5 :min 0 :max 1 :step 0.01}
   osc2-level {:default 0 :min 0 :max 1 :step 0.01}
   cutoff {:default 500 :min 0 :max 20000 :step 1}
   attack {:default 0.0001 :min 0.0001 :max 5 :step 0.001}
   decay {:default 0.3 :min 0.0001 :max 5 :step 0.001}
   sustain {:default 0.99 :min 0.0001 :max 1 :step 0.001}
   release {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fattack {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   fdecay {:default 0.3 :min 0.0001 :max 6 :step 0.001}
   fsustain {:default 0.999 :min 0.0001 :max 1 :step 0.001}
   frelease {:default 0.0001 :min 0.0001 :max 6 :step 0.001}
   gate 1]
  (let [osc-bank-1 [(saw freq) (sin-osc freq) (pulse freq)]
        osc-bank-2 [(saw freq) (sin-osc freq) (pulse freq)]
        amp-env (env-gen (adsr attack decay sustain release) gate :action FREE)
        f-env (env-gen (adsr fattack fdecay fsustain frelease) gate)
        s1 (* osc1-level (select osc1 osc-bank-1))
        s2 (* osc2-level (select osc2 osc-bank-2))
        filt (moog-ff (+ s1 s2) (* cutoff f-env) 3)]
    (* amp filt)))

(definst bowed
  [freq 60 amp 0.8 gate 1 
   bow-offset 0 bow-slope 0.5 bow-position 0.75 vib-freq 6.127 vib-gain 0.05]
  (let [beta-ratio   (+ 0.027236 (* 0.2 bow-position))
        base-delay   (reciprocal freq)
        [fb1 fb2]    (local-in 2)
        vibrato      (* (sin-osc vib-freq) vib-gain)
        neck-delay   (+ (* base-delay (- 1 beta-ratio)) (* base-delay vibrato))
        neck         (delay-l fb1 0.05 neck-delay)
        nut-refl     (neg neck)
        bridge       (delay-l fb2 0.025 (* base-delay beta-ratio))
        string-filt  (one-pole (* bridge 0.95) 0.55)
        bridge-refl  (neg string-filt)
        adsr         (* amp (env-gen (adsr 0.02 3.005 1.0 0.01) gate :action FREE))
        string-vel   (+ bridge-refl nut-refl)
        vel-diff     (- adsr string-vel)
        slope        (- 5.0 (* 4 bow-slope))
        bow-table    (clip:ar (pow (abs (+ (* (+ vel-diff bow-offset) slope) 0.75 )) -4) 0 1)
        new-vel       (* vel-diff bow-table)]
   (local-out (+ [bridge-refl nut-refl] new-vel))
   (resonz (* bridge 0.5) 500 0.85)))

;; (definst flute
;;   [freq 440 amp 1.0 gate 1 endreflection 0.5 jetreflection 0.5 
;;    jetratio 0.32 noise-gain 0.15 vibfreq 5.925 vib-gain 0.0 amp 1.0]
;;   (let [nenv           (env-gen (linen 0.2 0.03 0.5 0.5) gate :action FREE)
;;         adsr           (+ (* amp 0.2) (env-gen (adsr 0.005 0.01 1.1 0.01) gate :action FREE))
;;         noise          (* (white-noise) noise-gain)
;;         vibrato        (sin-osc vibfreq 0 vib-gain)
;;         delay          (reciprocal (* freq 0.66666))
;;         lastout        (local-in 1)
;;         breathpressure (* adsr (+ noise, vibrato))
;;         filter         (leak-dc (one-pole (neg lastout) 0.7))
;;         pressurediff   (- breathpressure (* jetreflection filter))
;;         jetdelay       (delay-l pressurediff 0.025 (* delay jetratio))
;;         jet            (clip2 (* jetdelay (- (squared jetdelay) 1.0)) 1.0)
;;         boredelay      (delay-l (+ jet (* endreflection filter) 0.05 delay))]
;;     (local-out boredelay)
;;     (* 0.3 boredelay amp nenv)))

(definst rise-fall-pad
  [freq 440 t 4 amt 0.3 amp 0.8]
  (let [f-env      (env-gen (perc t t) 1 1 0 1 FREE)
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (* amp echo)))

(definst pad
  [freq 60 t 10 amt 0.3 amp 0.1 a 0.4 d 0.5 s 0.8 r 2]
  (let [lfo        (+ 2 (* 0.01 (sin-osc:kr 5 (rand 1.5))))
        src        (apply + (saw [freq (* freq lfo)]))
        env        (env-gen (adsr a d s r) (sin-osc:kr 0.2))
        f-env      (x-line:kr 0.001 4 t)
        src        (* env src)
        signal     (rlpf src (+ (* 0.3 freq) (* f-env 2 freq)) 0.5)
        k          (/ (* 4 amt) (- 1 amt))
        dist       (clip2 (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
                          0.03)
        snd        (* amp dist (line:kr 1 0 t))]
    src))

(definst overpad
  [freq 60 amp 0.7 attack 0.001 release 2]
  (let [env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    audio))

(definst buzz
  [freq 40 cutoff 300 dur 200]
  (let [lpf-lev (* (+ 1 (lf-noise1:kr 10)) 400)
        a (lpf (saw freq) lpf-lev)
        b (sin-osc (* 0.5 freq))
        env (env-gen 1 1 0 1 2 (perc 0.01 (/ dur 1000)))]
    (* env (+ a b))))

(definst b3
  [freq 60 amp 0.8 gate 1.0
   a 0.01 d 3 s 1 r 0.01
   ]
  (let [waves (sin-osc [(* 0.5 freq)
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
        env (env-gen (adsr a d s r) gate :action FREE)]
    (* amp env snd 0.1)))


(definst j8
  [freq 60 amp 0.8 gate 1.0
   a 0.4 d 0.5 s 0.8 r 2
   amt 0.3 t 10]
  (let [lfo        (+ 2 (* 0.01 (sin-osc:kr 5 (rand 1.5))))
        src        (apply + (saw [freq (* freq lfo)]))
        env        (env-gen (adsr a d s r) gate :action FREE)
        f-env      (x-line:kr 0.001 4 t)
        src        (* env src)
        signal     (rlpf src (+ (* 0.3 freq) (* f-env 2 freq)) 0.5)
        k          (/ (* 4 amt) (- 1 amt))
        dist       (clip2 (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
                          0.03)
        snd        (* amp dist (line:kr 1 0 t))]
    snd))

;; (definst x3
;;   [freq 60 amp 0.1 gate 1.0 overtones 10 a 0.01 d 3 s 1 r 0.01]
;;   (let [oscs (map #(list [(* freq %) (/ 0.5 %)]) (iterate inc 1))
;;         waves (sin-osc oscs)
;;         snd (apply + waves)
;;         env (env-gen (adsr a d s r) gate :action FREE)]
;;     (* amp env snd 0.1)))

;; (definst ooo
;;   [freq 60 amp 0.1 gate 1.0]
;;   (let []
;;     (* amp env snd)))



(def thirteen
  (tonality/tonality (tonality/otonality 16) 200.0 48))

(def fifty-one
  (tonality/tonality (tonality/equal-temperament 51) 200.0 48))

;; (def keyboard (atom (keyboard/blank-keyboard)))
(def instrument b3)
(def tones fifty-one)
(def tones
  (tonality/tonality tonality/pure-twelve 440.0 72))
(def tones tonality/nineteen)

(defn play-tone
  [tonality instrument note velocity]
  (let [tone (tonality note)
        amp (* velocity keyboard/midi-normal)
        synth (instrument :freq tone :amp amp :vel velocity)]
    synth))

